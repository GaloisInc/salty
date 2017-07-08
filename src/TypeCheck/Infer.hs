{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module TypeCheck.Infer where

import           Scope.Check (Renamed)
import           Scope.Name (Name)
import           SrcLoc
import qualified Syntax.AST as AST
import           TypeCheck.AST
import           TypeCheck.Groups
import           TypeCheck.Monad

import           Control.Monad (when,unless,zipWithM_,forM)
import           Data.Either (partitionEithers)
import           Data.List (foldl')
import qualified Data.Set as Set


inferController :: AST.Controller Renamed -> TC Controller
inferController AST.Controller { AST.cName, AST.cDecls } =
  do updates <- traverse inferTopGroup (sccTopDecls cDecls)
     return $! foldr (id) (emptyController cName) updates


inferTopGroup :: Group (AST.TopDecl Renamed) -> TC (Controller -> Controller)

inferTopGroup (NonRecursive td) = simpleTopDecl td

-- XXX: for the future, it might be useful to allow recursive functions.
-- Currently, we have no measure to use for the recursion to terminate, so the
-- only recursive functions we could write would be non-terminating.
inferTopGroup (Recursive tds) = failWith (invalidRecursiveGroup tds)


-- | Check non-recursive declarations.
simpleTopDecl :: AST.TopDecl Renamed -> TC (Controller -> Controller)

simpleTopDecl (AST.TDEnum _ enum) = checkEnum enum

simpleTopDecl (AST.TDFun _ fun) =
     inferFun fun

simpleTopDecl (AST.TDInput _ sv) =
  do (sv', mbInit) <- checkStateVar sv
     let s' = case mbInit of
                Nothing -> mempty
                Just e' -> mempty { sEnvInit = [e'] }
     return $ \ c -> c { cInputs = sv' : cInputs c
                       , cSpec    = s' `mappend` cSpec c }

simpleTopDecl (AST.TDOutput _ sv) =
  do (sv', mbInit) <- checkStateVar sv
     let s' = case mbInit of
                Nothing -> mempty
                Just e' -> mempty { sSysInit = [e'] }
     return $ \ c -> c { cOutputs = sv' : cOutputs c
                       , cSpec    = s' `mappend` cSpec c }

simpleTopDecl (AST.TDSpec _ s) =
  do s' <- zonk =<< checkSpec s
     return $ \ c -> c { cSpec = s' `mappend` cSpec c }

simpleTopDecl (AST.TDExpr _ e) =
  do e'  <- zonk =<< checkExpr TSpec e
     loc <- askLoc
     return $ \ c -> c { cTopExprs = (loc,e') : cTopExprs c }


-- | Check a specification value.
checkSpec :: AST.Spec Renamed -> TC Spec

checkSpec (AST.SSysTrans loc es) = addLoc loc $
  do es' <- checkSpecEntry es
     return $ mempty { sSysTrans = es' }

checkSpec (AST.SSysLiveness loc es) = addLoc loc $
  do l <- checkLiveness es
     return $ mempty { sSysLiveness = [l] }

checkSpec (AST.SEnvTrans loc es) = addLoc loc $
  do es' <- checkSpecEntry es
     return $ mempty { sEnvTrans = es' }

checkSpec (AST.SEnvLiveness loc es) = addLoc loc $
  do l <- checkLiveness es
     return $ mempty { sEnvLiveness = [l] }

checkSpec (AST.SSysInit loc es) = addLoc loc $
  do es' <- checkSpecEntry es
     return $ mempty { sSysInit = es' }

checkSpec (AST.SEnvInit loc es) = addLoc loc $
  do es' <- checkSpecEntry es
     return $ mempty { sEnvInit = es' }


checkSpecEntry :: [AST.Expr Renamed] -> TC [(SrcLoc,Expr)]
checkSpecEntry  = traverse $ \e ->
  do e' <- checkExpr TBool e
     return (srcLoc e, e')

checkLiveness :: [AST.Expr Renamed] -> TC Liveness
checkLiveness es =
  do es' <- forM es $ \e ->
              do e' <- checkExpr TBool e
                 return (srcLoc e, e')

     return (Liveness es')


-- | Add all of the constructors to the typing environment.
checkEnum :: AST.EnumDef Renamed -> TC (Controller -> Controller)
checkEnum AST.EnumDef { eAnn, eName, eCons } =
  do let ty   = Forall [] (TEnum eName)
         cons = [ con | (con, _) <- eCons ]

     addTypes (zip cons (repeat ty))

     return $ \ c -> c { cEnums = EnumDef { eAnn  = eAnn
                                          , eName = eName
                                          , eCons = cons
                                          } : cEnums c }


checkStateVar :: AST.StateVar Renamed -> TC (StateVar, Maybe (SrcLoc,Expr))
checkStateVar AST.StateVar { svAnn, svName, svType, svInit, svBounds } =
  do ty' <- translateType svType
     e'  <- zonk =<< traverse (checkExpr ty') svInit

     let bounds = case svBounds of
                    Just AST.Bounds { bLow, bHigh } -> Just (bLow, bHigh)
                    Nothing                         -> Nothing

     -- TODO: add an assertion that the type doesn't contain variables [tc]
     addTypes [(svName, Forall [] ty')]

     -- when the type of the state var is Int, make sure that it has bounds
     -- defined.
     when (ty' == TInt && bounds == Nothing)
          (record (MissingBounds svName))

     let var = StateVar { svAnn    = svAnn
                        , svName   = svName
                        , svType   = ty'
                        , svBounds = bounds }

         mbInit = do val <- e'
                     return (srcLoc svInit, EEq ty' (EVar ty' svName) val)

     return (var, mbInit)


-- | Check a single function.
--
-- NOTE: we don't currently allow recursive functions, as there's no way to
-- terminate recursion in the language.
inferFun :: AST.Fun Renamed -> TC (Controller -> Controller)
inferFun locFun = withLoc locFun $ \ AST.Fun { fName } ->
  do ty  <- guessType locFun
     fb' <- checkFun ty locFun

     addTypes [(fName, fSchema fb')]

     return $ \ c -> c { cFuns = NonRecursive fb' : cFuns c }


-- | NOTE: type variables can still be present after zonking at this stage,
-- which just indicates that the variables are unused. It might be good to
-- propagate this information down, so that the function can ultimately be
-- re-written to not include that parameter.
checkFun :: Type -> AST.Fun Renamed -> TC Fun
checkFun ty locFun = withLoc locFun $ \ AST.Fun { fName, fParams, fBody, fAnn } ->
  withParams ty fParams $ \ (ps,rty) ->
    do e       <- checkFunBody rty fBody
       fSchema <- generalize (tFun (ps ++ [rty]))
       fBody'  <- zonk e
       return Fun { fName   = fName
                  , fAnn    = fAnn
                  , fParams = fParams
                  , fBody   = fBody'
                  , .. }


-- | When generalizing macros, we don't need to consider type variables from the
-- environment, as local functions are not allowed. As a result, any free
-- variables present after zonking should be generalized.
generalize :: Type -> TC Schema
generalize ty =
  do ty' <- zonk ty
     ps' <- ftvs ty'
     let ps = Set.toList ps'
     let gs = [ tv { tvUnique = ix } | (tv, ix) <- zip ps [0 .. ] ]

     -- bind all variables to be generalized
     zipWithM_ unify (map TFree ps) (map TGen gs)

     ty'' <- zonk ty'

     return (Forall gs ty'')


checkFunBody :: Type -> AST.FunBody Renamed -> TC FunBody

checkFunBody ty (AST.FBSpec loc ps) = addLoc loc $
  do unify ty TSpec
     ps' <- zonk =<< traverse checkSpec ps
     return (FunSpec (mconcat ps'))

checkFunBody ty (AST.FBExpr loc e) = addLoc loc $
  do e' <- checkExpr ty e
     return (FunExpr e')


-- | Type-check an expression, producing a type-checked expression as the
-- result.
checkExpr :: Type -> AST.Expr Renamed -> TC Expr

checkExpr ty (AST.EVar loc n) = addLoc loc $
  do (ty',tyApp) <- freshInst =<< lookupVar n
     unify ty ty'
     return (tyApp (EVar ty' n))

checkExpr ty (AST.ECon loc n) = addLoc loc $
  do ty' <- monoInst =<< lookupVar n
     unify ty ty'
     return (ECon ty' n)

checkExpr ty (AST.ENum loc i) = addLoc loc $
  do unify ty TInt
     return (ENum i)

checkExpr ty (AST.ETrue loc)= addLoc loc $
  do unify ty TBool
     return ETrue

checkExpr ty (AST.EFalse loc) = addLoc loc $
  do unify ty TBool
     return EFalse

checkExpr ty (AST.EAnd loc l r) = addLoc loc $
  do unify ty TBool
     l' <- checkExpr TBool l
     r' <- checkExpr TBool r
     return (EAnd l' r')

checkExpr ty (AST.EOr loc l r) = addLoc loc $
  do unify ty TBool
     l' <- checkExpr TBool l
     r' <- checkExpr TBool r
     return (EOr l' r')

checkExpr ty (AST.EXor loc l r) = addLoc loc $
  do unify ty TBool
     l' <- checkExpr TBool l
     r' <- checkExpr TBool r
     return (EXor l' r')

checkExpr ty (AST.ENot loc e) = addLoc loc $
  do unify ty TBool
     e' <- checkExpr TBool e
     return (ENot e')

checkExpr ty (AST.EIf loc p t f) = addLoc loc $
  do p' <- checkExpr TBool p
     t' <- checkExpr ty t
     f' <- checkExpr ty f
     return (eIf p' t' f')

checkExpr ty (AST.EApp loc f x) = addLoc loc $
  do xtv <- newTVar Nothing
     let xty = TFree xtv
     f'  <- checkExpr (TFun xty ty) f
     x'  <- checkExpr xty x
     return (EApp f' x')

checkExpr ty (AST.EEq loc a b) = addLoc loc $
  do atv <- newTVar Nothing
     let aty = TFree atv
     a' <- checkExpr aty a
     b' <- checkExpr aty b
     unify ty TBool
     return (EEq aty a' b')

checkExpr ty (AST.ENeq loc a b) = addLoc loc $
  do atv <- newTVar Nothing
     let aty = TFree atv
     a' <- checkExpr aty a
     b' <- checkExpr aty b
     unify ty TBool
     return (ENot (EEq aty a' b'))

checkExpr ty (AST.EImp loc a b) = addLoc loc $
  do a' <- checkExpr TBool a
     b' <- checkExpr TBool b
     unify ty TBool
     return (eImp a' b')

checkExpr ty (AST.EIff loc a b) = addLoc loc $
  do a' <- checkExpr TBool a
     b' <- checkExpr TBool b
     unify ty TBool
     return (eAnd [ eImp a' b', eImp b' a' ])

checkExpr ty (AST.ECase loc e cs) = addLoc loc $
  do etv <- newTVar Nothing
     let ety = TFree etv
     e' <- checkExpr ety e
     checkCases ety e' ty cs

checkExpr ty (AST.ESet loc es) = addLoc loc $
  do etv <- newTVar Nothing
     let ety = TFree etv
     unify ty (TSet ety)

     es' <- traverse (checkExpr ety) es

     return (ESet ety es')

checkExpr ty (AST.EIn loc e set) = addLoc loc $
  do etv  <- newTVar Nothing
     let ety = TFree etv
     e'   <- checkExpr ety e
     set' <- checkExpr (TSet ety) set

     unify ty TBool

     return (EIn ety e' set')

checkExpr ty (AST.EAny loc) = addLoc loc $
  do unify ty (TFun (TSet TBool) TBool)
     return (EPrim PAny)

checkExpr ty (AST.EAll loc) = addLoc loc $
  do unify ty (TFun (TSet TBool) TBool)
     return (EPrim PAll)

checkExpr ty (AST.EMutex loc) = addLoc loc $
  do unify ty (TFun (TSet TBool) TBool)
     return (EPrim PMutex)

checkExpr ty (AST.ENext loc e) = addLoc loc $
  do e' <- checkExpr ty e
     return (ENext ty e')


-- | Turn a case expression into one big disjunction of implications.
--
-- INVARIANT: this expects that there is at most one default present.
checkCases :: Type -> Expr -> Type -> [AST.Case Renamed] -> TC Expr
checkCases ety e ty cases =
  do pats' <- traverse mkImp pats
     let ps = map fst pats'

     let neg = eAnd (map eNot ps)
     defs' <- traverse (mkDefault neg) defs

     when (length defs' > 1)
          (record (tooManyDefaultCases [ c | (_,c,_) <- defs ]))

     let imps = [ eImp p c | (p,c) <- pats' ]
     return (eAnd (defs' ++ imps))

  where

  (defs,pats) = partitionEithers (map isDefault cases)

  isDefault c = go c
    where
    go (AST.CPat loc p rhs)   = Right (loc,p,rhs)
    go (AST.CDefault loc rhs) = Left (loc,c,rhs)

  mkImp (loc,pat,rhs) = addLoc loc $
    do pat' <- checkPat ety e pat
       rhs' <- checkExpr ty rhs
       return (pat',rhs')

  -- the condition for the default case to fire is when all of the implication
  -- conditions are false
  mkDefault cond (loc,_,body) = addLoc loc $
    do body' <- checkExpr ty body
       return (eImp cond body')


-- | Guard that a pattern matches the type given, and rewrite to a core
-- representation that implements the match.
checkPat :: Type -> Expr -> AST.Pat Renamed -> TC Expr

checkPat ty e (AST.PCon loc n) = addLoc loc $
  do nty <- monoInst =<< lookupVar n
     unify ty nty
     return (EEq ty e (ECon nty n))

-- XXX make sure to check that the number fits in the bounds expected
checkPat ty e (AST.PNum loc n) = addLoc loc $
  do return (EEq ty e (ENum n))


-- | Locally introduce types for function parameters, and run the body.
withParams :: Type -> [Name] -> (([Type],Type) -> TC a) -> TC a
withParams ty params body =
  -- it should be impossible for the number of function arguments and number
  -- of parameters to be different, as the user never has control over the type
  -- signature
  let tyParts@(args,_) = destTFun ty
   in withTypes (zip params args) (body tyParts)

-- | Produce a type that follows the shape of the function definition.
guessType :: AST.Fun Renamed -> TC Type
guessType loc = withLoc loc $ \ AST.Fun { fName, fParams } ->
  do vars <- traverse (`withLoc` (newTVar . Just)) fParams
     res  <- withLoc fName (newTVar . Just)
     return $! mkFun vars res
  where
  mkFun vars res = foldl' (\acc t -> TFree t `TFun` acc) (TFree res) vars


-- | Translate a parsed type into a core type.
translateType :: AST.Type Renamed -> TC Type
translateType (AST.TBool _)      = return TBool
translateType (AST.TInt _)       = return TInt
translateType (AST.TEnum _ name) = return (TEnum name)
translateType (AST.TFun loc l r) = addLoc loc $
  do l' <- translateType l
     r' <- translateType r
     return (TFun l' r')


-- Instantiation ---------------------------------------------------------------

monoInst :: Schema -> TC Type
monoInst (Forall ps ty) =
  do unless (null ps) (fail "Non-monorphic schema")
     return ty

-- | Instantiate a type schema with fresh variables.
freshInst :: Schema -> TC (Type, Expr -> Expr)
freshInst s@(Forall ps _) =
  do vs <- traverse (newTVar . tvOrigin) ps
     instantiate s (map TFree vs)

-- | Instantiate a schema, producing a type and a function that will add type
-- applications to the expression given.
instantiate :: Schema -> [Type] -> TC (Type, Expr -> Expr)
instantiate (Forall ps ty) ts
  | length ps /= length ts = fail "Invalid instantiation"
  | otherwise              = return (typeInst ts ty, tyApp)
  where
  tyApp e = foldl ETApp (typeInst ts e) ts
