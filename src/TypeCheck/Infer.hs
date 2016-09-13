{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module TypeCheck.Infer where

import           Scope.Name (Name)
import qualified Syntax.AST as AST
import           TypeCheck.AST
import           TypeCheck.Groups
import           TypeCheck.Monad

import           Control.Monad (when,unless,zipWithM_)
import           Data.Either (partitionEithers)
import           Data.List (foldl')
import qualified Data.Set as Set
import           Text.Location (thing,getLoc,at)


inferController :: AST.Controller Name -> TC Controller
inferController AST.Controller { AST.cName, AST.cDecls } =
  do updates <- traverse inferTopGroup (sccTopDecls cDecls)
     return $! foldr (id) (emptyController (thing cName)) updates


inferTopGroup :: Group (AST.TopDecl Name) -> TC (Controller -> Controller)

inferTopGroup (NonRecursive td) = simpleTopDecl td

-- XXX: for the future, it might be useful to allow recursive functions.
-- Currently, we have no measure to use for the recursion to terminate, so the
-- only recursive functions we could write would be non-terminating.
inferTopGroup (Recursive tds) = failWith (invalidRecursiveGroup tds)


mkLocFun :: AST.TopDecl Name -> Maybe (AST.Loc (AST.Fun Name))
mkLocFun  = go mempty
  where
  go _   (AST.TDLoc loc) = go (getLoc loc) (thing loc)
  go src (AST.TDFun fun) = Just (fun `at` src)
  go _   _               = Nothing


-- | Check non-recursive declarations.
simpleTopDecl :: AST.TopDecl Name -> TC (Controller -> Controller)

simpleTopDecl (AST.TDEnum enum) = checkEnum enum

simpleTopDecl (AST.TDFun fun) =
  do loc <- askLoc
     inferFun (fun `at` loc)

simpleTopDecl (AST.TDInput sv) =
  do sv' <- checkStateVar sv
     return $ \ c -> c { cInputs = sv' : cInputs c }

simpleTopDecl (AST.TDOutput sv) =
  do sv' <- checkStateVar sv
     return $ \ c -> c { cOutputs = sv' : cOutputs c }

simpleTopDecl (AST.TDSpec s) =
  do s' <- zonk =<< checkSpec s
     return $ \ c -> c { cSpec = cSpec c `mappend` s' }

simpleTopDecl (AST.TDExpr e) =
  do e' <- zonk =<< checkExpr TSpec e
     return $ \ c -> c { cTopExprs = e' : cTopExprs c }

simpleTopDecl (AST.TDLoc loc) = withLoc loc simpleTopDecl

-- | Check a specification value.
checkSpec :: AST.Spec Name -> TC Spec

checkSpec (AST.SSysTrans es) =
  do e' <- checkExpr TBool (foldl AST.EAnd AST.ETrue es)
     return $ mempty { sSysTrans = e' }

checkSpec (AST.SSysLiveness es) =
  do e' <- checkExpr TBool (foldl AST.EAnd AST.ETrue es)
     return $ mempty { sSysLiveness = e' }

checkSpec (AST.SEnvTrans es) =
  do e' <- checkExpr TBool (foldl AST.EAnd AST.ETrue es)
     return $ mempty { sEnvTrans = e' }

checkSpec (AST.SEnvLiveness es) =
  do e' <- checkExpr TBool (foldl AST.EAnd AST.ETrue es)
     return $ mempty { sEnvLiveness = e' }

checkSpec (AST.SLoc loc) = withLoc loc checkSpec


-- | Add all of the constructors to the typing environment.
checkEnum :: AST.EnumDef Name -> TC (Controller -> Controller)
checkEnum AST.EnumDef { eName, eCons } =
  do let ty   = Forall [] (TEnum (thing eName))
         cons = map thing eCons

     addTypes (zip cons (repeat ty))

     return $ \ c -> c { cEnums = EnumDef { eName = thing eName
                                          , eCons = cons
                                          } : cEnums c }


checkStateVar :: AST.StateVar Name -> TC StateVar
checkStateVar AST.StateVar { svName, svType, svInit, svBounds } =
  do ty' <- translateType svType
     e'  <- zonk =<< traverse (checkExpr ty') svInit

     bounds <- case svBounds of
                 Just loc -> withLoc loc (return . Just)
                 Nothing  -> return Nothing

     -- TODO: add an assertion that the type doesn't contain variables [tc]
     addTypes [(thing svName, Forall [] ty')]

     -- when the type of the state var is Int, make sure that it has bounds
     -- defined.
     when (ty' == TInt && bounds == Nothing)
          (record (MissingBounds svName))

     return StateVar { svName   = thing svName
                     , svType   = ty'
                     , svBounds = bounds
                     , svInit   = e' }


-- | Check a single function.
--
-- NOTE: we don't currently allow recursive functions, as there's no way to
-- terminate recursion in the language.
inferFun :: AST.Loc (AST.Fun Name) -> TC (Controller -> Controller)
inferFun locFun = withLoc locFun $ \ AST.Fun { fName } ->
  do ty  <- guessType locFun
     fb' <- checkFun ty locFun

     addTypes [(thing fName, fSchema fb')]

     return $ \ c -> c { cFuns = NonRecursive fb' : cFuns c }


-- | NOTE: type variables can still be present after zonking at this stage,
-- which just indicates that the variables are unused. It might be good to
-- propagate this information down, so that the function can ultimately be
-- re-written to not include that parameter.
checkFun :: Type -> AST.Loc (AST.Fun Name) -> TC Fun
checkFun ty locFun = withLoc locFun $ \ AST.Fun { fName, fParams, fBody } ->
  withParams ty fParams $ \ (ps,rty) ->
    do e       <- checkFunBody rty fBody
       fSchema <- generalize (tFun (ps ++ [rty]))
       fBody   <- zonk e
       return Fun { fName   = thing fName
                  , fParams = map thing fParams
                  , .. }


-- | When generalizing macros, we don't need to consider type variables from the
-- environment, as local functions are not allowed. As a result, any free
-- variables present after zonking should be generalized.
generalize :: Type -> TC Schema
generalize ty =
  do ty' <- zonk ty
     let ps = Set.toList (ftvs ty')
         gs = [ tv { tvUnique = ix } | (tv, ix) <- zip ps [0 .. ] ]

     -- bind all variables to be generalized
     zipWithM_ unify (map TFree ps) (map TGen gs)

     ty'' <- zonk ty'

     return (Forall gs ty'')


checkFunBody :: Type -> AST.FunBody Name -> TC FunBody

checkFunBody ty (AST.FBSpec ps) =
  do unify ty TSpec
     ps' <- zonk =<< traverse checkSpec ps
     return (FunSpec (mconcat ps'))

checkFunBody ty (AST.FBExpr e) =
  do e' <- checkExpr ty e
     return (FunExpr e')


-- | Type-check an expression, producing a type-checked expression as the
-- result.
checkExpr :: Type -> AST.Expr Name -> TC Expr

checkExpr ty (AST.EVar n) =
  do (ty',tyApp) <- freshInst =<< lookupVar n
     unify ty ty'
     return (tyApp (EVar ty' n))

checkExpr ty (AST.ECon n) =
  do ty' <- monoInst =<< lookupVar n
     unify ty ty'
     return (ECon ty' n)

checkExpr ty (AST.ENum i) =
  do unify ty TInt
     return (ENum i)

checkExpr ty AST.ETrue =
  do unify ty TBool
     return ETrue

checkExpr ty AST.EFalse =
  do unify ty TBool
     return EFalse

checkExpr ty (AST.EAnd l r) =
  do unify ty TBool
     l' <- checkExpr TBool l
     r' <- checkExpr TBool r
     return (EAnd l' r')

checkExpr ty (AST.EOr  l r) =
  do unify ty TBool
     l' <- checkExpr TBool l
     r' <- checkExpr TBool r
     return (EOr l' r')

checkExpr ty (AST.ENot e) =
  do unify ty TBool
     e' <- checkExpr TBool e
     return (ENot e')

checkExpr ty (AST.EIf p t f) =
  do p' <- checkExpr TBool p
     t' <- checkExpr ty t
     f' <- checkExpr ty f
     return (eIf p' t' f')

checkExpr ty (AST.EApp f x) =
  do xtv <- newTVar Nothing
     let xty = TFree xtv
     f'  <- checkExpr (TFun xty ty) f
     x'  <- checkExpr xty x
     return (EApp f' x')

checkExpr ty (AST.EEq a b) =
  do atv <- newTVar Nothing
     let aty = TFree atv
     a' <- checkExpr aty a
     b' <- checkExpr aty b
     unify ty TBool
     return (EEq aty a' b')

checkExpr ty (AST.ENeq a b) =
  do atv <- newTVar Nothing
     let aty = TFree atv
     a' <- checkExpr aty a
     b' <- checkExpr aty b
     unify ty TBool
     return (ENot (EEq aty a' b'))

checkExpr ty (AST.EImp a b) =
  do a' <- checkExpr TBool a
     b' <- checkExpr TBool b
     unify ty TBool
     return (eImp a' b')

checkExpr ty (AST.EIff a b) =
  do a' <- checkExpr TBool a
     b' <- checkExpr TBool b
     unify ty TBool
     return (eAnd [ eImp a' b', eImp (eNot a') (eNot b') ])

checkExpr ty (AST.ECase e cs) =
  do etv <- newTVar Nothing
     let ety = TFree etv
     e' <- checkExpr ety e
     checkCases ety e' ty cs

checkExpr ty (AST.ESet es) =
  do etv <- newTVar Nothing
     let ety = TFree etv
     unify ty (TSet ety)

     es' <- traverse (checkExpr ety) es

     return (ESet ety es')

checkExpr ty (AST.EIn e set) =
  do etv  <- newTVar Nothing
     let ety = TFree etv
     e'   <- checkExpr ety e
     set' <- checkExpr (TSet ety) set

     unify ty TBool

     return (EIn ety e' set')

checkExpr ty AST.EAny =
  do unify ty (TFun (TSet TBool) TBool)
     return (EPrim PAny)

checkExpr ty AST.EAll =
  do unify ty (TFun (TSet TBool) TBool)
     return (EPrim PAll)

checkExpr ty AST.EMutex =
  do unify ty (TFun (TSet TBool) TBool)
     return (EPrim PMutex)

checkExpr ty (AST.ENext e) =
  do e' <- checkExpr ty e
     return (ENext ty e')

checkExpr ty (AST.ELoc loc) = withLoc loc (checkExpr ty)


-- | Turn a case expression into one big disjunction of implications.
--
-- INVARIANT: this expects that there is at most one default present.
checkCases :: Type -> Expr -> Type -> [AST.Case Name] -> TC Expr
checkCases ety e ty cases =
  do pats' <- traverse mkImp pats
     let ps = map fst pats'

     let neg = eAnd (map eNot ps)
     defs' <- traverse (mkDefault neg) defs

     when (length defs' > 1)
          (record (tooManyDefaultCases (map fst defs)))

     let imps = [ eImp p c | (p,c) <- pats' ]
     return (eAnd (defs' ++ imps))

  where

  (defs,pats) = partitionEithers (map isDefault cases)

  isDefault c = go mempty c
    where
    go loc (AST.CPat p rhs)   = Right ((p,rhs) `at` loc)
    go loc (AST.CDefault rhs) = Left (c, rhs `at` loc)
    go _   (AST.CLoc loc)     = go (getLoc loc) (thing loc)

  mkImp loc = withLoc loc $ \ (pat,rhs) ->
    do pat' <- checkPat ety e pat
       rhs' <- checkExpr ty rhs
       return (pat',rhs')

  -- the condition for the default case to fire is when all of the implication
  -- conditions are false
  mkDefault cond (_,loc) = withLoc loc $ \ body ->
    do body' <- checkExpr ty body
       return (eImp cond body')


-- | Guard that a pattern matches the type given, and rewrite to a core
-- representation that implements the match.
checkPat :: Type -> Expr -> AST.Pat Name -> TC Expr

checkPat ty e (AST.PCon n) =
  do nty <- monoInst =<< lookupVar n
     unify ty nty
     return (EEq ty e (ECon nty n))

-- XXX make sure to check that the number fits in the bounds expected
checkPat ty e (AST.PNum n) =
  do return (EEq ty e (ENum n))

checkPat ty e (AST.PLoc loc) = withLoc loc (checkPat ty e)


-- | Locally introduce types for function parameters, and run the body.
withParams :: Type -> [AST.Loc Name] -> (([Type],Type) -> TC a) -> TC a
withParams ty params body =
  -- it should be impossible for the number of function arguments and number
  -- of parameters to be different, as the user never has control over the type
  -- signature
  let tyParts@(args,_) = destTFun ty
   in withTypes (zip (map thing params) args) (body tyParts)

-- | Produce a type that follows the shape of the function definition.
guessType :: AST.Loc (AST.Fun Name) -> TC Type
guessType loc = withLoc loc $ \ AST.Fun { fName, fParams } ->
  do vars <- traverse (`withLoc` (newTVar . Just)) fParams
     res  <- withLoc fName (newTVar . Just)
     return $! mkFun vars res
  where
  mkFun vars res = foldl' (\acc t -> TFree t `TFun` acc) (TFree res) vars


-- | Translate a parsed type into a core type.
translateType :: AST.Type Name -> TC Type
translateType AST.TBool        = return TBool
translateType AST.TInt         = return TInt
translateType (AST.TEnum name) = return (TEnum name)
translateType (AST.TFun l r)   = do l' <- translateType l
                                    r' <- translateType r
                                    return (TFun l' r')
translateType (AST.TLoc loc)   = withLoc loc translateType


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
