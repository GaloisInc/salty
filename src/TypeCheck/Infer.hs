{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module TypeCheck.Infer where

import           Scope.Name (Name)
import qualified Syntax.AST as AST
import           TypeCheck.AST
import           TypeCheck.Groups
import           TypeCheck.Monad

import           Control.Monad (unless)
import           Data.List (foldl')
import           Text.Location (thing,getLoc,at)


inferController :: AST.Controller Name -> TC Controller
inferController AST.Controller { AST.cName, AST.cDecls } =
  do updates <- collectErrors (map inferTopGroup (sccTopDecls cDecls))
     return $! foldr (id) (emptyController (thing cName)) updates


inferTopGroup :: Group (AST.TopDecl Name) -> TC (Controller -> Controller)

inferTopGroup (NonRecursive td) =
  simpleTopDecl td

inferTopGroup (Recursive tds)   =
  case traverse mkLocFun tds of
    Just funs -> inferFunGroup True funs
    _         -> invalidRecursiveGroup tds


mkLocFun :: AST.TopDecl Name -> Maybe (AST.Loc (AST.Fun Name))
mkLocFun  = go mempty
  where
  go _   (AST.TDLoc loc) = go (getLoc loc) (thing loc)
  go src (AST.TDFun fun) = Just (fun `at` src)
  go _   _               = Nothing



simpleTopDecl :: AST.TopDecl Name -> TC (Controller -> Controller)

simpleTopDecl (AST.TDEnum enum) = checkEnum enum

simpleTopDecl (AST.TDFun fun) =
  do loc <- askLoc
     inferFunGroup False [fun `at` loc]

simpleTopDecl (AST.TDLoc loc) = withLoc loc simpleTopDecl


-- | Add all of the constructors to the typing environment.
checkEnum :: AST.EnumDef Name -> TC (Controller -> Controller)
checkEnum AST.EnumDef { eName, eCons } =
  do let ty   = TEnum (thing eName)
         cons = map thing eCons

     addTypes (zip cons (repeat ty))

     return $ \ c -> c { cEnums = EnumDef { eName = thing eName
                                          , eCons = cons
                                          } : cEnums c }


-- | Check a recursive group of functions:
--
--  * Introduce type-variables for all functions in the group
--  * Check each function individually 
--  * Apply the substitution to each function, yielding its type
inferFunGroup :: Bool -> [AST.Loc (AST.Fun Name)] -> TC (Controller -> Controller)
inferFunGroup isRec funs =
  do let names = [ thing (AST.fName (thing locFun)) | locFun <- funs ]
     tys   <- traverse guessType funs
     funs' <- withTypes (zip names tys) (collectErrors (zipWith checkFun tys funs))
     tys'  <- traverse zonk tys

     addTypes (zip names tys')

     let group = case funs' of
                   [f] | not isRec -> NonRecursive f
                   _               -> Recursive funs'

     return $ \ c -> c { cFuns = group : cFuns c }


-- | NOTE: type variables can still be present after zonking at this stage,
-- which just indicates that the variables are unused. It might be good to
-- propagate this information down, so that the function can ultimately be
-- re-written to not include that parameter.
checkFun :: Type -> AST.Loc (AST.Fun Name) -> TC Fun
checkFun ty locFun = withLoc locFun $ \ AST.Fun { fName, fParams, fBody } ->
  withParams ty fParams $ \ (ps,rty) ->
    do e <- checkGuard rty fBody
       ps'  <- zonk ps
       rty' <- zonk rty
       return Fun { fName   = thing fName
                  , fParams = zip (map thing fParams) ps'
                  , fResult = rty'
                  , fBody   = e }


-- | Translate guards into if-then-else syntax.
checkGuard :: Type -> [AST.Guard Name] -> TC Expr
checkGuard ty = go
  where

  -- In the simple case, the guarded RHS is just a single expression. This
  -- translates to no conditionals on the RHS.
  go (AST.GExpr e:gs) =
    do e' <- checkExpr ty e
       unless (null gs) (unreachableCases gs)
       return e'

  go (AST.GGuard p e:gs) =
    do p' <- checkExpr TBool p
       e' <- checkExpr ty e
       k  <- if null gs
                then return EFalse
                else go gs
  
       return (EIf p' e' k)
  
  go (AST.GLoc loc:gs) =
    withLoc loc (\g -> go (g:gs))


-- | Type-check an expression, producing a type-checked expression as the
-- result.
checkExpr :: Type -> AST.Expr Name -> TC Expr

checkExpr ty (AST.EVar n) =
  do ty' <- lookupVar n
     unify ty ty'
     return (EVar n)

checkExpr ty (AST.ECon n) =
  do ty' <- lookupVar n
     unify ty ty'
     return (ECon n)

checkExpr ty (AST.ENum i) =
  do unify ty TNum
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
     return (EIf p' t' f')

checkExpr ty (AST.EApp f x) =
  do xtv <- newTVar Nothing
     let xty = TFree xtv
     f'  <- checkExpr (TFun xty ty) f
     x'  <- checkExpr xty x
     return (EApp f' x')

checkExpr ty (AST.ELoc loc) = withLoc loc (checkExpr ty)


-- | Locally introduce types for function parameters, and run the body.
withParams :: Type -> [AST.Loc Name] -> (([Type],Type) -> TC a) -> TC a
withParams ty params body =
  -- it should be impossible for the number of function arguments and number
  -- of parameters to be different
  let tyParts@(args,res) = destTFun ty
   in withTypes (zip (map thing params) args) (body tyParts)

-- | Produce a type that follows the shape of the function definition.
guessType :: AST.Loc (AST.Fun Name) -> TC Type
guessType locFun = withLoc locFun $ \ AST.Fun { fName, fParams } ->
  do vars <- traverse (`withLoc` (newTVar . Just)) fParams
     res  <- withLoc fName (newTVar . Just)
     return $! mkFun vars res
  where
  mkFun vars res = foldl' (\acc t -> TFree t `TFun` acc) (TFree res) vars
