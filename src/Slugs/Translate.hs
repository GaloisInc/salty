{-# LANGUAGE RecordWildCards #-}

module Slugs.Translate ( mkSpec ) where

import Slugs.Env
import TypeCheck.AST

import           Control.Monad (guard)
import qualified Language.Slugs as Slugs


-- Translation -----------------------------------------------------------------

panic :: String -> a
panic str = error ("PANIC: " ++ str)

mkSpec :: Controller -> (Slugs.Spec,Env)
mkSpec cont = (spec,env)
  where
  spec =
    Slugs.Spec { Slugs.specEnv    = mkState env (cInputs cont)  (cEnvTrans cont) (cEnvLiveness cont)
               , Slugs.specSys    = mkState env (cOutputs cont) (cSysTrans cont) (cSysLiveness cont)
               , .. }
  (env,specInput,specOutput) = mkEnv cont

mkState :: Env -> [StateVar] -> Expr -> Expr -> Slugs.State
mkState env vars trans liveness =
  Slugs.State { Slugs.stInit =
                do guard (not (null vars))
                   return (foldl1 Slugs.EAnd inits)

              , Slugs.stTrans =
                do guard (trans /= ETrue)
                   return (mkExpr env trans)

              , Slugs.stLiveness =
                do guard (liveness /= ETrue)
                   return (mkExpr env liveness)
              }

  where
  inits = [ mkInit env sv e | sv@StateVar { svInit = Just e, .. } <- vars ]

mkInit :: Env -> StateVar -> Expr -> Slugs.Expr
mkInit env StateVar { .. } e =
  case (svType, e) of

    (TEnum _, ECon c) ->
       Slugs.assignConst (lookupVar svName env) (lookupConstr c env)

    (TInt, ENum n) -> Slugs.assignConst (lookupVar svName env) n

    (TBool, ETrue) ->
      Slugs.EVar (lookupVar svName env)

    (TBool, EFalse) ->
      Slugs.ENeg (Slugs.EVar (lookupVar svName env))

    _ -> panic ("mkDecl: " ++ show e ++ " : " ++ show svType)

-- | Translate a boolean-valued expression.
mkExpr :: Env -> Expr -> Slugs.Expr
mkExpr _   ETrue            = Slugs.ETrue
mkExpr _   EFalse           = Slugs.EFalse
mkExpr env (EVar v)         = Slugs.EVar (lookupVar v env)
mkExpr env (EEq l r)        = mkAssign env l r
mkExpr env (ENot a)         = Slugs.ENeg (mkExpr env a)
mkExpr env (EAnd a b)       = Slugs.EAnd (mkExpr env a) (mkExpr env b)
mkExpr env (EOr  a b)       = Slugs.EOr  (mkExpr env a) (mkExpr env b)
mkExpr _   (ECon _)         = panic "mkExpr: Constructor used outside of assignment"
mkExpr _   (EApp _ _)       = panic "mkExpr: Unexpected EApp"
mkExpr _   (ENum _)         = panic "mkExpr: Unexpected ENum"
mkExpr env (ENext (EVar v)) = Slugs.ENext (lookupVar v env)
mkExpr _   (ENext _)        = panic "mkExpr: Unexpected ENext"


-- | Translate a use of equality.
mkAssign :: Env -> Expr -> Expr -> Slugs.Expr

mkAssign env (EVar n) (ECon c) =
  Slugs.assignConst (lookupVar n env) (lookupConstr c env)

mkAssign env (ECon c) (EVar n) =
  Slugs.assignConst (lookupVar n env) (lookupConstr c env)

mkAssign env (EVar n) ETrue  = Slugs.EVar (lookupVar n env)
mkAssign env (EVar n) EFalse = Slugs.EVar (lookupVar n env)

mkAssign env ETrue  (EVar n) = Slugs.EVar (lookupVar n env)
mkAssign env EFalse (EVar n) = Slugs.EVar (lookupVar n env)

mkAssign env (EVar n) (ENum a) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)
mkAssign env (ENum a) (EVar n) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)

mkAssign _ a b = panic ("mkAssign: Invalid arguments: " ++ show (EEq a b))
