{-# LANGUAGE RecordWildCards #-}

module Slugs.Translate ( mkSpec ) where

import Panic (panic,HasCallStack)
import Slugs.Env
import TypeCheck.AST

import           Control.Monad (guard)
import           Data.List (foldl1')
import qualified Language.Slugs as Slugs


-- Translation -----------------------------------------------------------------

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
                   return (conj inits)

              , Slugs.stTrans =
                do guard (trans /= ETrue)
                   return (mkExpr env trans)

              , Slugs.stLiveness =
                do guard (liveness /= ETrue)
                   return (mkExpr env liveness)
              }

  where

  conj []  = Slugs.ETrue
  conj [e] = e
  conj es  = foldl1 Slugs.EAnd es

  inits = [ mkInit env sv e | sv@StateVar { svInit = Just e, .. } <- vars ]

mkInit :: Env -> StateVar -> Expr -> Slugs.Expr
mkInit env StateVar { .. } e = mkExpr env (EEq (EVar svName) e)

-- | Translate a boolean-valued expression.
mkExpr :: HasCallStack => Env -> Expr -> Slugs.Expr
mkExpr _   ETrue            = Slugs.ETrue
mkExpr _   EFalse           = Slugs.EFalse
mkExpr env (EVar v)         = Slugs.EVar (lookupVar v env)
mkExpr env (EEq l r)        = mkAssign env l r
mkExpr env (ENot a)         = Slugs.ENeg (mkExpr env a)
mkExpr env (EAnd a b)       = Slugs.EAnd (mkExpr env a) (mkExpr env b)
mkExpr env (EOr  a b)       = Slugs.EOr  (mkExpr env a) (mkExpr env b)
mkExpr _   (ECon _)         = panic "Constructor used outside of assignment"
mkExpr _   (EApp _ _)       = panic "Unexpected EApp"
mkExpr _   (ENum _)         = panic "Unexpected ENum"
mkExpr env (ENext (EVar v)) = Slugs.ENext (lookupVar v env)
mkExpr _   (ENext _)        = panic "Unexpected ENext"


-- | Translate a use of equality.
mkAssign :: HasCallStack => Env -> Expr -> Expr -> Slugs.Expr

-- constant enum values
mkAssign env (EVar n) (ECon c) =
  Slugs.assignConst (lookupVar n env) (lookupConstr c env)

mkAssign env (ECon c) (EVar n) =
  Slugs.assignConst (lookupVar n env) (lookupConstr c env)

-- constant booleans
mkAssign env (EVar n) ETrue  =             Slugs.EVar (lookupVar n env)
mkAssign env (EVar n) EFalse = Slugs.ENeg (Slugs.EVar (lookupVar n env))
mkAssign env ETrue  (EVar n) =             Slugs.EVar (lookupVar n env)
mkAssign env EFalse (EVar n) = Slugs.ENeg (Slugs.EVar (lookupVar n env))

-- constant numbers
mkAssign env (EVar n) (ENum a) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)
mkAssign env (ENum a) (EVar n) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)

-- variable assignment
mkAssign env (EVar a) (EVar b) =
  let sa = lookupVar a env
      sb = lookupVar b env
   in case (sa,sb) of

        (Slugs.VarBool{}, Slugs.VarBool{}) ->
          mkEq (Slugs.EVar sa) (Slugs.EVar sb)

        (Slugs.VarNum _ _ hia, Slugs.VarNum _ _ hib) ->
          let numBits = Slugs.numBits (min hia hib)
           in mkEAnd [ mkEq (mkEBit sa i) (mkEBit sb i) | i <- [0 .. numBits - 1] ]

        _ -> panic "Incompatible variables in assignment"


mkAssign _ a b = panic ("Invalid arguments: " ++ show (EEq a b))


mkEq :: Slugs.Expr -> Slugs.Expr -> Slugs.Expr
mkEq a b = Slugs.EOr (Slugs.EAnd a b) (Slugs.EAnd (Slugs.ENeg a) (Slugs.ENeg b))

mkEBit :: Slugs.Var -> Int -> Slugs.Expr
mkEBit v i = Slugs.EBit v i


mkEAnd :: [Slugs.Expr] -> Slugs.Expr
mkEAnd [] = Slugs.ETrue
mkEAnd es = foldl1' step es
  where
  step acc (Slugs.EAnd a b) = Slugs.EAnd a (Slugs.EAnd b acc)
  step acc e                = Slugs.EAnd e acc
