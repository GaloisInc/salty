{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
mkExpr env (EEq l r)        = mkAssign env l r
mkExpr env (ENot a)         = Slugs.ENeg (mkExpr env a)
mkExpr env (EAnd a b)       = Slugs.EAnd (mkExpr env a) (mkExpr env b)
mkExpr env (EOr  a b)       = Slugs.EOr  (mkExpr env a) (mkExpr env b)
mkExpr _   (ECon _)         = panic "Constructor used outside of assignment"
mkExpr _   (EApp _ _)       = panic "Unexpected EApp"
mkExpr _   (ENum _)         = panic "Unexpected ENum"

mkExpr env (EVar v)         = mkVar Slugs.UVar  (lookupVar v env)
mkExpr env (ENext (EVar v)) = mkVar Slugs.UNext (lookupVar v env)

mkExpr _   (ENext _)        = panic "Unexpected ENext"


mkVar :: (Slugs.Var -> Slugs.Use) -> Slugs.Var -> Slugs.Expr

mkVar mk var@Slugs.VarBool{} =
  Slugs.EVar (mk var)

mkVar mk var@Slugs.VarNum{}  =
  let use = mk var
   in mkEAnd [ Slugs.EBit use i | i <- [0 .. Slugs.varBitSize var - 1] ]


-- | Translate an expression that's expected to be a variable (or a use of Next
-- with a variable)
slugsUse :: Env -> Expr -> Maybe Slugs.Use
slugsUse env (EVar v)         = Just (Slugs.UVar  (lookupVar v env))
slugsUse env (ENext (EVar v)) = Just (Slugs.UNext (lookupVar v env))
slugsUse _   _                = Nothing


-- | Translate a use of equality.
mkAssign :: HasCallStack => Env -> Expr -> Expr -> Slugs.Expr

-- constant enum values
mkAssign env (slugsUse env -> Just use) (ECon c) =
  Slugs.assignConst use (lookupConstr c env)

mkAssign env (ECon c) (slugsUse env -> Just use) =
  Slugs.assignConst use (lookupConstr c env)

-- constant booleans
mkAssign env (slugsUse env -> Just use) ETrue  =             Slugs.EVar use
mkAssign env (slugsUse env -> Just use) EFalse = Slugs.ENeg (Slugs.EVar use)
mkAssign env ETrue  (slugsUse env -> Just use) =             Slugs.EVar use
mkAssign env EFalse (slugsUse env -> Just use) = Slugs.ENeg (Slugs.EVar use)

-- constant numbers
mkAssign env (EVar n) (ENum a) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)
mkAssign env (ENum a) (EVar n) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)

-- variable assignment
mkAssign env (slugsUse env -> Just sa) (slugsUse env -> Just sb) =
  case (Slugs.toVar sa,Slugs.toVar sb) of

    (Slugs.VarBool{}, Slugs.VarBool{}) ->
      mkEq (Slugs.EVar sa) (Slugs.EVar sb)

    (Slugs.VarNum _ _ hia, Slugs.VarNum _ _ hib) ->
      let numBits = Slugs.numBits (min hia hib)
       in mkEAnd [ mkEq (mkEBit sa i) (mkEBit sb i) | i <- [0 .. numBits - 1] ]

    _ -> panic "Incompatible variables in assignment"


mkAssign _ a b = panic ("Invalid arguments: " ++ show (EEq a b))


mkEq :: Slugs.Expr -> Slugs.Expr -> Slugs.Expr
mkEq a b = Slugs.EOr (Slugs.EAnd a b) (Slugs.EAnd (Slugs.ENeg a) (Slugs.ENeg b))

mkEBit :: Slugs.HasVar var => var -> Int -> Slugs.Expr
mkEBit var i = Slugs.EBit (Slugs.toUse var) i


mkEAnd :: [Slugs.Expr] -> Slugs.Expr
mkEAnd [] = Slugs.ETrue
mkEAnd es = foldl1' step es
  where
  step acc (Slugs.EAnd a b) = Slugs.EAnd a (Slugs.EAnd b acc)
  step acc e                = Slugs.EAnd e acc
