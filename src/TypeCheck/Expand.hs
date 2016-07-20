{-# LANGUAGE RecordWildCards #-}

module TypeCheck.Expand (expand) where

import Panic (panic,HasCallStack)
import Scope.Name
import TypeCheck.AST

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Language.Slugs.Lens (rewriteOf)

-- | Expand all macro uses, and remove function declarations from the
-- controller.
expand :: Controller -> Controller
expand Controller { .. } =
  Controller { cFuns        = []
             , cInputs      = expand' env cInputs
             , cOutputs     = expand' env cOutputs
             , cEnvTrans    = expand' env cEnvTrans
             , cEnvLiveness = expand' env cEnvLiveness
             , cSysTrans    = expand' env cSysTrans
             , cSysLiveness = expand' env cSysLiveness
             , .. }

  where
  env = Map.fromList [ (fName f, f) | g <- cFuns, f <- F.toList g ]


subst :: Map.Map Name Expr -> Expr -> Expr
subst env = rewriteOf traverseExpr f
  where
  f (EVar v) = Map.lookup v env
  f _        = Nothing


type Env = Map.Map Name Fun

lookupFun :: HasCallStack => Name -> Env -> Fun
lookupFun n env = Map.findWithDefault missing n env
  where
  missing = panic ("Macro missing from environment: " ++ show n)

expandDef :: Env -> Name -> [Expr] -> Expr
expandDef env f args =
  let Fun { .. } = lookupFun f env
      inst       = Map.fromList (zip (map fst fParams) args)
   in subst inst fBody

class Expand a where
  expand' :: HasCallStack => Env -> a -> a

instance Expand a => Expand (Maybe a) where
  expand' env = fmap (expand' env)

instance Expand a => Expand [a] where
  expand' env = map (expand' env)

instance Expand StateVar where
  expand' env StateVar { .. } = StateVar { svInit = expand' env svInit, .. }

instance Expand Expr where

  expand' env e =
    case destEApp e of

      (EVar f, args) | not (null args) -> expandDef env f args

      (ETrue,    []) -> e
      (EFalse,   []) -> e
      (EVar{},   []) -> e
      (ECon{},   []) -> e

      (ENum{},   []) -> e
      (EAnd l r, []) -> EAnd  (expand' env l) (expand' env r)
      (EOr  l r, []) -> EOr   (expand' env l) (expand' env r)
      (ENot l,   []) -> ENot  (expand' env l)
      (ENext l,  []) -> ENext (expand' env l)
      (EEq  l r, []) -> EEq   (expand' env l) (expand' env r)

      _ -> panic ("Unexpected expression: " ++ show e)
