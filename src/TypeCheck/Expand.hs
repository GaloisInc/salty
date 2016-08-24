{-# LANGUAGE RecordWildCards #-}

module TypeCheck.Expand (expand) where

import Panic (panic,HasCallStack)
import Scope.Name
import TypeCheck.AST

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Language.Slugs.Lens (transformOf)

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
      (EPrim PAny, [set]) ->
        let set' = expand' env set
         in case set' of
              ESet _ es -> expand' env (foldl EOr EFalse es)
              _         -> EAny set'

      (EPrim PAll, [set]) ->
        let set' = expand' env set
         in case set' of
              ESet _ es -> expand' env (foldl EAnd ETrue es)
              _         -> EAll set'

      (EPrim (PIn ty), [a,set]) ->
        let a'   = expand' env a
            set' = expand' env set
         in case set' of
              ESet _ es -> expand' env (foldl EOr EFalse [ EEq ty a' x | x <- es ])
              _         -> EIn ty a' set'

      (EPrim (PNext _), [x]) -> eNext (expand' env x)

      -- macro expansion
      (EVar _ f, args)
        | not (null args) || Map.member f env -> expand' env (expandDef env f args)

      -- generic application case
      (f, args) | not (null args) -> eApp (expand' env f) (map (expand' env) args)

      (ESet ty es, []) -> ESet ty (map (expand' env) es)

      (ELet n ty b x, []) -> ELet n ty (expand' env b) (expand' env x)

      _ -> e



-- | Push the next operation down to the leaves of an expression.
eNext :: Expr -> Expr
eNext  = transformOf traverseExpr $ \ e ->
  case e of
    EVar ty _ -> ENext ty e
    _         -> e
