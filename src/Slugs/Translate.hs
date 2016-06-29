{-# LANGUAGE RecordWildCards #-}

module Slugs.Translate where

import Scope.Name
import TypeCheck.AST

import qualified Data.Map.Strict as Map
import qualified Language.Slugs as Slugs


-- Environment -----------------------------------------------------------------

data Env = Env { envFuns :: Map.Map Name Fun
               , envVars :: Map.Map Name Slugs.Expr
               }

instance Monoid Env where
  mempty = Env { envFuns = Map.empty, envVars = Map.empty }

  mappend a b =
    Env { envFuns = Map.union (envFuns a) (envFuns b)
        , envVars = Map.union (envVars a) (envVars b)
        }

emptyEnv :: Env
emptyEnv  = Env { envFuns = Map.empty, envVars = Map.empty }

bindEnv :: Name -> Slugs.Expr -> Env -> Env
bindEnv n e = \ env -> env { envVars = Map.insert n e (envVars env) }

controllerEnv :: Controller -> Env
controllerEnv c =
  mconcat [ foldMap (foldMap funEnv) (cFuns c)
          , foldMap svEnv (cInputs c)
          , foldMap svEnv (cOutputs c)
          ]

funEnv :: Fun -> Env
funEnv  = undefined

svEnv :: StateVar -> Env
svEnv StateVar { .. } = undefined


-- Translation -----------------------------------------------------------------

translateController :: Controller -> Slugs.Spec
translateController cont =
  let env = controllerEnv cont
   in Slugs.Spec {
                 }
