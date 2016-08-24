{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Opt.HashCons (hashCons) where

import Scope.Name
import TypeCheck.AST

import qualified Data.Map.Strict as Map
import           MonadLib


hashCons :: Supply -> Controller -> (Controller,Supply)
hashCons s cont =
  case runM (unHC (hashCons' cont)) (emptyRW s) of
    (c,RW { .. })  -> (c, rwSupply)

-- Monad -----------------------------------------------------------------------

type Bindings  = Map.Map Name (Type,Expr)
type Bindings' = Map.Map Expr Name

data RW = RW { rwSupply :: !Supply
             , rwEnv    :: !Bindings
             , rwEnv'   :: !Bindings'
             }

emptyRW :: Supply -> RW
emptyRW rwSupply = RW { rwEnv = Map.empty, rwEnv' = Map.empty, ..}

-- | Bind all intermediate values.
buildEnv :: Bindings -> Expr -> Expr
buildEnv env e = Map.foldrWithKey (\n (t,b) -> ELet n t b) e env


newtype HC a = HC { unHC :: StateT RW Id a
                  } deriving (Functor,Applicative,Monad)

fresh :: HC Name
fresh  = HC $
  do RW {..} <- get
     let (name,s') = mkName (Generated "CSE") "cse" rwSupply
     set $! RW { rwSupply = s', ..}
     return name


freshFrom :: Name -> HC Name
freshFrom n = HC $
  do RW {..} <- get
     let (name,s') = mkName (nameOrigin n) (nameText n) rwSupply
     set $! RW { rwSupply = s', ..}
     return name


resolve :: Type -> Name -> HC Expr
resolve ty n = HC $
  do RW {..} <- get
     return $! snd $ Map.findWithDefault (ty,EVar ty n) n rwEnv


cache :: Type -> Expr -> HC Expr
cache ty e =
  do RW {..} <- HC get
     case Map.lookup e rwEnv' of
       Just n  -> return (EVar ty n)
       Nothing -> do res <- fresh
                     nameExpr res ty e
                     return (EVar ty res)

cacheAs :: Name -> Type -> Expr -> HC Expr
cacheAs n ty e =
  do RW {..} <- HC get
     case Map.lookup e rwEnv' of
       Just n' -> return (EVar ty n')
       Nothing -> do nameExpr n ty e
                     return (EVar ty n)


nameExpr :: Name -> Type -> Expr -> HC ()
nameExpr res ty e = HC $
  do RW {..} <- get
     set $! RW { rwEnv  = Map.insert res (ty,e) rwEnv
               , rwEnv' = Map.insert e res rwEnv'
               , ..}


-- | Scope all bindings to this site.
scope :: HC Expr -> HC Expr
scope body = HC $
  do rw  <- get
     e   <- unHC body
     rw' <- get
     let env = rwEnv rw' `Map.difference` rwEnv rw
     set rw
     return $! buildEnv env e


-- Hash Consing ----------------------------------------------------------------

class HashCons a where
  hashCons' :: a -> HC a

instance HashCons a => HashCons [a] where
  hashCons' = traverse hashCons'

instance HashCons a => HashCons (Maybe a) where
  hashCons' = traverse hashCons'

instance HashCons Controller where
  hashCons' Controller { .. } =
    do is  <- hashCons' cInputs
       os  <- hashCons' cOutputs
       ets <- scope (hashCons' cEnvTrans)
       els <- scope (hashCons' cEnvLiveness)
       sts <- scope (hashCons' cSysTrans)
       sls <- scope (hashCons' cSysLiveness)
       return Controller { cInputs      = is
                         , cOutputs     = os
                         , cEnvTrans    = ets
                         , cEnvLiveness = els
                         , cSysTrans    = sts
                         , cSysLiveness = sls
                         , .. }

instance HashCons StateVar where
  hashCons' StateVar {..} =
    do mb <- traverse (scope . hashCons') svInit
       return StateVar { svInit = mb, ..}

instance HashCons Expr where
  hashCons' ETrue        = pure ETrue
  hashCons' EFalse       = pure EFalse
  hashCons' e@EIn{}      = pure e
  hashCons' e@EPrim{}    = pure e
  hashCons' e@ECon{}     = pure e
  hashCons' e@ENum{}     = pure e

  hashCons' (EVar ty n)  = resolve ty n

  hashCons' (ENext ty p) = do p' <- hashCons' p 
                              return (ENext ty p')

  hashCons' e@EApp{}     = do let (f, xs) = destEApp e
                              f'  <- hashCons' f
                              xs' <- traverse hashCons' xs
                              let e' = eApp f' xs'
                              cache (typeOf e') e'

  -- NOTE: sets aren't cached, as they are values that slugs can't represent
  hashCons' (ESet ty es) = do es' <- traverse hashCons' es
                              return (ESet ty es')

  hashCons' (ELet n t b e) = do b' <- hashCons' b
                                _  <- cacheAs n t b'
                                e' <- hashCons' e
                                return (ELet n t b' e')
