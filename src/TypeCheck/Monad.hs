{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module TypeCheck.Monad (
    TC(),
    runTC, TCError(..),
    unify,
    zonk,
  ) where

import qualified TypeCheck.Unify as Unify

import MonadLib


newtype TC a = TC { unTC :: StateT RW (ExceptionT TCError Lift) a
                  } deriving (Functor,Applicative,Monad)

data RW = RW { rwSubst :: !Unify.Env
             }

data TCError = UnifyError Unify.UnifyError
               deriving (Show)

-- | Run a TC action.
runTC :: TC a -> Either TCError a
runTC (TC m) =
  case runM m RW { rwSubst = Unify.emptyEnv } of
    Right (a,_) -> Right a
    Left  err   -> Left err

-- | Unify two types.
unify :: Unify.Types ty => ty -> ty -> TC ()
unify a b = TC $
  do RW { .. } <- get
     case Unify.unify a b rwSubst of
       Right rwSubst' -> set $! RW { rwSubst = rwSubst', .. }
       Left err       -> raise (UnifyError err)

-- | Remove type variables from a type.
zonk :: Unify.Types ty => ty -> TC ty
zonk ty = TC $
  do RW { .. } <- get
     case Unify.zonk ty rwSubst of
       Right ty' -> return ty'
       Left  err -> raise (UnifyError err)
