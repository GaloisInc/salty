{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TypeCheck.Monad (
    -- * Type-checking Monad
    TC(),
    runTC, TCError(..),
    withLoc,

    -- ** Unification
    unify,
    zonk,

    -- ** Errors
    collectErrors,
    invalidRecursiveGroup,
  ) where

import           Scope.Name (Name)
import qualified Syntax.AST as AST
import qualified TypeCheck.Unify as Unify

import Data.Either (partitionEithers)
import MonadLib
import Text.Location (HasLoc(..),Range)


newtype TC a = TC { unTC :: StateT RW (ExceptionT [TCError] Lift) a
                  } deriving (Functor,Applicative,Monad)

data RW = RW { rwSubst :: !Unify.Env
             , rwLoc   :: !(Range FilePath)
             }

data TCError = UnifyError Unify.UnifyError
             | InvalidRecursiveGroup [AST.TopDecl Name]
               deriving (Show)

-- | Run a TC action.
runTC :: TC a -> Either [TCError] a
runTC (TC m) =
  case runM m RW { rwSubst = Unify.emptyEnv, rwLoc = mempty } of
    Right (a,_) -> Right a
    Left  err   -> Left err

-- | Unify two types.
unify :: Unify.Types ty => ty -> ty -> TC ()
unify a b = TC $
  do RW { .. } <- get
     case Unify.unify a b rwSubst of
       Right rwSubst' -> set $! RW { rwSubst = rwSubst', .. }
       Left err       -> raise [UnifyError err]

-- | Remove type variables from a type.
zonk :: Unify.Types ty => ty -> TC ty
zonk ty = TC $
  do RW { .. } <- get
     case Unify.zonk ty rwSubst of
       Right ty' -> return ty'
       Left  err -> raise [UnifyError err]

withLoc :: (LocSource loc ~ FilePath, HasLoc loc) => loc -> TC a -> TC a
withLoc loc m = TC $
  do rw  <- get
     set $! rw { rwLoc = getLoc loc }
     a   <- unTC m
     rw' <- get
     set $! rw' { rwLoc = rwLoc rw }
     return a


-- Errors ----------------------------------------------------------------------

-- | Run all the sub-computations, yielding either a list of results, or pushing
-- out a list of errors.
collectErrors :: [TC a] -> TC [a]
collectErrors ms = TC $
  do es <- traverse (try . unTC) ms
     let (errs,as) = partitionEithers es
     case concat errs of
       [] -> return as
       ls -> raise ls

-- | A recursive group that does not consist of just functions.
invalidRecursiveGroup :: [AST.TopDecl Name] -> TC a
invalidRecursiveGroup g = TC (raise [InvalidRecursiveGroup g])
