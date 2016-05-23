{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TypeCheck.Monad (
    -- * Type-checking Monad
    TC(),
    runTC, TCError(..),
    addLoc, withLoc,
    askLoc,

    -- ** Unification
    unify,
    zonk,

    -- ** Type Environment 
    lookupVar,
    newTVar,
    withTypes,
    addTypes,

    -- ** Errors
    collectErrors,
    invalidRecursiveGroup,
    unreachableCases,
  ) where

import           Scope.Name (Name,Supply,nextUnique)
import           Syntax.AST (Loc)
import qualified Syntax.AST as AST
import           TypeCheck.AST (Type(..),TVar(..))
import qualified TypeCheck.Unify as Unify

import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import           MonadLib
import           Text.Location (HasLoc(..),Range,thing)


newtype TC a = TC { unTC :: StateT RW (ExceptionT [TCError] Lift) a
                  } deriving (Functor,Applicative,Monad)

data RW = RW { rwSubst :: !Unify.Env
             , rwLoc   :: !(Range FilePath)
             , rwEnv   :: Map.Map Name TCType
             , rwSupply:: !Supply
             }

emptyRW :: Supply -> RW
emptyRW rwSupply = RW { rwSubst = Unify.emptyEnv
                      , rwLoc   = mempty
                      , rwEnv   = Map.empty
                      , .. }

data TCType = Checking Type
            | HasType !Type
              deriving (Show)

data TCError = UnifyError Unify.UnifyError
             | InvalidRecursiveGroup [AST.TopDecl Name]
             | UnreachableCases [AST.Guard Name]
               deriving (Show)

-- | Run a TC action.
runTC :: Supply -> TC a -> Either [TCError] (a,Supply)
runTC sup (TC m) =
  case runM m (emptyRW sup) of
    Right (a,i) -> Right (a,rwSupply i)
    Left  err   -> Left err

addLoc :: (LocSource loc ~ FilePath, HasLoc loc) => loc -> TC a -> TC a
addLoc loc m = TC $
  do rw  <- get
     set $! rw { rwLoc = getLoc loc }
     a   <- unTC m
     rw' <- get
     set $! rw' { rwLoc = rwLoc rw }
     return a

withLoc :: Loc a -> (a -> TC b) -> TC b
withLoc loc f = addLoc loc (f (thing loc))

askLoc :: TC (Range FilePath)
askLoc  = TC $
  do RW { .. } <- get
     return rwLoc


-- Unification -----------------------------------------------------------------

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


-- Type Environment ------------------------------------------------------------

-- | Lookup the type of a variable.
lookupVar :: Name -> TC Type
lookupVar n = TC $
  do RW { .. } <- get
     case Map.lookup n rwEnv of
       Just (Checking ty) -> return ty
       Just (HasType ty)  -> return ty
       Nothing            -> error ("Unknown variable: " ++ show n)

-- | Introduce a new type variable. The optional name is the origin of the
-- variable (for a type variable derived from a parameter, it's the name of the
-- function parameter).
newTVar :: Maybe Name -> TC TVar
newTVar tvOrigin = TC $ sets $ \ RW { .. } ->
  let (tvUnique,sup') = nextUnique rwSupply
   in (TVar { .. }, RW { rwSupply = sup', .. })

-- | Add a bunch of bindings to the typing environment, and run an action. The
-- bindings are only in the environment for the duration of the body.
withTypes :: [(Name,Type)] -> TC a -> TC a
withTypes tys body = TC $
  do let binds = Map.fromList [ (n, Checking ty) | (n,ty) <- tys ]
     env <- sets (\ RW { .. } -> (rwEnv, RW { rwEnv = Map.union binds rwEnv, .. }))
     a   <- unTC body
     sets_ (\ RW { .. } -> RW { rwEnv = env, .. })
     return a

-- | Permanently add bindings to the typing environment.
addTypes :: [(Name,Type)] -> TC ()
addTypes tys =
  let binds = Map.fromList [ (n, HasType ty) | (n,ty) <- tys ]
   in TC (sets_ (\ RW { .. } -> RW { rwEnv = Map.union binds rwEnv, .. }))



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

-- | These guarded cases are unreachable.
unreachableCases :: [AST.Guard Name] -> TC a
unreachableCases gs = TC (raise [UnreachableCases gs])
