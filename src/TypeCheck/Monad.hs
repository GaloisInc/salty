{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeCheck.Monad (
    -- * Type-checking Monad
    TC(),
    runTC, TCError(..),
    addLoc, withLoc,
    askLoc,

    -- ** Unification
    unify,
    zonk,
    Unify.ftvs,

    -- ** Type Environment 
    lookupVar,
    newTVar,
    withTypes,
    addTypes,

    -- ** Error handling
    record,
    failErrors,
    failWith,
    try,
    tryAll,
    collectErrors,

    -- ** Errors
    ppTCError,
    invalidRecursiveGroup,
    tooManyDefaultCases,
  ) where

import           PP
import           Scope.Name (Name,Supply,nextUnique)
import           Syntax.AST (Loc)
import qualified Syntax.AST as AST
import           TypeCheck.AST (Type(..),TVar(..),Schema(..))
import qualified TypeCheck.Unify as Unify

import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import qualified MonadLib
import           MonadLib hiding (try)
import           Text.Location (HasLoc(..),Range,thing,at)

newtype TC a = TC { unTC :: StateT RW (ExceptionT [Loc TCError] Lift) a
                  } deriving (Functor,Applicative,Monad)

data RW = RW { rwSubst :: !Unify.Env
             , rwLoc   :: !(Range FilePath)
             , rwEnv   :: Map.Map Name TCType
             , rwSupply:: !Supply
             , rwErrs  :: ![Loc TCError]
             }

emptyRW :: Supply -> RW
emptyRW rwSupply = RW { rwSubst = Unify.emptyEnv
                      , rwLoc   = mempty
                      , rwEnv   = Map.empty
                      , rwErrs  = []
                      , .. }

data TCType = HasType !Schema -- ^ A variable with known type.
              deriving (Show)

data TCError = UnifyError Unify.UnifyError
             | InvalidRecursiveGroup [AST.TopDecl Name]
             | TooManyDefaultCases [AST.Case Name]
             | MissingBounds (AST.Loc Name)
               deriving (Show)

ppTCError :: Loc TCError -> Doc
ppTCError err =
  vcat [ banner, nest 2 body, text " " ]

  where
  
  banner = text "[error]" <+> pp (getLoc err)

  body =
    case thing err of
      UnifyError ue ->
        pp ue

      -- XXX print more information
      InvalidRecursiveGroup _ ->
        text "Recursive functions are not supported"

      -- XXX print more information
      TooManyDefaultCases _ ->
        text "Too many default cases in case expression"

      MissingBounds n ->
        text "Numeric state variable" <+> ticks (pp (thing n)) <+> text "is missing bounds"

-- | Run a TC action.
runTC :: Supply -> TC a -> Either [Loc TCError] (a,Supply)
runTC sup m =
  case runM (unTC (failErrors m)) (emptyRW sup) of
    Right (a,rw) -> Right (a,rwSupply rw)
    Left errs    -> Left errs

-- | Decorate errors generated in the action with location information provided.
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
unify a b =
  do RW { .. } <- TC get
     case Unify.unify a b rwSubst of
       Right rwSubst' -> TC (set $! RW { rwSubst = rwSubst', .. })
       Left err       -> record (UnifyError err)

-- | Remove type variables from a type.
zonk :: Unify.Zonk ty => ty -> TC ty
zonk ty =
  do RW { .. } <- TC get
     case Unify.zonk ty rwSubst of
       Right ty' -> return ty'
       Left  err -> failWith (UnifyError err)


-- Type Environment ------------------------------------------------------------

-- | Lookup the type of a variable.
lookupVar :: Name -> TC Schema
lookupVar n = TC $
  do RW { .. } <- get
     case Map.lookup n rwEnv of
       Just (HasType s) -> return s
       Nothing          -> error ("Unknown variable: " ++ show n)

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
  do let binds = Map.fromList [ (n, HasType (Forall [] ty)) | (n,ty) <- tys ]
     env <- sets (\ RW { .. } -> (rwEnv, RW { rwEnv = Map.union binds rwEnv, .. }))
     a   <- unTC body
     sets_ (\ RW { .. } -> RW { rwEnv = env, .. })
     return a

-- | Permanently add bindings to the typing environment.
addTypes :: [(Name,Schema)] -> TC ()
addTypes tys =
  let binds = Map.fromList [ (n, HasType s) | (n,s) <- tys ]
   in TC (sets_ (\ RW { .. } -> RW { rwEnv = Map.union binds rwEnv, .. }))


-- Errors ----------------------------------------------------------------------

record :: TCError -> TC ()
record err =
  do loc <- askLoc
     addErrs [err `at` loc]

addErrs :: [Loc TCError] -> TC ()
addErrs errs = TC (sets_ (\rw -> rw { rwErrs = errs ++ rwErrs rw }))

getErrs :: TC [Loc TCError]
getErrs  = TC (sets (\rw -> (rwErrs rw, rw { rwErrs = [] })))

setErrs :: [Loc TCError] -> TC ()
setErrs errs = TC (sets_ (\rw -> rw { rwErrs = errs }))

-- | Fails when the given action has added errors to the environment.
failErrors :: TC a -> TC a
failErrors m =
  do (a,errs)<- collectErrors m
     unless (null errs) (TC (raise errs))

     return a

-- | When we'd like to fail right away.
failWith :: TCError -> TC a
failWith err = TC $
  do rw <- get
     raise ((err `at` rwLoc rw) : rwErrs rw)

try :: TC a -> TC (Either [Loc TCError] a)
try m = TC (MonadLib.try (unTC m))

tryAll :: [TC a] -> TC [a]
tryAll ms = TC $
  do es <- mapM (MonadLib.try . unTC) ms
     let (as,bs) = partitionEithers es
     unless (null as) (raise (concat as))
     return bs


-- | Collect any errors that get recorded when running the given action.
-- Failures will still propagate through.
collectErrors :: TC a -> TC (a,[Loc TCError])
collectErrors m =
  do errs  <- getErrs
     a     <- m
     errs' <- getErrs
     setErrs errs
     return (a,errs')

-- | A recursive group that does not consist of just functions.
invalidRecursiveGroup :: [AST.TopDecl Name] -> TCError
invalidRecursiveGroup g = InvalidRecursiveGroup g

-- | Too many default cases in a case expression.
tooManyDefaultCases :: [AST.Case Name] -> TCError
tooManyDefaultCases arms = TooManyDefaultCases arms
