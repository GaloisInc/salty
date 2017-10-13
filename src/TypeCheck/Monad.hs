{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.Monad (
    -- * Type-checking Monad
    TC(),
    runTC, TCError(..),
    addLoc, withLoc,
    askLoc,

    -- ** Unification
    unify,
    zonk,
    ftvs,

    -- ** Type Environment
    lookupVar,
    newTVar,
    withTypes,
    addTypes,

    -- ** Value Environment

    newVarPair,
    getVars,

    -- ** Error handling
    record,
    failErrors,
    failWith,
    try,
    tryAll,
    collectErrors,

    -- ** Errors
    invalidRecursiveGroup,
    tooManyDefaultCases,
  ) where

import           PP
import           Scope.Check (Renamed)
import           Scope.Name (Name,Supply,nextUnique,mkName,Origin(Generated))
import           SrcLoc
import qualified Syntax.AST as AST
import           TypeCheck.AST (Type(..),TVar(..),Schema(..))
import qualified TypeCheck.Unify as Unify

import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified MonadLib
import           MonadLib hiding (try)

newtype TC a = TC { unTC :: StateT RW (ExceptionT [TCError] Lift) a
                  } deriving (Functor,Applicative,Monad)

data RW = RW { rwSubst :: !Unify.Env
             , rwLoc   :: !SrcLoc
             , rwEnv   :: Map.Map Name TCType
             , rwSupply:: !Supply
             , rwErrs  :: ![TCError]
             , rwTmps  :: ![(Name, Name)]
             }

emptyRW :: Supply -> RW
emptyRW rwSupply = RW { rwSubst = Unify.emptyEnv
                      , rwLoc   = mempty
                      , rwEnv   = Map.empty
                      , rwErrs  = []
                      , rwTmps  = []
                      , .. }

data TCType = HasType !Schema -- ^ A variable with known type.
              deriving (Show)

data TCError = UnifyError SrcLoc Unify.UnifyError
             | InvalidRecursiveGroup [AST.TopDecl Renamed]
             | TooManyDefaultCases [AST.Case Renamed]
             | MissingBounds Name
               deriving (Show)

instance HasSrcLoc TCError where
  srcLoc (UnifyError loc _)         = loc
  srcLoc (InvalidRecursiveGroup gs) = srcLoc gs
  srcLoc (TooManyDefaultCases cs)   = srcLoc cs
  srcLoc (MissingBounds n)          = srcLoc n

instance PP TCError where
  ppPrec _ (UnifyError _ ue) = pp ue
  ppPrec _ (InvalidRecursiveGroup _) =
      text "Recursive functions are not supported"
  ppPrec _ (TooManyDefaultCases _) =
      text "Too many default cases in case expression"
  ppPrec _ (MissingBounds n) =
      text "Numeric state variable"
      <+> ticks (pp n) <+> text "is missing bounds"


-- | Run a TC action.
runTC :: Supply -> TC a -> Either [TCError] (a,Supply)
runTC sup m =
  case runM (unTC (failErrors m)) (emptyRW sup) of
    Right (a,rw) -> Right (a,rwSupply rw)
    Left errs    -> Left errs

-- | Decorate errors generated in the action with location information provided.
addLoc :: HasSrcLoc loc => loc -> TC a -> TC a
addLoc loc m = TC $
  do rw  <- get
     set $! rw { rwLoc = srcLoc loc }
     a   <- unTC m
     rw' <- get
     set $! rw' { rwLoc = rwLoc rw }
     return a

withLoc :: HasSrcLoc a => a -> (a -> TC b) -> TC b
withLoc loc f = addLoc loc (f loc)

askLoc :: TC SrcLoc
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
       Left err       -> record (UnifyError rwLoc err)

-- | Remove type variables from a type.
zonk :: Unify.Zonk ty => ty -> TC ty
zonk ty =
  do RW { .. } <- TC get
     case Unify.zonk ty rwSubst of
       Right ty' -> return ty'
       Left  err -> failWith (UnifyError rwLoc err)

-- | Compute free variables.
ftvs :: Unify.Zonk ty => ty -> TC (Set.Set TVar)
ftvs ty =
  do RW { .. } <- TC get
     case Unify.ftvs ty rwSubst of
       Right fvs -> return fvs
       Left err  -> failWith (UnifyError rwLoc err)


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


-- New synthetic values --------------------------------------------------------

newVarPair :: TC (Name, Name)
newVarPair = TC $ sets $ \ RW { .. } ->
  let (nameIn, sup') = mkName (Generated "pattern specification") "s_in" Nothing rwSupply
      (nameOut, sup'') = mkName (Generated "pattern specification") "s_out" Nothing sup'
   in ((nameIn, nameOut), RW { rwSupply = sup'', rwTmps = (nameIn, nameOut) : rwTmps, .. })

getVars :: TC ([Name], [Name])
getVars = TC $ sets $ \ RW { .. } ->
  (unzip rwTmps, RW { rwTmps = [], .. })

-- Errors ----------------------------------------------------------------------

record :: TCError -> TC ()
record err = addErrs [err]

addErrs :: [TCError] -> TC ()
addErrs errs = TC (sets_ (\rw -> rw { rwErrs = errs ++ rwErrs rw }))

getErrs :: TC [TCError]
getErrs  = TC (sets (\rw -> (rwErrs rw, rw { rwErrs = [] })))

setErrs :: [TCError] -> TC ()
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
     raise (err : rwErrs rw)

try :: TC a -> TC (Either [TCError] a)
try m = TC (MonadLib.try (unTC m))

tryAll :: [TC a] -> TC [a]
tryAll ms = TC $
  do es <- mapM (MonadLib.try . unTC) ms
     let (as,bs) = partitionEithers es
     unless (null as) (raise (concat as))
     return bs


-- | Collect any errors that get recorded when running the given action.
-- Failures will still propagate through.
collectErrors :: TC a -> TC (a,[TCError])
collectErrors m =
  do errs  <- getErrs
     a     <- m
     errs' <- getErrs
     setErrs errs
     return (a,errs')

-- | A recursive group that does not consist of just functions.
invalidRecursiveGroup :: [AST.TopDecl Renamed] -> TCError
invalidRecursiveGroup g = InvalidRecursiveGroup g

-- | Too many default cases in a case expression.
tooManyDefaultCases :: [AST.Case Renamed] -> TCError
tooManyDefaultCases arms = TooManyDefaultCases arms
