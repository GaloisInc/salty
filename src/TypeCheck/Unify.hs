{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeCheck.Unify (
    Env(), emptyEnv,
    UnifyError(..),
    Types(),
    unify,
    match,
    zonk
  ) where

import TypeCheck.AST
import TypeCheck.PP

import           Control.Monad (zipWithM_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           MonadLib


-- External Interface ----------------------------------------------------------

-- | Unify these two types, and update the environment.
unify :: Types ty => ty -> ty -> Env -> Either UnifyError Env
unify a b = runUnify_ (unify' a b)

-- | Matching unification, allowing variables on the LHS to be bound.
match :: Types ty => ty -> ty -> Env -> Either UnifyError Env
match a b = runUnify_ (match' a b)

-- | Remove type variables from this type.
zonk :: Types ty => ty -> Env -> Either UnifyError ty
zonk a env =
  case runUnify (zonk' a) env of
    Right (a',_) -> Right a'
    Left err     -> Left err

data UnifyError = UnifyError Type Type
                  -- ^ Unification failed -- these two types don't unify.

                | MatchError Type Type
                  -- ^ Matching failed -- can't turn the left into the right.

                | OccursCheckFailure Type
                  -- ^ The occurs check failed during zonking.
                  deriving (Show)

instance PP UnifyError where

  ppPrec _ (UnifyError a b) =
    hang (text "Unable to unify the types:")
       2 (vcat [ char '*' <+> pp a
               , char '*' <+> pp b ])

  ppPrec _ (MatchError a b) =
    hang (text "Types do not match:")
       2 (vcat [ char '*' <+> pp a
               , char '*' <+> pp b ])

  ppPrec _ (OccursCheckFailure ty) =
    hang (text "Cannot construct the infinite type:")
       2 (pp ty)


-- Environment -----------------------------------------------------------------

data Env = Env { envCanon :: !(Map.Map TVar Int)
                 -- ^ Canonical names for type variables

               , envVars :: !(IntMap.IntMap Type)
                 -- ^ Values of type variables

               , envNext :: !Int
                 -- ^ The next canonical name to use
               } deriving (Show)


emptyEnv :: Env
emptyEnv  = Env { envCanon = Map.empty
                , envVars  = IntMap.empty
                , envNext  = 0
                }


-- Unification -----------------------------------------------------------------

type Unify = StateT Env (ExceptionT UnifyError Lift)

-- | Run a 'Unify' action.
runUnify :: Unify a -> Env -> Either UnifyError (a,Env)
runUnify m env = runLift (runExceptionT (runStateT env m))

-- | Run a 'Unify' action, and throw away its result.
runUnify_ :: Unify a -> Env -> Either UnifyError Env
runUnify_ m env =
  case runUnify m env of
    Right (_,env') -> Right env'
    Left  err      -> Left err

-- | Yield the next canonical name.
nextIx :: Unify Int
nextIx  =
  do Env { .. } <- get
     set $! Env { envNext = envNext + 1, .. }
     return envNext

-- | Bind a variable.
bindVar :: TVar -> Type -> Unify ()
bindVar var ty =
  do Env { .. } <- get
     case Map.lookup var envCanon of
       Just ix ->
         case IntMap.lookup ix envVars of
           Just ty' -> unify' ty' ty
           Nothing  -> set $! Env { envVars = IntMap.insert ix ty envVars, .. }

       Nothing ->
         do ix <- nextIx
            env <- get
            set $! env { envVars  = IntMap.insert ix ty envVars
                       , envCanon = Map.insert var ix envCanon }


class Types ty where
  -- | Unify two types, effecting the environment.
  unify' :: ty -> ty -> Unify ()

  -- | Matching unification. Only allow variables on the LHS to bind.
  match' :: ty -> ty -> Unify ()

  -- | Remove type variables by looking them up in the environment.
  zonk' :: ty -> Unify ty


instance Types Type where

  unify' (TFree x) y = bindVar x y
  unify' y (TFree x) = bindVar x y

  unify' (TFun x1 y1) (TFun x2 y2) =
    do unify' x1 x2
       unify' y1 y2

  unify' TBool TBool = return ()
  unify' TInt  TInt  = return ()

  unify' (TEnum x) (TEnum y) | x == y = return ()

  unify' x y = raise (UnifyError x y)


  match' (TFree x) y = bindVar x y
  match' (TFun x1 y1) (TFun x2 y2) =
    do match' x1 x2
       match' y1 y2

  match' TBool TBool = return ()
  match' TInt  TInt  = return ()

  match' (TEnum x) (TEnum y) | x == y = return ()

  match' x y = raise (MatchError x y)


  zonk' t0 = go Set.empty t0
    where

    go seen ty@(TFree x) =
      do Env { .. } <- get
         case Map.lookup x envCanon of
           Just ix ->
             case IntMap.lookup ix envVars of
               Just ty' | ix `Set.member` seen -> raise (OccursCheckFailure t0)
                        | otherwise            -> go (Set.insert ix seen) ty'

               Nothing -> return ty
           Nothing -> return ty

    go seen (TFun a b) =
      do a' <- go seen a
         b' <- go seen b
         return (TFun a' b')

    go _ ty = return ty


instance Types a => Types [a] where
  unify' as bs | length as == length bs = zipWithM_ unify' as bs
               | otherwise              = fail "Can't unify different length lists"

  match' as bs | length as == length bs = zipWithM_ match' as bs
               | otherwise              = fail "Can't match different length lists"

  zonk' as = traverse zonk' as
