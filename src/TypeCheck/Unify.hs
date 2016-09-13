{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeCheck.Unify (
    Env(), emptyEnv,
    UnifyError(..),
    Types(),
    Unify, unify, match,
    Zonk, zonk, ftvs,
  ) where

import PP
import TypeCheck.AST

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
zonk :: Zonk ty => ty -> Env -> Either UnifyError ty
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

class Zonk ty where
  -- | Remove type variables by looking them up in the environment.
  zonk' :: ty -> Unify ty

  -- | Free type variables.
  ftvs :: ty -> Set.Set TVar

instance Zonk a => Zonk (Maybe a) where
  zonk' = traverse zonk'
  ftvs  = maybe Set.empty ftvs

instance Zonk a => Zonk [a] where
  zonk' = traverse zonk'

  ftvs as = Set.unions (map ftvs as)

instance Zonk Expr where
  zonk' ETrue           = pure ETrue
  zonk' EFalse          = pure EFalse
  zonk' (EVar ty v)     = EVar <$> zonk' ty <*> pure v
  zonk' (ECon ty v)     = ECon <$> zonk' ty <*> pure v
  zonk' e@ENum{}        = pure e
  zonk' (EApp f x)      = EApp   <$> zonk' f  <*> zonk' x
  zonk' (ESet ty es)    = ESet   <$> zonk' ty <*> traverse zonk' es
  zonk' (ELet n ty b e) = ELet n <$> zonk' ty <*> zonk' b <*> zonk' e
  zonk' (EPrim p)       = EPrim  <$> zonk' p
  zonk' (ETApp e ty)    = ETApp  <$> zonk' e  <*> zonk' ty

  ftvs ETrue           = Set.empty
  ftvs EFalse          = Set.empty
  ftvs EVar{}          = Set.empty
  ftvs ECon{}          = Set.empty
  ftvs ENum{}          = Set.empty
  ftvs (EApp f x)      = Set.union (ftvs f)  (ftvs x)
  ftvs (ESet ty es)    = Set.union (ftvs ty) (ftvs es)
  ftvs (ELet _ ty b e) = Set.unions [ftvs ty, ftvs b, ftvs e]
  ftvs (EPrim p)       = ftvs p
  ftvs (ETApp e ty)    = Set.union (ftvs e) (ftvs ty)

instance Zonk Prim where
  zonk' (PIn   ty) = PIn   <$> zonk' ty
  zonk' (PEq   ty) = PEq   <$> zonk' ty
  zonk' (PNext ty) = PNext <$> zonk' ty
  zonk' ef         = pure ef

  ftvs (PIn   ty) = ftvs ty
  ftvs (PEq   ty) = ftvs ty
  ftvs (PNext ty) = ftvs ty
  ftvs _          = Set.empty

instance Zonk FunBody where
  zonk' (FunSpec s) = FunSpec <$> zonk' s
  zonk' (FunExpr e) = FunExpr <$> zonk' e

  ftvs (FunSpec s) = ftvs s
  ftvs (FunExpr e) = ftvs e

instance Zonk Spec where
  zonk' Spec { .. } =
    do st <- zonk' sSysTrans
       sl <- zonk' sSysLiveness
       et <- zonk' sEnvTrans
       el <- zonk' sEnvLiveness
       return Spec { sSysTrans    = st
                   , sSysLiveness = sl
                   , sEnvTrans    = et
                   , sEnvLiveness = el }

  ftvs Spec { .. } = Set.unions
    [ ftvs sSysTrans
    , ftvs sSysLiveness
    , ftvs sEnvTrans
    , ftvs sEnvLiveness ]

instance Zonk Type where
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

    go seen (TSet as) =
      do as' <- go seen as
         return (TSet as')

    go _ ty = return ty

  ftvs (TFree v)  = Set.singleton v
  ftvs (TSet ty)  = ftvs ty
  ftvs (TFun a b) = Set.union (ftvs a) (ftvs b)
  ftvs TGen{}     = Set.empty
  ftvs TBool      = Set.empty
  ftvs TInt       = Set.empty
  ftvs TEnum{}    = Set.empty
  ftvs TSpec      = Set.empty


class Zonk ty => Types ty where
  -- | Unify two types, effecting the environment.
  unify' :: ty -> ty -> Unify ()

  -- | Matching unification. Only allow variables on the LHS to bind.
  match' :: ty -> ty -> Unify ()

instance Types Type where

  unify' (TFree x) y = bindVar x y
  unify' y (TFree x) = bindVar x y

  unify' (TFun x1 y1) (TFun x2 y2) =
    do unify' x1 x2
       unify' y1 y2

  unify' (TSet a) (TSet b) = unify' a b

  unify' TSpec TSpec = return ()
  unify' TBool TBool = return ()
  unify' TInt  TInt  = return ()

  unify' (TEnum x) (TEnum y) | x == y = return ()

  unify' x y = raise (UnifyError x y)


  match' (TFree x) y = bindVar x y
  match' (TFun x1 y1) (TFun x2 y2) =
    do match' x1 x2
       match' y1 y2

  match' (TSet a) (TSet b) = match' a b

  match' TSpec TSpec = return ()
  match' TBool TBool = return ()
  match' TInt  TInt  = return ()

  match' (TEnum x) (TEnum y) | x == y = return ()

  match' x y = raise (MatchError x y)

instance Types a => Types [a] where
  unify' as bs | length as == length bs = zipWithM_ unify' as bs
               | otherwise              = fail "Can't unify different length lists"

  match' as bs | length as == length bs = zipWithM_ match' as bs
               | otherwise              = fail "Can't match different length lists"
