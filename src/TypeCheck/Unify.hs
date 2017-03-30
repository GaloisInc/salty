{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeCheck.Unify (
    Env(), emptyEnv,
    UnifyError(..),
    Types(),
    Unify, unify,
    Zonk, zonk, ftvs,
  ) where

import PP
import SrcLoc
import TypeCheck.AST

import           Control.Monad (zipWithM_)
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           MonadLib


-- External Interface ----------------------------------------------------------

-- | Unify these two types, and update the environment.
unify :: Types ty => ty -> ty -> Env -> Either UnifyError Env
unify a b = runUnify_ (unify' a b)

-- | Remove type variables from this type.
zonk :: Zonk ty => ty -> Env -> Either UnifyError ty
zonk a env =
  case runUnify (zonk' a) env of
    Right (a',_) -> Right a'
    Left err     -> Left err

ftvs :: Zonk ty => ty -> Env -> Either UnifyError (Set.Set TVar)
ftvs a env =
  case runUnify (ftvs' a) env of
    Right (a',_) -> Right a'
    Left err     -> Left err

data UnifyError = UnifyError Type Type
                  -- ^ Unification failed -- these two types don't unify.

                | OccursCheckFailure TVar Type
                  -- ^ The occurs check failed.
                  deriving (Show)

instance PP UnifyError where

  ppPrec _ (UnifyError a b) =
    hang (text "Unable to unify the types:")
       2 (bullets (map pp [a,b]))

  ppPrec _ (OccursCheckFailure var ty) =
    hang (text "Cannot construct the infinite type:")
       2 (pp var <+> char '=' <+> pp ty)


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


class Zonk ty where
  -- | Remove type variables by looking them up in the environment.
  zonk' :: ty -> Unify ty

  -- | Free type variables.
  ftvs' :: ty -> Unify (Set.Set TVar)

instance Zonk SrcLoc where
  zonk' = pure
  {-# INLINE zonk' #-}

  ftvs' = \_ -> pure Set.empty
  {-# INLINE ftvs' #-}

instance (Zonk a, Zonk b) => Zonk (a,b) where
  zonk' (a,b) = (,)       <$> zonk' a <*> zonk' b
  ftvs' (a,b) = Set.union <$> ftvs' a <*> ftvs' b

instance Zonk a => Zonk (Maybe a) where
  zonk' = traverse zonk'

  ftvs' Nothing  = return Set.empty
  ftvs' (Just a) = ftvs' a

instance Zonk a => Zonk [a] where
  zonk' = traverse zonk'
  ftvs' = F.foldrM (\a b -> Set.union b <$> ftvs' a) Set.empty

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

  ftvs' ETrue           = pure Set.empty
  ftvs' EFalse          = pure Set.empty
  ftvs' EVar{}          = pure Set.empty
  ftvs' ECon{}          = pure Set.empty
  ftvs' ENum{}          = pure Set.empty
  ftvs' (EApp f x)      = Set.union  <$> ftvs' f  <*> ftvs' x
  ftvs' (ESet ty es)    = Set.union  <$> ftvs' ty <*> ftvs' es
  ftvs' (ELet _ ty b e) = Set.unions <$> sequence [ftvs' ty, ftvs' b, ftvs' e]
  ftvs' (EPrim p)       = ftvs' p
  ftvs' (ETApp e ty)    = Set.union  <$> ftvs' e   <*> ftvs' ty

instance Zonk Prim where
  zonk' (PIn   ty) = PIn   <$> zonk' ty
  zonk' (PEq   ty) = PEq   <$> zonk' ty
  zonk' (PNext ty) = PNext <$> zonk' ty
  zonk' ef         = pure ef

  ftvs' (PIn   ty) = ftvs' ty
  ftvs' (PEq   ty) = ftvs' ty
  ftvs' (PNext ty) = ftvs' ty
  ftvs' _          = pure Set.empty

instance Zonk FunBody where
  zonk' (FunSpec s) = FunSpec <$> zonk' s
  zonk' (FunExpr e) = FunExpr <$> zonk' e

  ftvs' (FunSpec s) = ftvs' s
  ftvs' (FunExpr e) = ftvs' e

instance Zonk Spec where
  zonk' Spec { .. } =
    do si <- traverse (traverse zonk') sSysInit
       st <- traverse (traverse zonk') sSysTrans
       sl <- traverse (traverse zonk') sSysLiveness
       ei <- traverse (traverse zonk') sEnvInit
       et <- traverse (traverse zonk') sEnvTrans
       el <- traverse (traverse zonk') sEnvLiveness
       return Spec { sSysInit     = si
                   , sSysTrans    = st
                   , sSysLiveness = sl
                   , sEnvInit     = ei
                   , sEnvTrans    = et
                   , sEnvLiveness = el }

  ftvs' Spec { .. } = Set.unions <$> sequence
    [ ftvs' sSysTrans
    , ftvs' sSysLiveness
    , ftvs' sEnvTrans
    , ftvs' sEnvLiveness ]

instance Zonk Type where
  zonk' t0 = go IntSet.empty t0
    where

    go seen ty@(TFree x) =
      do Env { .. } <- get
         case Map.lookup x envCanon of
           Just ix ->
             case IntMap.lookup ix envVars of
               Just ty' | ix `IntSet.member` seen -> raise (OccursCheckFailure x ty')
                        | otherwise               -> go (IntSet.insert ix seen) ty'

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

  ftvs' (TFree v)  =
    do Env { .. } <- get
       case Map.lookup v envCanon of
         Just ix | Just ty' <- IntMap.lookup ix envVars -> ftvs' ty'
         _                                              -> pure (Set.singleton v)

  ftvs' (TSet ty)  = ftvs' ty
  ftvs' (TFun a b) = Set.union <$> ftvs' a <*> ftvs' b
  ftvs' TGen{}     = pure Set.empty
  ftvs' TBool      = pure Set.empty
  ftvs' TInt       = pure Set.empty
  ftvs' TEnum{}    = pure Set.empty
  ftvs' TSpec      = pure Set.empty


class Zonk ty => Types ty where
  -- | Unify two types, effecting the environment.
  unify' :: ty -> ty -> Unify ()


-- | Bind a variable.
bindVar :: IntSet.IntSet -> TVar -> Type -> Unify ()
bindVar seen var ty =
  do Env { .. } <- get
     case Map.lookup var envCanon of
       Just ix ->
         case IntMap.lookup ix envVars of
           Just ty'
             | ix `IntSet.member` seen -> raise (OccursCheckFailure var ty)
             | otherwise               -> unifyType (IntSet.insert ix seen) ty' ty
           Nothing  -> set $! Env { envVars = IntMap.insert ix ty envVars, .. }

       Nothing ->
         do ix <- nextIx
            env <- get
            set $! env { envVars  = IntMap.insert ix ty envVars
                       , envCanon = Map.insert var ix envCanon }

unifyType :: IntSet.IntSet -> Type -> Type -> Unify ()

unifyType seen (TFree x) y = bindVar seen x y
unifyType seen y (TFree x) = bindVar seen x y

unifyType seen (TFun x1 y1) (TFun x2 y2) =
  do unifyType seen x1 x2
     unifyType seen y1 y2

unifyType seen (TSet a) (TSet b) = unifyType seen a b

unifyType _ TSpec TSpec = return ()
unifyType _ TBool TBool = return ()
unifyType _ TInt  TInt  = return ()

unifyType _ (TEnum x) (TEnum y) | x == y = return ()

unifyType _ x y = raise (UnifyError x y)


instance Types Type where
  unify' = unifyType IntSet.empty

instance Types a => Types [a] where
  unify' as bs | length as == length bs = zipWithM_ unify' as bs
               | otherwise              = fail "Can't unify different length lists"
