{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Syntax.AST where

import PP
import SrcLoc

import           Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.Exts (Constraint)


type family AnnotOf a :: *
type family NameOf  a :: *


data Parsed

type instance AnnotOf Parsed = SrcLoc
type instance NameOf  Parsed = PName


-- | Parsed names that retain their origin. The comparison of the names is done
-- without regard to their source location.
data PName = PName SrcLoc T.Text
             deriving (Show)

instance Eq PName where
  (==) = (==) `on` pnameText
  (/=) = (/=) `on` pnameText

instance Ord PName where
  compare = compare `on` pnameText

pnameText :: PName -> T.Text
pnameText (PName _ t) = t

instance PP PName where
  ppPrec p (PName _ t) = ppPrec p t


data Controller a =
  Controller { cAnnot :: AnnotOf a
             , cName  :: NameOf a
             , cDecls :: [TopDecl a]
             }


data TopDecl a = TDEnum   (AnnotOf a) (EnumDef a)
               | TDFun    (AnnotOf a) (Fun a)
               | TDInput  (AnnotOf a) (StateVar a)
               | TDOutput (AnnotOf a) (StateVar a)
               | TDSpec   (AnnotOf a) (Spec a)
               | TDExpr   (AnnotOf a) (Expr a)

data Spec a = SEnvTrans    (AnnotOf a) [Expr a]
            | SEnvLiveness (AnnotOf a) [Expr a]
            | SSysTrans    (AnnotOf a) [Expr a]
            | SSysLiveness (AnnotOf a) [Expr a]


isFun :: TopDecl a -> Bool
isFun TDFun{}     = True
isFun _           = False


data EnumDef a = EnumDef { eAnnot :: AnnotOf a
                         , eAnn   :: Maybe (Ann a)
                         , eName  :: NameOf a
                         , eCons  :: [(NameOf a, Maybe T.Text)]
                         }

data Fun a = Fun { fAnnot  :: AnnotOf a
                 , fAnn    :: Maybe (Ann a)
                 , fName   :: NameOf a
                 , fParams :: [NameOf a]
                 , fBody   :: FunBody a
                 }

-- | Declaration-level annotation values.
data Ann a = AnnSym  (AnnotOf a) T.Text
           | AnnApp  (AnnotOf a) T.Text [Ann a]
           | AnnArr  (AnnotOf a) [Ann a]
           | AnnObj  (AnnotOf a) [(T.Text,Ann a)]
           | AnnStr  (AnnotOf a) T.Text
           | AnnCode (AnnotOf a) T.Text T.Text


data Bounds a = Bounds { bAnnot :: AnnotOf a
                       , bLow, bHigh :: !Int
                       }

data StateVar a = StateVar { svAnnot  :: AnnotOf a
                           , svAnn    :: Maybe (Ann a)
                           , svName   :: NameOf a
                           , svType   :: Type a
                           , svBounds :: Maybe (Bounds a)
                           , svInit   :: Maybe (Expr a)
                           , svOutName:: Maybe T.Text
                           }

data FunBody a = FBSpec (AnnotOf a) [Spec a]
               | FBExpr (AnnotOf a) (Expr a)

data Type a = TBool (AnnotOf a)
            | TInt  (AnnotOf a)
            | TEnum (AnnotOf a) (NameOf a)
            | TFun  (AnnotOf a) (Type a) (Type a)

data Expr a = EVar   (AnnotOf a) (NameOf a)
            | ECon   (AnnotOf a) (NameOf a)
            | ENum   (AnnotOf a) Integer
            | ETrue  (AnnotOf a)
            | EFalse (AnnotOf a)
            | EAnd   (AnnotOf a) (Expr a) (Expr a)
            | EOr    (AnnotOf a) (Expr a) (Expr a)
            | ENot   (AnnotOf a) (Expr a)
            | EIf    (AnnotOf a) (Expr a) (Expr a) (Expr a)
            | ECase  (AnnotOf a) (Expr a) [Case a]
            | EApp   (AnnotOf a) (Expr a) (Expr a)
              -- ^ Function application
            | ENext  (AnnotOf a) (Expr a)
            | EEq    (AnnotOf a) (Expr a) (Expr a)
            | ENeq   (AnnotOf a) (Expr a) (Expr a)
            | EImp   (AnnotOf a) (Expr a) (Expr a)
            | EIff   (AnnotOf a) (Expr a) (Expr a)

            | ESet   (AnnotOf a) [Expr a]
            | EIn    (AnnotOf a) (Expr a) (Expr a)
              -- ^ @ x <- { a, b, c } @

            | EAny   (AnnotOf a)
            | EAll   (AnnotOf a)
            | EMutex (AnnotOf a)

data Case a = CPat     (AnnotOf a) (Pat a) (Expr a)
            | CDefault (AnnotOf a) (Expr a)

data Pat a = PCon (AnnotOf a) (NameOf a)
           | PNum (AnnotOf a) Integer


-- Location information --------------------------------------------------------

instance HasSrcLoc PName where
  srcLoc (PName loc _) = loc

annLoc :: (HasAnnot f, HasSrcLoc (AnnotOf a)) => f a -> SrcLoc
annLoc x = srcLoc (ann x)

instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Controller a) where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (TopDecl a)    where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Spec a)       where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (EnumDef a)    where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Fun a)        where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Ann a)        where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Bounds a)     where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (StateVar a)   where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (FunBody a)    where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Type a)       where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Expr a)       where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Case a)       where srcLoc = annLoc
instance (HasSrcLoc (AnnotOf a)) => HasSrcLoc (Pat a)        where srcLoc = annLoc


-- Show Instances --------------------------------------------------------------

type PCtx (f :: * -> Constraint) a = (f (AnnotOf a), f (NameOf a))

deriving instance PCtx Show a => Show (Controller a)
deriving instance PCtx Show a => Show (TopDecl a)
deriving instance PCtx Show a => Show (Spec a)
deriving instance PCtx Show a => Show (EnumDef a)
deriving instance PCtx Show a => Show (Fun a)
deriving instance PCtx Show a => Show (Ann a)
deriving instance PCtx Show a => Show (Bounds a)
deriving instance PCtx Show a => Show (StateVar a)
deriving instance PCtx Show a => Show (FunBody a)
deriving instance PCtx Show a => Show (Type a)
deriving instance PCtx Show a => Show (Expr a)
deriving instance PCtx Show a => Show (Case a)
deriving instance PCtx Show a => Show (Pat a)

-- Annotations -----------------------------------------------------------------

class HasAnnot f where
  ann :: f a -> AnnotOf a

instance HasAnnot Controller where ann = cAnnot
instance HasAnnot EnumDef    where ann = eAnnot
instance HasAnnot Fun        where ann = fAnnot
instance HasAnnot StateVar   where ann = svAnnot
instance HasAnnot Bounds     where ann = bAnnot

instance HasAnnot TopDecl where
  ann (TDEnum   a _) = a
  ann (TDFun    a _) = a
  ann (TDInput  a _) = a
  ann (TDOutput a _) = a
  ann (TDSpec   a _) = a
  ann (TDExpr   a _) = a

instance HasAnnot Spec where
  ann (SEnvTrans    a _) = a
  ann (SEnvLiveness a _) = a
  ann (SSysTrans    a _) = a
  ann (SSysLiveness a _) = a

instance HasAnnot Ann where
  ann (AnnSym  a _)   = a
  ann (AnnApp  a _ _) = a
  ann (AnnArr  a _)   = a
  ann (AnnObj  a _)   = a
  ann (AnnStr  a _)   = a
  ann (AnnCode a _ _) = a

instance HasAnnot FunBody where
  ann (FBSpec a _) = a
  ann (FBExpr a _) = a

instance HasAnnot Type where
  ann (TBool a)     = a
  ann (TInt  a)     = a
  ann (TEnum a _)   = a
  ann (TFun  a _ _) = a

instance HasAnnot Expr where
  ann (EVar   a _)     = a
  ann (ECon   a _)     = a
  ann (ENum   a _)     = a
  ann (ETrue  a)       = a
  ann (EFalse a)       = a
  ann (EAnd   a _ _)   = a
  ann (EOr    a _ _)   = a
  ann (ENot   a _)     = a
  ann (EIf    a _ _ _) = a
  ann (ECase  a _ _)   = a
  ann (EApp   a _ _)   = a
  ann (ENext  a _)     = a
  ann (EEq    a _ _)   = a
  ann (ENeq   a _ _)   = a
  ann (EImp   a _ _)   = a
  ann (EIff   a _ _)   = a
  ann (ESet   a _)     = a
  ann (EIn    a _ _)   = a
  ann (EAny   a)       = a
  ann (EAll   a)       = a
  ann (EMutex a)       = a

instance HasAnnot Case where
  ann (CPat     a _ _) = a
  ann (CDefault a _)   = a

instance HasAnnot Pat where
  ann (PCon a _) = a
  ann (PNum a _) = a


-- Name Functions --------------------------------------------------------------

-- | Give the value-level names introduced by this declaration.
topDeclDs :: Ord (NameOf a) => TopDecl a -> Set.Set (NameOf a)
topDeclDs (TDEnum _ enum) = enumDs enum
topDeclDs (TDFun _ fun)   = funDs fun
topDeclDs (TDInput _ sv)  = stateVarDs sv
topDeclDs (TDOutput _ sv) = stateVarDs sv
topDeclDs TDSpec{}        = Set.empty
topDeclDs TDExpr{}        = Set.empty


-- | The names of all the constructors.
enumDs :: Ord (NameOf a) => EnumDef a -> Set.Set (NameOf a)
enumDs EnumDef { eCons } = Set.fromList [ con | (con,_) <- eCons ]


-- | The name of this function.
funDs :: Fun a -> Set.Set (NameOf a)
funDs Fun { fName } = Set.singleton fName


-- | The name of this state var
stateVarDs :: StateVar a -> Set.Set (NameOf a)
stateVarDs StateVar { svName } = Set.singleton svName


-- | Free variables in this top-level declaration.
topDeclFvs :: Ord (NameOf a) => TopDecl a -> Set.Set (NameOf a)
topDeclFvs TDEnum {}        = Set.empty
topDeclFvs (TDFun    _ fun) = funFvs fun
topDeclFvs (TDInput  _ sv)  = stateVarFvs sv
topDeclFvs (TDOutput _ sv)  = stateVarFvs sv
topDeclFvs (TDSpec   _ s)   = specFvs s
topDeclFvs (TDExpr   _ s)   = exprFvs s

-- | Free variables used in this specification.
specFvs :: Ord (NameOf a) => Spec a -> Set.Set (NameOf a)
specFvs (SSysTrans    _ es) = foldMap exprFvs es
specFvs (SSysLiveness _ es) = foldMap exprFvs es
specFvs (SEnvTrans    _ es) = foldMap exprFvs es
specFvs (SEnvLiveness _ es) = foldMap exprFvs es

-- | The free variables of the function body.
funFvs :: Ord (NameOf a) => Fun a -> Set.Set (NameOf a)
funFvs Fun { fParams, fBody } = 
  let fvs = funBodyFvs fBody
      bvs = Set.fromList fParams
   in fvs Set.\\ bvs

funBodyFvs :: Ord (NameOf a) => FunBody a -> Set.Set (NameOf a)
funBodyFvs (FBSpec _ ps) = foldMap specFvs ps
funBodyFvs (FBExpr _ e)  = exprFvs e

-- | The free variables of the state-variable initializer.
stateVarFvs :: Ord (NameOf a) => StateVar a -> Set.Set (NameOf a)
stateVarFvs StateVar { svName, svInit } =
  let fvs = maybe Set.empty exprFvs svInit
   in Set.delete svName fvs

-- | The names that appear free in this expression.
exprFvs :: Ord (NameOf a) => Expr a -> Set.Set (NameOf a)
exprFvs (EVar _ name)  = Set.singleton name
exprFvs (ECon _ name)  = Set.singleton name
exprFvs ENum{}         = Set.empty
exprFvs ETrue{}        = Set.empty
exprFvs EFalse{}       = Set.empty
exprFvs (EAnd _ l r)   = Set.union (exprFvs l) (exprFvs r)
exprFvs (EOr  _ l r)   = Set.union (exprFvs l) (exprFvs r)
exprFvs (ENot _ e)     = exprFvs e
exprFvs (EIf _ a b c)  = Set.unions [ exprFvs a, exprFvs b, exprFvs c ]
exprFvs (EApp _ f x)   = Set.union (exprFvs f) (exprFvs x)
exprFvs (ENext _ e)    = exprFvs e
exprFvs (EEq _ a b)    = Set.union (exprFvs a) (exprFvs b)
exprFvs (ENeq _ a b)   = Set.union (exprFvs a) (exprFvs b)
exprFvs (EImp _ a b)   = Set.union (exprFvs a) (exprFvs b)
exprFvs (EIff _ a b)   = Set.union (exprFvs a) (exprFvs b)
exprFvs (ECase _ e gs) = Set.union (exprFvs e) (foldMap caseFvs gs)
exprFvs (ESet _ es)    = Set.unions (map exprFvs es)
exprFvs (EIn _ a b)    = Set.union (exprFvs a) (exprFvs b)
exprFvs EAny{}         = Set.empty
exprFvs EAll{}         = Set.empty
exprFvs EMutex{}       = Set.empty

caseFvs :: Ord (NameOf a) => Case a -> Set.Set (NameOf a)
caseFvs (CPat     _ p e) = Set.union (patFvs p) (exprFvs e)
caseFvs (CDefault _ e)   = exprFvs e

patFvs :: Pat a -> Set.Set (NameOf a)
patFvs (PCon _ p)   = Set.singleton p
patFvs (PNum _ _)   = Set.empty
