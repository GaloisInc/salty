{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.AST where

import Scope.Name (Name)

import Data.Function (on)
import Language.Slugs.Lens


data TVar = TVar { tvOrigin :: !(Maybe Name)
                 , tvUnique :: !Int
                 } deriving (Show)

instance Eq TVar where
  (==) = (==) `on` tvUnique
  (/=) = (/=) `on` tvUnique

instance Ord TVar where
  compare = compare `on` tvUnique

data Type = TFree TVar
          | TBool
          | TInt
          | TEnum Name
          | TFun Type Type
            deriving (Eq,Ord,Show)

data Controller = Controller { cName        :: !Name
                             , cInputs      :: [StateVar]
                             , cOutputs     :: [StateVar]
                             , cEnvTrans    :: Expr
                             , cEnvLiveness :: Expr
                             , cSysTrans    :: Expr
                             , cSysLiveness :: Expr
                             , cFuns        :: [Group Fun]
                             , cEnums       :: [EnumDef]
                             } deriving (Show)

emptyController :: Name -> Controller
emptyController cName =
  Controller { cName
             , cInputs      = []
             , cOutputs     = []
             , cEnvTrans    = ETrue
             , cEnvLiveness = ETrue
             , cSysTrans    = ETrue
             , cSysLiveness = ETrue
             , cFuns        = []
             , cEnums       = [] }

data Group a = NonRecursive a
             | Recursive [a]
               deriving (Functor,Foldable,Traversable,Show)

data EnumDef = EnumDef { eName :: !Name
                       , eCons :: [Name]
                       } deriving (Show)

data StateVar = StateVar { svName   :: !Name
                         , svType   :: !Type
                         , svBounds :: !(Maybe (Int,Int))
                           -- ^ When the state var is of type TInt, the bounds
                           -- are the min and max values that can appear.
                         , svInit   :: !(Maybe Expr)
                         } deriving (Show)

data Fun = Fun { fName   :: !Name
               , fParams :: [(Name,Type)]
               , fResult :: Type
               , fBody   :: Expr
               } deriving (Show)

data Expr = ETrue
          | EFalse
          | EVar Name
          | ECon Name
          | ENum Integer
          | EApp Expr Expr
          | EAnd Expr Expr
          | EOr  Expr Expr
          | ENot Expr
          | ENext Expr
          | EEq Expr Expr
            -- ^ Coerce from a state var to a value
            deriving (Show,Eq,Ord)


destTFun :: Type -> ([Type],Type)
destTFun (TFun l r) =
  let (args,res) = destTFun r
   in (l:args,res)
destTFun ty = ([],ty)

destEApp :: Expr -> (Expr,[Expr])
destEApp = go []
  where
  go acc (EApp f x) = go (x:acc) f
  go acc e          = (e, acc)


eAnd :: [Expr] -> Expr
eAnd  = foldl1 EAnd

eNot :: Expr -> Expr
eNot  = ENot

eOr :: [Expr] -> Expr
eOr  = foldl1 EOr

eImp :: Expr -> Expr -> Expr
eImp a b = eOr [ eNot a, b ]

eIf :: Expr -> Expr -> Expr -> Expr
eIf p t f = eAnd [ eImp p t, eImp (eNot p) f ]


-- Traversals ------------------------------------------------------------------

traverseExpr :: Traversal' Expr Expr
traverseExpr _ ETrue      = pure ETrue
traverseExpr _ EFalse     = pure EFalse
traverseExpr _ e@EVar{}   = pure e
traverseExpr _ e@ECon{}   = pure e
traverseExpr _ e@ENum{}   = pure e
traverseExpr f (EApp a b) = EApp  <$> f a <*> f b
traverseExpr f (EAnd a b) = EAnd  <$> f a <*> f b
traverseExpr f (EOr  a b) = EOr   <$> f a <*> f b
traverseExpr f (ENot a)   = ENot  <$> f a
traverseExpr f (ENext a)  = ENext <$> f a
traverseExpr f (EEq a b)  = EEq   <$> f a <*> f b
