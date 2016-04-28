{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module TypeCheck.AST where

import Data.Function (on)


data TVar = TVar { tvHint   :: !(Maybe String)
                 , tvUnique :: !Int
                 } deriving (Show)

instance Eq TVar where
  (==) = (==) `on` tvUnique
  (/=) = (/=) `on` tvUnique

instance Ord TVar where
  compare = compare `on` tvUnique

data Type = TFree TVar
          | TBool
          | TEnum String
          | TFun Type Type
            deriving (Eq,Ord,Show)

data Controller = Controller { cName        :: !Name
                             , cEnvTrans    :: Expr
                             , cEnvLiveness :: Expr
                             , cSysTrans    :: Expr
                             , cSysLiveness :: Expr
                             , cFuns        :: [Group Fun]
                             , cEnums       :: [Enum]
                             } deriving (Show)

data Group a = NonRecursive a
             | Recursive [a]
               deriving (Functor,Foldable,Traversable,Show)

data Enum = Enum { eName :: !Name
                 , eCons :: [Name]
                 } deriving (Show)
