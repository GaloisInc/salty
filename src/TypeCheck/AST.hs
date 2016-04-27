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
