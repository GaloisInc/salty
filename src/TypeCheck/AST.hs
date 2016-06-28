{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.AST where

import Scope.Name (Name)

import Data.Function (on)


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

data StateVar = StateVar { svName :: !Name
                         , svType :: !Type
                         , svInit :: !(Maybe Expr)
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
            deriving (Show)


destTFun :: Type -> ([Type],Type)
destTFun (TFun l r) =
  let (args,res) = destTFun r
   in (l:args,res)
destTFun ty = ([],ty)

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
