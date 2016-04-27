{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Syntax.AST where

import qualified Data.Text.Lazy as L
import           Text.Location


type Loc = Located FilePath

-- | Parsed names.
type PName = L.Text

data Controller name =
  Controller { cName  :: Loc name
             , cDecls :: [TopDecl name]
             } deriving (Functor,Show)


data TopDecl name = TDEnum (EnumDef name)
                  | TDFun (Fun name)
                  | TDInput (StateVar name)
                  | TDOutput (StateVar name)
                  | TDLoc (Loc (TopDecl name))
                    deriving (Functor,Show)


data EnumDef name = EnumDef { eName :: Loc name
                            , eCons :: [Loc name]
                            } deriving (Functor,Show)


data Fun name = Fun { fName :: Loc name
                    , fParams :: [Loc name]
                    , fBody :: Guard name
                    } deriving (Functor,Show)

data StateVar name = StateVar { svName :: Loc name
                              , svType :: Type name
                              , svInit :: Maybe (Expr name)
                              } deriving (Functor,Show)

data Guard name = GChoose (Guard name) (Guard name)
                | GGuard (Expr name) (Expr name)
                | GExpr (Expr name)
                | GLoc (Loc (Guard name))
                  deriving (Functor,Show)

data Type name = TBool
               | TInt
               | TEnum L.Text
               | TFun (Type name) (Type name)
               | TLoc (Loc (Type name))
                 deriving (Functor,Show)

data Expr name = EVar name
               | ECon name
               | ENum Integer
               | ETrue
               | EFalse
               | EAnd (Expr name) (Expr name)
               | EOr  (Expr name) (Expr name)
               | ENot (Expr name)
               | EIf (Expr name) (Expr name) (Expr name)
               | EApp (Expr name) (Expr name)
                 -- ^ Function application
               | ELoc (Loc (Expr name))
                 deriving (Functor,Show)


-- Location Helpers ------------------------------------------------------------

instance HasLoc (TopDecl name) where
  type LocSource (TopDecl name) = FilePath
  getLoc (TDLoc loc) = getLoc loc
  getLoc _           = mempty

instance HasLoc (EnumDef name) where
  type LocSource (EnumDef name) = FilePath
  getLoc EnumDef { .. } = foldMap getLoc (eName : eCons)

instance HasLoc (Guard name) where
  type LocSource (Guard name) = FilePath
  getLoc (GLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (Expr name) where
  type LocSource (Expr name) = FilePath
  getLoc (ELoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (Type name) where
  type LocSource (Type name) = FilePath
  getLoc (TLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (StateVar name) where
  type LocSource (StateVar name) = FilePath
  getLoc StateVar { .. } = mconcat [ getLoc svName
                                   , getLoc svType
                                   , getLoc svInit ]
