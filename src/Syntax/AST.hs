{-# LANGUAGE TypeFamilies #-}

module Syntax.AST where

import qualified Data.Text.Lazy as L
import           Text.Location


type Loc = Located FilePath


data Program = Program { progDecls :: [TopDecl]
                       } deriving (Show)


data TopDecl = TDEnum EnumDef
             | TDLoc (Loc TopDecl)
               deriving (Show)


data EnumDef = EnumDef { eName :: Loc L.Text
                       , eCons :: [Loc L.Text]
                       } deriving (Show)


-- Location Helpers ------------------------------------------------------------

instance HasLoc TopDecl where
  type LocSource TopDecl = FilePath
  getLoc (TDLoc loc) = getLoc loc
  getLoc _           = mempty
