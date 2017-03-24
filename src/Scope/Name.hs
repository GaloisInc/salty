{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Scope.Name (
    Name(), nameText, nameOrigin, nameUnique, nameOutText,
    Origin(..),
    Supply(), emptySupply, nextUnique,
    mkName,
  ) where

import SrcLoc

import           Data.Function (on)
import qualified Data.Text as T


-- Names -----------------------------------------------------------------------

data Origin = FromController !SrcLoc
              -- ^ Top-level controller, and its region

            | FromDecl !SrcLoc !Name
              -- ^ Top-level definition region and parent controller

            | FromParam !SrcLoc !Name
              -- ^ Parameter definition region and parent function

            | Generated String
              -- ^ Generated from this pass

              deriving (Show)

instance HasSrcLoc Origin where
  srcLoc (FromController r) = r
  srcLoc (FromDecl r _)     = r
  srcLoc (FromParam r _)    = r
  srcLoc Generated{}        = mempty


data Name = Name { nText   :: !T.Text
                 , nUnique :: !Int
                 , nOrigin :: !Origin
                 , nOutName:: !(Maybe T.Text)
                 } deriving (Show)

instance HasSrcLoc Name where
  srcLoc n = srcLoc (nameOrigin n)

instance Eq Name where
  (==) = (==) `on` nUnique
  (/=) = (/=) `on` nUnique
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance Ord Name where
  compare = compare `on` nUnique
  {-# INLINE compare #-}

nameOrigin :: Name -> Origin
nameOrigin  = nOrigin

nameText :: Name -> T.Text
nameText  = nText

nameUnique :: Name -> Int
nameUnique  = nUnique

nameOutText :: Name -> Maybe T.Text
nameOutText  = nOutName


-- Name Supply -----------------------------------------------------------------

newtype Supply = Supply Int

emptySupply :: Supply
emptySupply  = Supply 0

nextUnique :: Supply -> (Int,Supply)
nextUnique (Supply n) =
  let n' = n + 1
   in n `seq` (n, Supply n')


-- Name Construction -----------------------------------------------------------

mkName :: Origin -> T.Text -> Maybe T.Text -> Supply -> (Name,Supply)
mkName nOrigin nText nOutName s =
  let (nUnique,s') = nextUnique s
   in (Name { .. }, s')
