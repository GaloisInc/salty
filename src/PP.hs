{-# LANGUAGE RecordWildCards #-}

module PP (
    PP(..),
    pp,
    optParens,
    ticks,
    semicolon,

    module Text.PrettyPrint.HughesPJ
  ) where

import           Scope.Name (Name,nameText)

import           Data.Int (Int64)
import qualified Data.Text.Lazy as L
import           Text.PrettyPrint.HughesPJ
import           Text.Location

render :: PP a => a -> String
render a = show (pp a)

pp :: PP a => a -> Doc
pp  = ppPrec 0

optParens :: Bool -> Doc -> Doc
optParens True  = parens
optParens False = id

ticks :: Doc -> Doc
ticks d = char '`' <> d <> char '`'

semicolon :: Doc
semicolon  = char ';'

class PP a where
  ppPrec :: Int -> a -> Doc

  ppList :: [a] -> Doc
  ppList as = brackets (fsep (punctuate comma (map pp as)))

instance PP a => PP [a] where
  ppPrec _ = ppList
  ppList   = ppList

instance PP Int where
  ppPrec _ = int

instance PP Int64 where
  ppPrec _ = integer . toInteger

instance PP Char where
  ppPrec _ = char
  ppList   = text

instance PP L.Text where
  ppPrec _ = text . L.unpack

instance PP Name where
  ppPrec _ = pp . nameText

instance PP Integer where
  ppPrec _ = integer

instance PP Position where
  ppPrec _ Position { .. } =
    pp posRow <> char ',' <> pp posCol

instance PP src => PP (Range src) where
  ppPrec _ Range { .. } =
    maybe (text "<no location>") pp rangeSource <> char ':'
    <> pp rangeStart <> char '-' <> pp rangeEnd
