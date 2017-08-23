{-# LANGUAGE RecordWildCards #-}

module PP (
    PP(..),
    pp,
    render,
    optParens,
    ticks,
    angles,
    semicolon,
    bullets,
    commas,
    semis,

    ppNameWithOrigin,
    ppOrigin,

    module Text.PrettyPrint.HughesPJ
  ) where

import           Scope.Name (Origin(..),Name,nameOrigin,nameText,nameUnique)
import           SrcLoc

import           AlexTools (SourceRange(..),SourcePos(..))
import           Data.Int (Int64)
import qualified Data.Text as T
import           Text.PrettyPrint.HughesPJ hiding (render)

render :: PP a => a -> String
render a = show (pp a)

pp :: PP a => a -> Doc
pp  = ppPrec 0

optParens :: Bool -> Doc -> Doc
optParens True  = parens
optParens False = id

ticks :: Doc -> Doc
ticks d = char '`' <> d <> char '`'

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

-- | Join a bunch of lines together with bullet points.
bullets :: [Doc] -> Doc
bullets ds = vcat [ char '*' <+> nest 2 d | d <- ds ]

semicolon :: Doc
semicolon  = char ';'

semis :: [Doc] -> [Doc]
semis  = map (<> semi)

commas :: [Doc] -> [Doc]
commas  = punctuate comma

class PP a where
  ppPrec :: Int -> a -> Doc

  ppList :: [a] -> Doc
  ppList as = brackets (fsep (punctuate comma (map pp as)))

instance PP Doc where
  ppPrec _ = id
  {-# INLINE ppPrec #-}

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

instance PP T.Text where
  ppPrec _ = text . T.unpack

instance PP Name where
  ppPrec _ n = pp (nameText n) <> char '_' <> pp (nameUnique n)

-- | Pretty-print a name with its location.
ppNameWithOrigin :: Name -> Doc
ppNameWithOrigin n =
  pp (nameText n) <+> text "from" <+> ppOrigin (nameOrigin n)

ppOrigin :: Origin -> Doc
ppOrigin (FromController _) = text "controller"
ppOrigin (FromDecl d _)     = text "declaration at" <+> pp d
ppOrigin (FromParam d _)    = text "parameter from" <+> pp d
ppOrigin (Generated p)      = text "fresh name from pass" <+> text p

instance PP Integer where
  ppPrec _ = integer

instance PP SourcePos where
  ppPrec _ SourcePos { .. } =
    pp sourceLine <> char ',' <> pp sourceColumn

instance PP SourceRange where
  ppPrec _ SourceRange { .. } =
    pp (sourceFile sourceFrom) <> char ':' <> pp sourceFrom <> char '-' <> pp sourceTo

instance PP SrcLoc where
  ppPrec p (Known r) = ppPrec p r
  ppPrec _ Unknown   = text "<unknown location>"
