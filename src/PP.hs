{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PP (
    Doc,
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
    hang,
    text,
    char,
    integer,
    int,
    (<>),
    ($$),
    Pr.nest,
    Pr.parens,
    Pr.comma,
    Pr.space,
    Pr.braces,
    Pr.hcat,
    Pr.vcat,
    empty,
    fsep,
    Pr.brackets,
    Pr.punctuate,
    doubleQuotes,
    Pr.colon,
    Pr.sep,
    Pr.hsep,
    Pr.semi,
    isEmpty,
    Pr.indent,
    Pr.align,
    Pr.hardline,
    Pr.line,
    Pr.concatWith,
    Pr.surround,

    ppNameWithOrigin,
    ppOrigin,

--    module Text.PrettyPrint.HughesPJ
    module Data.Text.Prettyprint.Doc
  ) where

import           Scope.Name (Origin(..),Name,nameOrigin,nameText,nameUnique)
import           SrcLoc

import           AlexTools (SourceRange(..),SourcePos(..))
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import qualified Data.Text as T
--import           Text.PrettyPrint.HughesPJ hiding (render)
import           Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pr
import qualified Data.Text.Prettyprint.Doc.Internal.Type as Pr

type Doc = Pr.Doc ()

isEmpty :: Doc -> Bool
isEmpty Pr.Empty = True
isEmpty _        = False

doubleQuotes :: Doc -> Doc
doubleQuotes x = Pr.surround x Pr.dquote Pr.dquote

char :: Char -> Doc
char = Pr.pretty

text :: String -> Doc
text = Pr.pretty

integer :: Integer -> Doc
integer = Pr.pretty

int :: Int -> Doc
int = Pr.pretty

hang :: Doc -> Int -> Doc -> Doc
hang x n xs = Pr.hang n (x <+> xs)

($$) :: Doc -> Doc -> Doc
x $$ y = Pr.vsep [x, y]

empty :: Doc
empty = mempty

fsep :: [Doc] -> Doc
fsep = Pr.hsep

render :: PP a => a -> String
render a = show (pp a)

pp :: PP a => a -> Doc
pp  = ppPrec 0

optParens :: Bool -> Doc -> Doc
optParens True  = Pr.parens
optParens False = id

ticks :: Doc -> Doc
ticks d = Pr.pretty '`' <> d <> Pr.pretty '`'

angles :: Doc -> Doc
angles d = Pr.pretty '<' <> d <> Pr.pretty '>'

-- | Join a bunch of lines together with bullet points.
bullets :: [Doc] -> Doc
bullets ds = Pr.vcat [ Pr.pretty '*' <+> Pr.nest 2 d | d <- ds ]

semicolon :: Doc
semicolon  = Pr.pretty ';'

semis :: [Doc] -> [Doc]
semis  = map (<> Pr.semi)

commas :: [Doc] -> [Doc]
commas  = Pr.punctuate Pr.comma

class PP a where
  ppPrec :: Int -> a -> Doc

  ppList :: [a] -> Doc
  ppList as = Pr.brackets (Pr.sep (Pr.punctuate Pr.comma (map pp as)))

instance PP Doc where
  ppPrec _ = id
  {-# INLINE ppPrec #-}

instance PP a => PP [a] where
  ppPrec _ = ppList
  ppList   = ppList

instance PP Int where
  ppPrec _ = Pr.pretty

instance PP Int64 where
  ppPrec _ = Pr.pretty . toInteger

instance PP Char where
  ppPrec _ = Pr.pretty
  ppList   = Pr.pretty

instance PP T.Text where
  ppPrec _ = Pr.pretty

instance PP Name where
  ppPrec _ n = pp (nameText n) <> Pr.pretty '_' <> pp (nameUnique n)

-- | Pretty-print a name with its location.
ppNameWithOrigin :: Name -> Doc
ppNameWithOrigin n =
  pp (nameText n) <+> Pr.pretty "from" <+> ppOrigin (nameOrigin n)

ppOrigin :: Origin -> Doc
ppOrigin (FromController _) = Pr.pretty "controller"
ppOrigin (FromDecl d _)     = Pr.pretty "declaration at" <+> pp d
ppOrigin (FromParam d _)    = Pr.pretty "parameter from" <+> pp d
ppOrigin (Generated p)      = Pr.pretty "fresh name from pass" <+> Pr.pretty p

instance PP Integer where
  ppPrec _ = Pr.pretty

instance PP SourcePos where
  ppPrec _ SourcePos { .. } =
    pp sourceLine <> Pr.pretty ',' <> pp sourceColumn

instance PP SourceRange where
  ppPrec _ SourceRange { .. } =
    pp (sourceFile sourceFrom) <> Pr.pretty ':' <> pp sourceFrom <> Pr.pretty '-' <> pp sourceTo

instance PP SrcLoc where
  ppPrec p (Known r) = ppPrec p r
  ppPrec _ Unknown   = Pr.pretty "<unknown location>"
