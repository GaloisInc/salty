-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Lexer (
    Token(..),
    Keyword(..),
    Virtual(..),
    lexer, Lexeme,
    lexWithLayout,
  ) where

import Panic (panic)

import           AlexTools
import           Data.Char (ord,isAscii,isSpace)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Word (Word8)
import           Text.Layout.OffSides


}

$lower = [a-z]
$upper = [A-Z]
$num   = [0-9]

@number   = $num+
@ident    = [_ $lower] [$lower $upper $num _]*
@conident = $upper [$lower $upper $num _]*

@strPart  = [^\\\"]+

:-

<0> {

-- skip whitespace
$white+ ;

-- single-line comments
"--" .* { token TLineComment }

"[" @ident "|" { startCode }

"controller" { keyword Kcontroller }
"where"      { keyword Kwhere      }
"input"      { keyword Kinput      }
"output"     { keyword Koutput     }

"sys_init"     { keyword Ksys_init     }
"env_init"     { keyword Kenv_init     }
"sys_trans"    { keyword Ksys_trans    }
"env_trans"    { keyword Kenv_trans    }
"sys_liveness" { keyword Ksys_liveness }
"env_liveness" { keyword Kenv_liveness }

"def"   { keyword Kdef  }

"if"    { keyword Kif   }
"then"  { keyword Kthen }
"else"  { keyword Kelse }

"case"  { keyword Kcase }
"of"    { keyword Kof   }

"enum"  { keyword Kenum }
"|"     { keyword Kpipe }

"..."   { keyword Krange }

"any"   { keyword Kany   }
"all"   { keyword Kall   }
"mutex" { keyword Kmutex }

"@"     { keyword Kann }

":"     { keyword Kcolon  }
"="     { keyword Kassign }
"=="    { keyword Keq     }
"<->"   { keyword Kiff    }
"!="    { keyword Kneq    }
"("     { keyword Klparen }
")"     { keyword Krparen }
"{"     { keyword Klbrace }
"}"     { keyword Krbrace }
","     { keyword Kcomma  }

"\/"    { keyword Kor }
"||"    { keyword Kor }
"/\"    { keyword Kand }
"&&"    { keyword Kand }
"^"     { keyword Kxor }
"!"     { keyword Knot }
"+"     { keyword Kplus}

"otherwise" { keyword Kotherwise }

-- Built-in Types
"Bool"  { keyword KBool  }
"Int"   { keyword KInt   }
"->"    { keyword Krarrow }
"<-"    { keyword Klarrow }

-- Built-in constants
"True"  { keyword KTrue  }
"False" { keyword KFalse }

"'"     { keyword Kprime }

\"      { startString }

"["     { keyword Klbracket }
"]"     { keyword Krbracket }

@number   { number }
@ident    { token TIdent }
@conident { token TConIdent }

}

<string> {
@strPart { addString }
\"       { endString }
\\.      { addString }
}

<code> {
[^\|]+  { addCode }
"|]"    { endCode }
.       { addCode }
$white+ { addCode }
}

{


-- Tokens ----------------------------------------------------------------------

data Token = TLineComment
           | TIdent
           | TConIdent
           | TLexicalError
           | TNum !Integer
           | TKeyword !Keyword
           | TVirt !Virtual
           | TString !T.Text
           | TCode !T.Text !T.Text
             deriving (Eq,Show)

data Keyword = Kif
             | Kthen
             | Kelse
             | Kcase
             | Kof
             | Keq
             | Kassign
             | Kneq
             | Klparen
             | Krparen
             | Klbrace
             | Krbrace
             | Klbracket
             | Krbracket
             | Kprime
             | Kenum
             | Kpipe
             | Krange
             | Kcontroller
             | Kwhere
             | Kcomma
             | Kotherwise
             | Kiff
             | Kor
             | Kxor
             | Kplus
             | Kand
             | Knot
             | Kany
             | Kall
             | Kmutex
             | Kinput
             | Kin
             | Koutput
             | KBool
             | KInt
             | Kcolon
             | Klarrow
             | Krarrow
             | KTrue
             | KFalse
             | Kdef
             | Ksys_init
             | Kenv_init
             | Ksys_trans
             | Kenv_trans
             | Kenv_liveness
             | Ksys_liveness

             | Kann
               deriving (Eq,Show)

data Virtual = VBegin
             | VSep
             | VEnd
               deriving (Eq,Show)

isComment :: Token -> Bool
isComment TLineComment{} = True
isComment _              = False

ignoreComments :: [Lexeme Token] -> [Lexeme Token]
ignoreComments  = filter (not . isComment . lexemeToken)


-- Lexer -----------------------------------------------------------------------

lexWithLayout :: FilePath -> T.Text -> [Lexeme Token]
lexWithLayout src bytes =
  layout Layout { .. } (ignoreComments (lexer src Nothing bytes))
  where
  beginsLayout (TKeyword Kwhere        ) = True
  beginsLayout (TKeyword Kof           ) = True
  beginsLayout (TKeyword Ksys_init     ) = True
  beginsLayout (TKeyword Ksys_trans    ) = True
  beginsLayout (TKeyword Ksys_liveness ) = True
  beginsLayout (TKeyword Kenv_init     ) = True
  beginsLayout (TKeyword Kenv_trans    ) = True
  beginsLayout (TKeyword Kenv_liveness ) = True
  beginsLayout _                         = False

  endsLayout (TKeyword Ksys_init     ) = True
  endsLayout (TKeyword Ksys_trans    ) = True
  endsLayout (TKeyword Ksys_liveness ) = True
  endsLayout (TKeyword Kenv_init     ) = True
  endsLayout (TKeyword Kenv_trans    ) = True
  endsLayout (TKeyword Kenv_liveness ) = True
  endsLayout _                         = False

  start = wrapToken (TVirt VBegin)
  sep   = wrapToken (TVirt VSep)
  end   = wrapToken (TVirt VEnd)

data LexerState = LexerState { lsMode   :: !Mode
                             , lsSource :: !T.Text
                             }

mkConfig :: FilePath -> LexerConfig LexerState Token
mkConfig src =
  LexerConfig { lexerInitialState = LexerState { lsMode = Normal
                                               , lsSource = T.pack src
                                               }
              , lexerStateMode    = modeToInt
              , lexerEOF          = \_ -> [] }

lexer :: FilePath -> Maybe SourcePos -> T.Text -> [Lexeme Token]
lexer src mbPos bytes = $makeLexer (mkConfig src) input
  where
  input = case mbPos of
            Just start -> (initialInput bytes) { inputPos = start }
            Nothing    ->  initialInput bytes

alexGetByte = makeAlexGetByte $ \ c ->
  if isAscii c
     then toEnum (fromEnum c)
     else 0x1


data Mode = Normal
          | InString !SourcePos [T.Text] [T.Text]
          | InCode !SourcePos T.Text [T.Text] [T.Text]
            deriving (Show)

modeToInt :: LexerState -> Int
modeToInt LexerState { lsMode = Normal     } = 0
modeToInt LexerState { lsMode = InString{} } = string
modeToInt LexerState { lsMode = InCode{}   } = code


-- Actions ---------------------------------------------------------------------

getMode :: Action LexerState Mode
getMode  = lsMode <$> getLexerState

setMode :: Mode -> Action LexerState ()
setMode mode =
  do LexerState { .. } <- getLexerState
     setLexerState $! LexerState { lsMode = mode, .. }

getSource :: Action LexerState T.Text
getSource  = lsSource <$> getLexerState

matchRange' :: Action LexerState SourceRange
matchRange'  =
  do sourceFile <- getSource
     start      <- startInput
     end        <- endInput
     return $! SourceRange { sourceFrom = inputPos start
                           , sourceTo   = inputPos end
                           , .. }

keyword :: Keyword -> Action LexerState [Lexeme Token]
keyword kw = token (TKeyword kw)

number :: Action LexerState [Lexeme Token]
number  =
  do txt <- matchText
     case T.decimal txt of
       Right (n,_) -> token (TNum n)
       Left err    -> error ("number: " ++ err)

token :: Token -> Action LexerState [Lexeme Token]
token lexemeToken =
  do lexemeText  <- matchText
     lexemeRange <- matchRange'
     return [ Lexeme { .. } ]


startString, addString, endString :: Action LexerState [Lexeme Token]

startString  =
  do Normal <- getMode
     txt    <- matchText
     r      <- matchRange'
     setMode (InString (sourceFrom r) [txt] [])
     return []

addString  =
  do InString pos txts acc <- getMode
     chunk                 <- matchText
     setMode (InString pos (chunk:txts) (chunk:acc))
     return []

endString  =
  do InString sourceFrom txts acc <- getMode
     txt                          <- matchText
     r                            <- matchRange'
     sourceFile                   <- getSource
     setMode Normal
     return [ Lexeme { lexemeText  = T.concat (reverse (txt:txts))
                     , lexemeToken = TString (T.concat (reverse acc))
                     , lexemeRange = SourceRange { sourceTo = sourceTo r, .. }
                     } ]


startCode, addCode, endCode :: Action LexerState [Lexeme Token]

startCode =
  do Normal <- getMode
     len    <- matchLength
     txt    <- matchText
     r      <- matchRange'

     let ty = T.take (fromIntegral len - 2) (T.drop 1 txt)
     setMode (InCode (sourceFrom r) ty [txt] [])

     return []

addCode =
  do InCode pos n txts acc <- getMode
     chunk                 <- matchText
     setMode (InCode pos n (chunk:txts) (chunk:acc))
     return []


endCode =
  do InCode sourceFrom n txts acc <- getMode
     chunk                        <- matchText
     r                            <- matchRange'
     sourceFile                   <- getSource
     setMode Normal

     -- remove common leading whitespace from lines
     let txt = T.unlines (map (T.drop leadingSpace) ls)
         ls  = filter (not . empty) (T.lines (T.concat (reverse acc)))

         empty str    = T.null str || T.all isSpace str
         leadingSpace = minimum [ T.length (T.takeWhile isSpace line) | line <- ls
                                                                      , not (empty line) ]

     return [ Lexeme { lexemeText  = T.concat (reverse (chunk:txts))
                     , lexemeToken = TCode n txt
                     , lexemeRange = SourceRange { sourceTo = sourceTo r, .. }
                     } ]


-- Utility ---------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | isAscii c = fromIntegral (ord c)
  | otherwise = non_graphic
  where
  non_graphic = 0

}
