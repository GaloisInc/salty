-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RecordWildCards #-}

module Syntax.Lexer (
    Token(..),
    TokenType(..),
    Keyword(..),
    Virtual(..),
    lexer, Lexeme,
    lexWithLayout,
  ) where

import Panic (panic)

import           Data.Char (ord,isAscii,isSpace)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Read as L
import           Data.Word (Word8)
import           Text.Location
import           Text.Location.Layout

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
"--" .* { emits TLineComment }

"[" @ident "|" { startCode }

"controller" { keyword Kcontroller }
"where"      { keyword Kwhere      }
"input"      { keyword Kinput      }
"output"     { keyword Koutput     }

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
"!"     { keyword Knot }

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

@number   { emits mkTNum }
@ident    { emits TIdent }
@conident { emits TConIdent }

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

type Lexeme = Located FilePath Token

data Token = Token { tokText :: !L.Text
                   , tokType :: !TokenType
                   } deriving (Eq,Show)

data TokenType = TLineComment !L.Text
               | TIdent !L.Text
               | TConIdent !L.Text
               | TLexicalError
               | TNum !Integer
               | TKeyword !Keyword
               | TVirt !Virtual
               | TString !L.Text
               | TCode !L.Text !L.Text
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
isComment Token { tokType = TLineComment{} } = True
isComment _                                  = False

mkTNum :: L.Text -> TokenType
mkTNum txt = TNum $! case L.decimal txt of
                       Right (n,_) -> n
                       Left err    -> error ("mkTNum: " ++ err)

ignoreComments :: [Lexeme] -> [Lexeme]
ignoreComments  = filter (not . isComment . thing)


-- Lexer -----------------------------------------------------------------------

lexWithLayout :: FilePath -> L.Text -> [Lexeme]
lexWithLayout src bytes =
  layout Layout { .. } (ignoreComments (lexer src Nothing bytes))
  where
  beginsLayout Token { tokType = TKeyword Kwhere        } = True
  beginsLayout Token { tokType = TKeyword Kof           } = True
  beginsLayout Token { tokType = TKeyword Ksys_trans    } = True
  beginsLayout Token { tokType = TKeyword Ksys_liveness } = True
  beginsLayout Token { tokType = TKeyword Kenv_trans    } = True
  beginsLayout Token { tokType = TKeyword Kenv_liveness } = True
  beginsLayout _                                          = False

  endsLayout Token { tokType = TKeyword Ksys_trans    } = True
  endsLayout Token { tokType = TKeyword Ksys_liveness } = True
  endsLayout Token { tokType = TKeyword Kenv_trans    } = True
  endsLayout Token { tokType = TKeyword Kenv_liveness } = True
  endsLayout _                                          = False

  start = Token { tokText = L.empty, tokType = TVirt VBegin }
  sep   = Token { tokText = L.empty, tokType = TVirt VSep   }
  end   = Token { tokText = L.empty, tokType = TVirt VEnd   }

lexer :: FilePath -> Maybe Position -> L.Text -> [Lexeme]
lexer src mbPos bytes = go AlexInput { aiPos = start, aiText = bytes } Normal
  where
  start = fromMaybe (Position 1 1) mbPos

  rangeSource = Just src

  go ai st =
    case alexScan ai (modeToInt st) of
      AlexEOF -> []

      AlexError ai' ->
        let (as,bs) = L.break isSpace (aiText ai')
            pos'    = L.foldl' (flip move) (aiPos ai') as
            ai2     = AlexInput { aiPos = pos', aiText = bs }
            loc     = Range { rangeStart = aiPos ai', rangeEnd = pos', .. }
         in (Token { tokText = as, tokType = TLexicalError } `at` loc) : go ai2 Normal

      AlexSkip ai' _ ->
        go ai' st

      AlexToken ai' len act ->
        case act rangeSource len ai st of
          (st',xs) -> xs ++ go ai' st'

data AlexInput = AlexInput { aiPos   :: !Position
                           , aiText  :: !L.Text
                           }

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte AlexInput { .. } =
  do (c,rest) <- L.uncons aiText
     return (byteForChar c, AlexInput { aiText = rest, aiPos = move c aiPos, .. })


data Mode = Normal
          | InString Position [L.Text]
          | InCode Position L.Text [L.Text]
            deriving (Show)

modeToInt :: Mode -> Int
modeToInt Normal     = 0
modeToInt InString{} = string
modeToInt InCode{}   = code


-- Actions ---------------------------------------------------------------------

type AlexAction = Maybe FilePath -> Int -> AlexInput -> Mode -> (Mode,[Lexeme])

move :: Char -> Position -> Position
move  = movePos 8

withInput :: (L.Text -> TokenType) -> Maybe FilePath -> Int -> AlexInput
          -> Lexeme
withInput mk rangeSource len AlexInput { .. } =
  Token { .. } `at` Range { rangeStart = aiPos
                          , rangeEnd   = L.foldl' (flip move) aiPos tokText
                          , .. }
  where
  tokText = L.take (fromIntegral len) aiText
  tokType = mk tokText

emits :: (L.Text -> TokenType) -> AlexAction
emits mk src len ai st = (st, [withInput mk src len ai])

keyword :: Keyword -> AlexAction
keyword kw src len ai st = (st, [withInput (const (TKeyword kw)) src len ai])

startString :: AlexAction
startString _ _ ai Normal = (InString (aiPos ai) [], [])
startString _ _ _ _ =
  panic "startString: invalid state"


addString :: AlexAction
addString _ len ai (InString start chunks) = (InString start (chunk:chunks), [])
  where
  chunk = L.take (fromIntegral len) (aiText ai)
addString _ _ _ _ =
  panic "addString: invalid state"

endString :: AlexAction
endString rangeSource _ ai (InString rangeStart chunks) =
  (Normal, [Token str (TString str) `at` Range { rangeEnd = aiPos ai, .. }])
  where
  str = L.concat (reverse chunks)
endString _ _ _ _ =
  panic "endString: invalid state"


startCode, addCode, endCode :: AlexAction

startCode _ len ai Normal = (InCode (aiPos ai) ty [], [])
  where
  ty = L.take (fromIntegral len - 2) (L.drop 1 (aiText ai))

startCode _ _ _ _ = panic "startCode: invalid state"

addCode _ len ai (InCode n s chunks) = (InCode n s (chunk:chunks), [])
  where
  chunk = L.take (fromIntegral len) (aiText ai)

addCode _ _ _ _ = panic "addCode: invalid state"

endCode rangeSource len ai (InCode rangeStart n chunks) =
  (Normal, [Token txt (TCode n txt) `at` Range { rangeEnd = aiPos ai, .. }])
  where
  -- remove common leading whitespace from lines
  txt = L.unlines (map (L.drop leadingSpace) ls)

  ls = filter (not . empty) (L.lines (L.concat (reverse chunks)))

  empty str    = L.null str || L.all isSpace str
  leadingSpace = minimum [ L.length (L.takeWhile isSpace line) | line <- ls
                                                               , not (empty line) ]


endCode _ _ _ _ = panic "endCode: invalid state"



-- Utility ---------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | isAscii c = fromIntegral (ord c)
  | otherwise = non_graphic
  where
  non_graphic = 0

}
