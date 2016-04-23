-- vim: ft=haskell

{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -w #-}

module Syntax.Parser (
    parseProgram
  ) where

import Syntax.AST
import Syntax.Lexer

import qualified Data.Text.Lazy as L
import           Text.Location
import           Text.Location.Layout

}

%tokentype { Lexeme }

%token
  IDENT    { (matchIdent    -> Just $$) }
  CONIDENT { (matchConIdent -> Just $$) }
  NUM      { $$ @ Located { locValue = TNum      _ } }

  -- keywords
  'controller' { Located { locValue = TKeyword Kcontroller, locRange = $$ } }
  'where'      { Located { locValue = TKeyword Kwhere,      locRange = $$ } }
  'enum'       { Located { locValue = TKeyword Kenum,       locRange = $$ } }
  '='          { Located { locValue = TKeyword Keq,         locRange = $$ } }
  '|'          { Located { locValue = TKeyword Kpipe,       locRange = $$ } }

  -- layout
  'v{'  { Located { locValue = TVirt VBegin, locRange = $$ } }
  'v;'  { Located { locValue = TVirt VSep,   locRange = $$ } }
  'v}'  { Located { locValue = TVirt VEnd,   locRange = $$ } }


%monad { Either Error }
%error { parseError }

%name program

%%

program :: { Program }
  : 'controller' CONIDENT 'where' layout(top_decl) { Program $4 }

top_decl :: { TopDecl }
  : 'enum' CONIDENT '=' sep1('|', CONIDENT)
    { TDLoc (TDEnum (EnumDef $2 $4) `at` getLoc ($1,$4)) }

layout(p)
  : 'v{' sep('v;', p) 'v}'  { $2 }

sep(p,q)
  : {- empty -}    { []         }
  | sep1_body(p,q) { reverse $1 }

sep1(p,q)
  : sep1_body(p,q) { reverse $1 }

sep1_body(p,q)
  : q                  { [$1]  }
  | sep1_body(p,q) p q { $3:$1 }

{

data Error = LexcialError !(Range FilePath)
           | ParseError !Lexeme
           | UnexpectedEOF
             deriving (Show)

parseProgram :: FilePath -> L.Text -> Either Error Program
parseProgram src bytes = program (lexWithLayout src bytes)

parseError :: [Lexeme] -> Either Error a
parseError toks =
  case toks of
    Located { locValue = TLexicalError, .. }:_ -> Left (LexcialError locRange)
    []                                         -> Left UnexpectedEOF
    tok:_                                      -> Left (ParseError tok)


-- Utilities -------------------------------------------------------------------

matchConIdent :: Lexeme -> Maybe (Loc L.Text)
matchConIdent Located { locValue = TConIdent str, .. } = Just (str `at` locRange)
matchConIdent _                                        = Nothing


matchIdent :: Lexeme -> Maybe (Loc L.Text)
matchIdent Located { locValue = TIdent str, .. } = Just (str `at` locRange)
matchIdent _                                     = Nothing
}
