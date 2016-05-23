-- vim: ft=haskell

{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -w #-}

module Syntax.Parser (
    parseController
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
  NUM      { (matchNum      -> Just $$) }

  -- types
  'Bool'       { Keyword KBool       $$ }
  'Int'        { Keyword KInt        $$ }
  '->'         { Keyword Karrow      $$ }

  -- constants
  'True'       { Keyword KTrue       $$ }
  'False'      { Keyword KFalse      $$ }

  -- keywords
  'controller' { Keyword Kcontroller $$ }
  'where'      { Keyword Kwhere      $$ }
  'enum'       { Keyword Kenum       $$ }
  'otherwise'  { Keyword Kotherwise  $$ }
  'input'      { Keyword Kinput      $$ }
  'output'     { Keyword Koutput     $$ }
  'if'         { Keyword Kif         $$ }
  'then'       { Keyword Kthen       $$ }
  'else'       { Keyword Kelse       $$ }
  '='          { Keyword Keq         $$ }
  '|'          { Keyword Kpipe       $$ }
  '('          { Keyword Klparen     $$ }
  ')'          { Keyword Krparen     $$ }
  ','          { Keyword Kcomma      $$ }
  ':'          { Keyword Kcolon      $$ }

  'sys_liveness' { Keyword Ksys_liveness $$ }
  'env_liveness' { Keyword Ksys_liveness $$ }
  'sys_trans'    { Keyword Ksys_trans    $$ }
  'env_trans'    { Keyword Ksys_trans    $$ }

  '&&'         { Keyword Kand        $$ }
  '||'         { Keyword Kor         $$ }
  '!'          { Keyword Knot        $$ }

  -- layout
  'v{'  { Virt VBegin $$ }
  'v;'  { Virt VSep   $$ }
  'v}'  { Virt VEnd   $$ }


%right '||'
%right '&&'
%right NOT
%right '->'


%monad { Either Error }
%error { parseError }

%name controller

%%

controller :: { Controller PName }
  : 'controller' CONIDENT 'where' layout(top_decl)
    { Controller $2 $4 }

top_decl :: { TopDecl PName }

  : 'enum' CONIDENT '=' sep1('|', CONIDENT)
    { TDLoc (TDEnum (EnumDef $2 $4) `at` getLoc ($1,$4)) }

  | fun_decl
    { TDLoc (fmap TDFun $1) }

  | 'input' state_var_decl
    { TDLoc (fmap TDInput $2) }

  | 'output' state_var_decl
    { TDLoc (fmap TDOutput $2) }

  | 'sys_trans' expr
    { TDLoc (TDSysTrans $2 `at` getLoc $2) }

  | 'sys_liveness' expr
    { TDLoc (TDSysLiveness $2 `at` getLoc $2) }

  | 'env_trans' expr
    { TDLoc (TDEnvTrans $2 `at` getLoc $2) }

  | 'env_liveness' expr
    { TDLoc (TDEnvLiveness $2 `at` getLoc $2) }


-- Functions -------------------------------------------------------------------

fun_decl :: { Loc (Fun PName) }
  : IDENT list(IDENT) fun_body
    { Fun $1 $2 $3 `at` mconcat [getLoc $1, getLoc $2, getLoc $3] }

fun_body :: { [Guard PName] }
  : '=' expr
    { [GExpr $2] }

  | '|' sep1('|', guard_body)
    { $2 }

guard_body :: { Guard PName }
  : expr '=' expr
    { GLoc (GGuard $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | 'otherwise' '=' expr
    { GLoc (GExpr $3 `at` mappend $1 (getLoc $3)) }


-- State Var Declarations ------------------------------------------------------

state_var_decl :: { Loc (StateVar PName) }
  : IDENT ':' type opt(state_var_init)
    { StateVar { svName = $1
               , svType = $3
               , svInit = $4 } `at` mconcat [getLoc $1, getLoc $3, getLoc $4] }

-- XXX should these be restricted to values?
state_var_init :: { Expr PName }
  : '=' expr { $2 }


-- Types -----------------------------------------------------------------------

type :: { Type PName }
  : type '->' type   { TLoc (TFun $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }
  | 'Bool'           { TLoc (TBool `at` $1) }
  | 'Int'            { TLoc (TInt  `at` $1) }
  | CONIDENT         { TLoc (fmap TEnum $1) }


-- Expressions -----------------------------------------------------------------

expr :: { Expr PName }
  : 'if' app_expr 'then' app_expr 'else' app_expr
    { ELoc (EIf $2 $4 $6 `at` mappend $1 (getLoc $6)) }

  | app_expr
    { $1 }

app_expr :: { Expr PName }
  : list1(bexpr)
    { mkEApp $1 }

bexpr :: { Expr PName }
  : bexpr '||' bexpr
    { ELoc (EOr $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '&&' bexpr
    { ELoc (EAnd $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | '!' bexpr %prec NOT
    { ELoc (ENot $2 `at` mappend $1 (getLoc $2)) }

  | aexpr
    { $1 }

aexpr :: { Expr PName }
  : IDENT
    { ELoc (fmap EVar $1) }

  | CONIDENT
    { ELoc (fmap ECon $1) }

  | NUM
    { ELoc (fmap ENum $1) }

  | 'True'
    { ELoc (ETrue `at` $1) }

  | 'False'
    { ELoc (EFalse `at` $1) }

  | '(' expr ')'
    { $2 }


-- Utilities -------------------------------------------------------------------

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

list(p)
  : {- empty -}  { []         }
  | list_body(p) { reverse $1 }

list1(p)
  : list_body(p) { reverse $1 }

list_body(p)
  : p              { [$1]    }
  | list_body(p) p { $2 : $1 }

opt(p)
  : {- empty -}        { Nothing }
  | p                  { Just $1 }

{

data Error = LexcialError !(Range FilePath)
           | ParseError !Lexeme
           | UnexpectedEOF
             deriving (Show)

parseController :: FilePath -> L.Text -> Either Error (Controller PName)
parseController src bytes = controller (lexWithLayout src bytes)

parseError :: [Lexeme] -> Either Error a
parseError toks =
  case toks of
    LexError from : _ -> Left (LexcialError from)
    []                -> Left UnexpectedEOF
    tok:_             -> Left (ParseError tok)


-- Utilities -------------------------------------------------------------------

matchConIdent :: Lexeme -> Maybe (Loc L.Text)
matchConIdent Located { locValue = Token { tokType = TConIdent str }, .. } = Just (str `at` locRange)
matchConIdent _                                                            = Nothing


matchIdent :: Lexeme -> Maybe (Loc L.Text)
matchIdent Located { locValue = Token { tokType = TIdent str }, .. } = Just (str `at` locRange)
matchIdent _                                                         = Nothing


matchNum :: Lexeme -> Maybe (Loc Integer)
matchNum Located { locValue = Token { tokType = TNum i }, .. } = Just (i `at` locRange)
matchNum _                                                     = Nothing


pattern LexError range <- Located { locValue = Token { tokType = TLexicalError }, locRange = range }

pattern Keyword kw range <- Located { locValue = Token { tokType = TKeyword kw }
                                    , locRange = range }

pattern Virt v range <- Located { locValue = Token { tokType = TVirt v }
                                , locRange = range }

mkEApp :: [Expr PName] -> Expr PName
mkEApp [e] = e
mkEApp es  = ELoc (foldl1 EApp es `at` es)
}
