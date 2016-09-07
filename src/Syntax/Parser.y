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
  '->'         { Keyword Krarrow     $$ }
  '<-'         { Keyword Klarrow     $$ }

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
  'case'       { Keyword Kcase       $$ }
  'of'         { Keyword Kof         $$ }
  '='          { Keyword Kassign     $$ }
  '=='         { Keyword Keq         $$ }
  '!='         { Keyword Kneq        $$ }
  '|'          { Keyword Kpipe       $$ }
  '('          { Keyword Klparen     $$ }
  ')'          { Keyword Krparen     $$ }
  '{'          { Keyword Klbrace     $$ }
  '}'          { Keyword Krbrace     $$ }
  ','          { Keyword Kcomma      $$ }
  ':'          { Keyword Kcolon      $$ }

  'any'        { Keyword Kany        $$ }
  'all'        { Keyword Kall        $$ }
  'mutex'      { Keyword Kmutex      $$ }

  '...'        { Keyword Krange      $$ }

  'prime'      { Keyword Kprime      $$ }

  'sys_liveness' { Keyword Ksys_liveness $$ }
  'env_liveness' { Keyword Kenv_liveness $$ }
  'sys_trans'    { Keyword Ksys_trans    $$ }
  'env_trans'    { Keyword Kenv_trans    $$ }

  '&&'         { Keyword Kand        $$ }
  '||'         { Keyword Kor         $$ }
  '!'          { Keyword Knot        $$ }
  '<->'        { Keyword Kiff        $$ }

  -- layout
  'v{'  { Virt VBegin $$ }
  'v;'  { Virt VSep   $$ }
  'v}'  { Virt VEnd   $$ }


%nonassoc '<->'
%right '->'
%right '||'
%right '&&'
%right NOT
%nonassoc '==' '!=' '<-'


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

  | 'sys_trans' layout(expr)
    { TDLoc (TDSysTrans $2 `at` getLoc $2) }

  | 'sys_liveness' layout(expr)
    { TDLoc (TDSysLiveness $2 `at` getLoc $2) }

  | 'env_trans' layout(expr)
    { TDLoc (TDEnvTrans $2 `at` getLoc $2) }

  | 'env_liveness' layout(expr)
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
  : bexpr '=' expr
    { GLoc (GGuard $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | 'otherwise' '=' expr
    { GLoc (GExpr $3 `at` mappend $1 (getLoc $3)) }


-- State Var Declarations ------------------------------------------------------

state_var_decl :: { Loc (StateVar PName) }
  : IDENT ':' type opt(bounds) opt(state_var_init)
    { StateVar { svName = $1
               , svType = $3
               , svBounds = $4
               , svInit = $5 } `at` mconcat [getLoc $1, getLoc $3, getLoc $4, getLoc $5] }

-- XXX should these be restricted to values?
state_var_init :: { Expr PName }
  : '=' expr { $2 }


-- Types -----------------------------------------------------------------------

type :: { Type PName }
  : type '->' type   { TLoc (TFun $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }
  | 'Bool'           { TLoc (TBool `at` $1) }
  | 'Int'            { TLoc (TInt  `at` $1) }
  | CONIDENT         { TLoc (fmap TEnum $1) }

bounds :: { Loc (Int,Int) }
  : NUM '...' NUM { (fromInteger (thing $1), fromInteger (thing $3))
                    `at` mappend (getLoc $1) (getLoc $3) }


-- Expressions -----------------------------------------------------------------

expr :: { Expr PName }
  : 'if' bexpr 'then' expr 'else' expr
    { ELoc (EIf $2 $4 $6 `at` mappend $1 (getLoc $6)) }

  | 'case' bexpr 'of' layout(case_arm)
    { ELoc (ECase $2 $4 `at` mappend $1 (getLoc $4)) }

  | bexpr
    { $1 }

-- XXX: this should be a pattern, not an `aexpr`
case_arm :: { Case PName }
  : pat '->' expr
    { CLoc (CPat $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | 'otherwise' '->' expr
    { CLoc (CDefault $3 `at` mappend $1 (getLoc $3)) }

pat :: { Pat PName }
  : CONIDENT    { PLoc (PCon `fmap` $1) }
  | NUM         { PLoc (PNum `fmap` $1) }

bexpr :: { Expr PName }
  : bexpr '||' bexpr
    { ELoc (EOr $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '&&' bexpr
    { ELoc (EAnd $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | '!' bexpr %prec NOT
    { ELoc (ENot $2 `at` mappend $1 (getLoc $2)) }

  | bexpr '->' bexpr
    { ELoc (EImp $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '!=' bexpr
    { ELoc (EImp $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '==' bexpr
    { ELoc (EEq $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '<->' bexpr
    { ELoc (EIff $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '<-' bexpr
    { ELoc (EIn $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | app_expr
    { $1 }

app_expr :: { Expr PName }
  : list1(aexpr)
    { mkEApp $1 }

aexpr :: { Expr PName }
  : IDENT opt('prime')
    { let var = ELoc (fmap EVar $1)
       in case $2 of
            Just p  -> ELoc (ENext var `at` mappend (getLoc var) p)
            Nothing -> var }

  | CONIDENT
    { ELoc (fmap ECon $1) }

  | NUM
    { ELoc (fmap ENum $1) }

  | 'True'
    { ELoc (ETrue `at` $1) }

  | 'any'
    { ELoc (EAny `at` $1) }

  | 'all'
    { ELoc (EAll `at` $1) }

  | 'mutex'
    { ELoc (EMutex `at` $1) }

  | 'False'
    { ELoc (EFalse `at` $1) }

  | '{' sep(',', expr) '}'
    { ELoc (ESet $2 `at` mappend $1 $3) }

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
