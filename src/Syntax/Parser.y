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
import PP

import qualified Data.Text.Lazy as L
import           Text.Location
import           Text.Location.Layout

}

%tokentype { Lexeme }

%token
  IDENT    { (matchIdent    -> Just $$) }
  CONIDENT { (matchConIdent -> Just $$) }
  NUM      { (matchNum      -> Just $$) }
  STRING   { (matchString   -> Just $$) }
  CODE     { (matchCode     -> Just $$) }

  -- types
  'Bool'       { Keyword KBool       $$ }
  'Int'        { Keyword KInt        $$ }
  '->'         { Keyword Krarrow     $$ }
  '<-'         { Keyword Klarrow     $$ }

  -- constants
  'True'       { Keyword KTrue       $$ }
  'False'      { Keyword KFalse      $$ }

  -- keywords
  'def'        { Keyword Kdef        $$ }
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
  '['          { Keyword Klbracket   $$ }
  ']'          { Keyword Krbracket   $$ }
  ','          { Keyword Kcomma      $$ }
  ':'          { Keyword Kcolon      $$ }

  '@'          { Keyword Kann        $$ }

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
%right 'any' 'all' 'mutex'


%monad { Either Error }
%error { parseError }

%name controller

%%

controller :: { Controller PName }
  : 'controller' CONIDENT 'where' layout(top_decl)
    { Controller $2 $4 }

top_decl :: { TopDecl PName }

  : opt(ann) 'enum' CONIDENT '=' sep1('|', con_def)
    { TDLoc (TDEnum (EnumDef $1 $3 $5) `at` getLoc ($1,$2,$5)) }

  | fun_decl
    { TDLoc (fmap TDFun $1) }

  | opt(ann) 'input' state_var_decl
    { TDLoc (fmap TDInput ($3 $1)) }

  | opt(ann) 'output' state_var_decl
    { TDLoc (fmap TDOutput ($3 $1)) }

  | spec
    { TDLoc (TDSpec $1 `at` $1) }

  | expr
    { TDLoc (TDExpr $1 `at` $1) }

con_def :: { (Loc L.Text, Maybe (Loc L.Text)) }
  : CONIDENT opt(out_name) { ($1,$2) }


-- Specification ---------------------------------------------------------------

spec :: { Spec PName }
  : 'sys_trans' layout(expr)
    { SLoc (SSysTrans $2 `at` getLoc $2) }

  | 'sys_liveness' layout(expr)
    { SLoc (SSysLiveness $2 `at` getLoc $2) }

  | 'env_trans' layout(expr)
    { SLoc (SEnvTrans $2 `at` getLoc $2) }

  | 'env_liveness' layout(expr)
    { SLoc (SEnvLiveness $2 `at` getLoc $2) }


-- Functions -------------------------------------------------------------------

fun_decl :: { Loc (Fun PName) }
  : opt(ann) 'def' IDENT parens(sep(',', IDENT)) '=' fun_body
    { Fun $1 $3 $4 $6 `at` mconcat [getLoc $1, getLoc $3, getLoc $4, getLoc $6] }

  | opt(ann) 'def' IDENT '=' fun_body
    { Fun $1 $3 [] $5 `at` mconcat [getLoc $1, getLoc $3, getLoc $5] }

fun_body :: { FunBody PName }
  : list1(spec) { FBSpec $1 }
  | expr        { FBExpr $1 }


-- State Var Declarations ------------------------------------------------------

state_var_decl :: { Maybe Ann -> Loc (StateVar PName) }
  : IDENT opt(out_name) ':' type opt(bounds) opt(state_var_init)
    { \ svAnn ->
      StateVar { svName = $1
               , svType = $4
               , svBounds = $5
               , svInit = $6
               , svOutName = $2
               , .. } `at` mconcat [getLoc $1, getLoc $4, getLoc $5, getLoc $6] }

state_var_init :: { Expr PName }
  : '=' expr { $2 }

-- optional literal name to use during code generation
out_name :: { Loc L.Text }
  : '(' IDENT    ')' { $2 }
  | '(' CONIDENT ')' { $2 }


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
    { ELoc (ENeq $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '==' bexpr
    { ELoc (EEq $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '<->' bexpr
    { ELoc (EIff $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | bexpr '<-' bexpr
    { ELoc (EIn $1 $3 `at` mappend (getLoc $1) (getLoc $3)) }

  | 'any' bexpr
    { mkEApp (ELoc (EAny `at` $1)) [$2] }

  | 'all' bexpr
    { mkEApp (ELoc (EAll `at` $1)) [$2] }

  | 'mutex' bexpr
    { mkEApp (ELoc (EMutex `at` $1)) [$2] }

  | aexpr
    { $1 }

aexpr :: { Expr PName }
  : IDENT parens(sep(',', expr))
    { mkEApp (ELoc (EVar (thing $1) `at` $1)) $2 }

  | IDENT opt('prime')
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

  | 'False'
    { ELoc (EFalse `at` $1) }

  | '{' sep(',', expr) '}'
    { ELoc (ESet $2 `at` mappend $1 $3) }

  | '(' expr ')'
    { $2 }


-- Annotations -----------------------------------------------------------------

ann :: { Ann }
  : '@' ann_expr opt('v;')
    { $2 }

ann_app :: { Ann }
  : CONIDENT parens(sep(',', ann_expr))
    { AnnLoc (AnnApp (thing $1) $2 `at` mappend (getLoc $1) (getLoc $2)) }

ann_expr :: { Ann }
  : ann_app
    { $1 }

  | CONIDENT
    { AnnLoc (AnnSym `fmap` $1) }

  | IDENT
    { AnnLoc (AnnSym `fmap` $1) }

  | STRING
    { AnnLoc (AnnStr `fmap` $1) }

  | '{' sep(',', ann_obj_entry) '}'
    { AnnLoc (AnnObj $2 `at` map snd $2) }

  | '[' sep(',', ann_expr) ']'
    { AnnLoc (AnnArr $2 `at` $2) }

  | CODE
    { let { (a,b) = thing $1 } in AnnLoc (AnnCode a b `at` $1) }

ann_obj_entry :: { (L.Text, Ann) }
  : CONIDENT '=' ann_expr
    { (thing $1, $3) }

  | IDENT '=' ann_expr
    { (thing $1, $3) }


-- Utilities -------------------------------------------------------------------

parens(p)
  : '(' p ')' { $2 }

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

data Error = LexicalError !(Range FilePath)
           | ParseError !Lexeme
           | UnexpectedEOF
             deriving (Show)

instance PP Error where
  ppPrec _ (LexicalError _) = text "Lexical error"
  ppPrec _ (ParseError _)   = text "Parse error"
  ppPrec _ UnexpectedEOF    = text "Unexpected end of file"

parseController :: FilePath -> L.Text -> Either (Loc Error) (Controller PName)
parseController src bytes =
  case controller (lexWithLayout src bytes) of
    Right a -> Right a
    Left (LexicalError loc) -> Left (LexicalError loc `at` loc)
    Left (ParseError loc)   -> Left (ParseError   loc `at` loc)
    Left UnexpectedEOF      -> Left (UnexpectedEOF    `at` (mempty :: Range FilePath))

parseError :: [Lexeme] -> Either Error a
parseError toks =
  case toks of
    LexError from : _ -> Left (LexicalError from)
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

matchString :: Lexeme -> Maybe (Loc L.Text)
matchString Located { locValue = Token { tokType = TString str}, .. } = Just (str `at` locRange)
matchString _                                                         = Nothing

matchCode :: Lexeme -> Maybe (Loc (L.Text,L.Text))
matchCode Located { locValue = Token { tokType = TCode n str }, .. } =
  Just ((n,str) `at` locRange)
matchCode _ = Nothing


pattern LexError range <- Located { locValue = Token { tokType = TLexicalError }, locRange = range }

pattern Keyword kw range <- Located { locValue = Token { tokType = TKeyword kw }
                                    , locRange = range }

pattern Virt v range <- Located { locValue = Token { tokType = TVirt v }
                                , locRange = range }

mkEApp :: Expr PName -> [Expr PName] -> Expr PName
mkEApp f es = ELoc (foldl EApp f es `at` mconcat (getLoc f : map getLoc es))
}
