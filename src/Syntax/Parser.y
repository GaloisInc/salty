-- vim: ft=haskell

{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -w #-}

module Syntax.Parser (
    parseController
  ) where

import SrcLoc
import Syntax.AST
import Syntax.Lexer
import PP

import           AlexTools (SourceRange,Lexeme(..),(<->),range)
import qualified Data.Text as T

}

%tokentype { Lexeme Token }

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

  'sys_init'     { Keyword Ksys_init     $$ }
  'env_init'     { Keyword Kenv_init     $$ }
  'sys_liveness' { Keyword Ksys_liveness $$ }
  'env_liveness' { Keyword Kenv_liveness $$ }
  'sys_trans'    { Keyword Ksys_trans    $$ }
  'env_trans'    { Keyword Kenv_trans    $$ }

  '&&'         { Keyword Kand        $$ }
  '||'         { Keyword Kor         $$ }
  '^'          { Keyword Kxor        $$ }
  '!'          { Keyword Knot        $$ }
  '<->'        { Keyword Kiff        $$ }
  '+'          { Keyword Kplus       $$ }

  -- layout
  'v{'  { Virt VBegin $$ }
  'v;'  { Virt VSep   $$ }
  'v}'  { Virt VEnd   $$ }


%nonassoc '<->'
%right '->'
%right '||'
%right '&&'
%right '^'
%right NOT
%nonassoc '==' '!=' '<-'
%left '+'
%right 'any' 'all' 'mutex'


%monad { Either Error }
%error { parseError }

%name controller

%%

controller :: { Controller Parsed }
  : 'controller' CONIDENT 'where' layout(top_decl)
    { Controller (srcLoc ($1,$3,$4)) $2 $4 }

top_decl :: { TopDecl Parsed }

  : opt(ann) 'enum' CONIDENT '=' sep1('|', con_def)
    { let { loc = srcLoc ($1,$2,$3) } in TDEnum loc (EnumDef loc $1 $3 $5) }

  | fun_decl
    { TDFun (srcLoc $1) $1 }

  | opt(ann) 'input' state_var_decl
    { let { i = $3 $1 } in TDInput (srcLoc i) i }

  | opt(ann) 'output' state_var_decl
    { let { o = $3 $1 } in TDOutput (srcLoc o) o }

  | spec
    { TDSpec (srcLoc $1) $1 }

  | expr
    { TDExpr (srcLoc $1) $1 }

con_def :: { (PName, Maybe T.Text) }
  : CONIDENT opt(out_name)
    { ($1,$2) }


-- Specification ---------------------------------------------------------------

spec :: { Spec Parsed }
  : 'sys_trans' layout(expr)
    { SSysTrans (srcLoc $2) $2 }

  | 'sys_liveness' layout(expr)
    { SSysLiveness (srcLoc $2) $2 }

  | 'env_trans' layout(expr)
    { SEnvTrans (srcLoc $2) $2 }

  | 'env_liveness' layout(expr)
    { SEnvLiveness (srcLoc $2) $2 }

  | 'sys_init' layout(expr)
    { SSysInit (srcLoc $2) $2 }

  | 'env_init' layout(expr)
    { SEnvInit (srcLoc $2) $2 }


-- Functions -------------------------------------------------------------------

fun_decl :: { Fun Parsed }
  : opt(ann) 'def' IDENT parens(sep(',', IDENT)) '=' fun_body
    { Fun (srcLoc ($1,$2,$3,$4,$6)) $1 $3 $4 $6 }

  | opt(ann) 'def' IDENT '=' fun_body
    { Fun (srcLoc ($1,$2,$3,$5)) $1 $3 [] $5 }

fun_body :: { FunBody Parsed }
  : list1(spec) { FBSpec (srcLoc $1) $1 }
  | expr        { FBExpr (srcLoc $1) $1 }


-- State Var Declarations ------------------------------------------------------

state_var_decl :: { Maybe (Ann Parsed) -> StateVar Parsed }
  : IDENT opt(out_name) ':' type opt(bounds) opt(state_var_init)
    { \ svAnn ->
      StateVar { svAnnot = srcLoc ($1,$4,$5,$6)
               , svName = $1
               , svType = $4
               , svBounds = $5
               , svInit = $6
               , svOutName = $2
               , .. } }

state_var_init :: { Expr Parsed }
  : '=' expr { $2 }

-- optional literal name to use during code generation
out_name :: { T.Text }
  : '(' IDENT    ')' { pnameText $2 }
  | '(' CONIDENT ')' { pnameText $2 }


-- Types -----------------------------------------------------------------------

type :: { Type Parsed }
  : type '->' type   { TFun  (srcLoc ($1,$3)) $1 $3 }
  | 'Bool'           { TBool (srcLoc $1) }
  | 'Int'            { TInt  (srcLoc $1) }
  | CONIDENT         { TEnum (srcLoc $1) $1 }

bounds :: { Bounds Parsed }
  : NUM '...' NUM
    { Bounds { bAnnot = srcLoc (fst $1, fst $3)
             , bLow   = fromIntegral (snd $1)
             , bHigh  = fromIntegral (snd $3)
             } }


-- Expressions -----------------------------------------------------------------

expr :: { Expr Parsed }
  : 'if' bexpr 'then' expr 'else' expr
    { EIf (srcLoc ($1,$6)) $2 $4 $6 }

  | 'case' bexpr 'of' layout(case_arm)
    { ECase (srcLoc ($1,$4)) $2 $4 }

  | bexpr
    { $1 }

case_arm :: { Case Parsed }
  : pat '->' expr
    { CPat (srcLoc ($1,$3)) $1 $3 }

  | 'otherwise' '->' expr
    { CDefault (srcLoc ($1,$3)) $3 }

pat :: { Pat Parsed }
  : CONIDENT    { PCon (srcLoc $1) $1 }
  | NUM         { PNum (fst $1) (snd $1) }

bexpr :: { Expr Parsed }
  : bexpr '||' bexpr
    { EOr (srcLoc ($1,$3)) $1 $3 }

  | bexpr '&&' bexpr
    { EAnd (srcLoc ($1,$3)) $1 $3 }

  | bexpr '^' bexpr
    { EXor (srcLoc ($1,$3)) $1 $3 }

  | '!' bexpr %prec NOT
    { ENot (srcLoc ($1,$2)) $2 }

  | bexpr '->' bexpr
    { EImp (srcLoc ($1,$3)) $1 $3 }

  | bexpr '!=' bexpr
    { ENeq (srcLoc ($1,$3)) $1 $3 }

  | bexpr '==' bexpr
    { EEq (srcLoc ($1,$3)) $1 $3 }

  | bexpr '<->' bexpr
    { EIff (srcLoc ($1,$3)) $1 $3 }

  | bexpr '<-' bexpr
    { EIn (srcLoc ($1,$3)) $1 $3 }

  | bexpr '+' bexpr
    { EPlus (srcLoc ($1,$3)) $1 $3 }

  | 'any' bexpr
    { mkEApp (EAny (srcLoc $1)) [$2] }

  | 'all' bexpr
    { mkEApp (EAll (srcLoc $1)) [$2] }

  | 'mutex' bexpr
    { mkEApp (EMutex (srcLoc $1)) [$2] }

  | aexpr
    { $1 }

aexpr :: { Expr Parsed }
  : IDENT parens(sep(',', expr))
    { mkEApp (EVar (srcLoc $1) $1) $2 }

  | IDENT opt('prime')
    { let var = EVar (srcLoc $1) $1
       in case $2 of
            Just p  -> ENext (srcLoc (p :: SourceRange,var)) var
            Nothing -> var }

  | CONIDENT
    { ECon (srcLoc $1) $1 }

  | NUM
    { ENum (fst $1) (snd $1) }

  | 'True'
    { ETrue (srcLoc $1) }

  | 'False'
    { EFalse (srcLoc $1) }

  | '{' sep(',', expr) '}'
    { ESet (srcLoc ($1,$3)) $2 }

  | '(' expr ')'
    { $2 }


-- Annotations -----------------------------------------------------------------

ann :: { Ann Parsed }
  : '@' ann_expr opt('v;')
    { $2 }

ann_app :: { Ann Parsed }
  : CONIDENT parens(sep(',', ann_expr))
    { AnnApp (srcLoc ($1,$2)) (pnameText $1) $2 }

ann_expr :: { Ann Parsed }
  : ann_app
    { $1 }

  | CONIDENT
    { AnnSym (srcLoc $1) (pnameText $1) }

  | IDENT
    { AnnSym (srcLoc $1) (pnameText $1) }

  | STRING
    { AnnStr (fst $1) (snd $1) }

  | '{' sep(',', ann_obj_entry) '}'
    { AnnObj (srcLoc ($1,$3)) $2 }

  | '[' sep(',', ann_expr) ']'
    { AnnArr (srcLoc ($1,$3)) $2 }

  | CODE
    { let { (a,b,c) = $1 } in AnnCode a b c }

ann_obj_entry :: { (T.Text, Ann Parsed) }
  : CONIDENT '=' ann_expr
    { (pnameText $1, $3) }

  | IDENT '=' ann_expr
    { (pnameText $1, $3) }


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

data Error = LexicalError !SourceRange
           | ParseError !(Lexeme Token)
           | UnexpectedEOF
             deriving (Show)

instance HasSrcLoc Error where
  srcLoc (LexicalError loc) = srcLoc loc
  srcLoc (ParseError   loc) = srcLoc (lexemeRange loc)
  srcLoc UnexpectedEOF      = mempty

instance PP Error where
  ppPrec _ (LexicalError _) = text "Lexical error"
  ppPrec _ (ParseError _)   = text "Parse error"
  ppPrec _ UnexpectedEOF    = text "Unexpected end of file"

parseController :: FilePath -> T.Text -> Either Error (Controller Parsed)
parseController src bytes =
  case controller (lexWithLayout src bytes) of
    Right a -> Right a
    Left (LexicalError loc) -> Left (LexicalError loc)
    Left (ParseError loc)   -> Left (ParseError   loc)
    Left UnexpectedEOF      -> Left UnexpectedEOF

parseError :: [Lexeme Token] -> Either Error a
parseError toks =
  case toks of
    LexError from : _ -> Left (LexicalError from)
    []                -> Left UnexpectedEOF
    tok:_             -> Left (ParseError tok)


-- Utilities -------------------------------------------------------------------

matchConIdent :: Lexeme Token -> Maybe PName
matchConIdent Lexeme { lexemeToken = TConIdent, .. } = Just (PName (srcLoc lexemeRange) lexemeText)
matchConIdent _                                      = Nothing


matchIdent :: Lexeme Token -> Maybe PName
matchIdent Lexeme { lexemeToken = TIdent, .. } = Just (PName (srcLoc lexemeRange) lexemeText)
matchIdent _                                   = Nothing


matchNum :: Lexeme Token -> Maybe (SrcLoc,Integer)
matchNum Lexeme { lexemeToken = TNum i, .. } = Just (srcLoc lexemeRange, i)
matchNum _                                   = Nothing

matchString :: Lexeme Token -> Maybe (SrcLoc,T.Text)
matchString Lexeme { lexemeToken = TString str, .. } = Just (srcLoc lexemeRange, str)
matchString _                                        = Nothing

matchCode :: Lexeme Token -> Maybe (SrcLoc,T.Text,T.Text)
matchCode Lexeme { lexemeToken = TCode n str, .. } = Just (srcLoc lexemeRange,n,str)
matchCode _                                        = Nothing


pattern LexError range <- Lexeme { lexemeToken = TLexicalError, lexemeRange = range }

pattern Keyword kw range <- Lexeme { lexemeToken = TKeyword kw, lexemeRange = range }

pattern Virt v range <- Lexeme { lexemeToken = TVirt v, lexemeRange = range }

mkEApp :: Expr Parsed -> [Expr Parsed] -> Expr Parsed
mkEApp  = foldl (\acc e -> EApp (srcLoc (acc,e)) acc e)
}
