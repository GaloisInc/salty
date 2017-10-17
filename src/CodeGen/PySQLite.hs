{-# LANGUAGE RecordWildCards #-}

module CodeGen.PySQLite (
    pySQLiteFSM
  ) where

import PP
import Scope.Name
import Slugs.FSM
import TypeCheck.AST

import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T


type Package = Map.Map FilePath Doc

-- | Generate a python class when given an FSM from slugs.
pySQLiteFSM :: FSM -> Package
pySQLiteFSM FSM { .. } =
  Map.fromList [ (render (pythonName fsmName <> text ".py"), impl)
               , (render (pythonName fsmName <> text ".sql"), sqlite)
               ]
  where
  cont = pythonName fsmName

  sqlite = vcat
    [ mkSchema fsmInputs fsmOutputs
    , mkData fsmNodes
    , text ""
    ]

  impl = vcat $
    [ text "from enum import Enum"
    , text "import sqlite3"
    , text ""
    , block (cls cont (text "object")) $ vcat $
        [ text ""

        , hang (text "__slots__ =")
             2 (brackets (fsep (punctuate comma (map doubleQuotes [ text "_state"
                                                                  , text "_table"
                                                                  , text "_conn"
                                                                  ]))))

        , text ""
        , block (def "__init__" [text "self", text "db_path"]) $ vcat
            [ assign (self "_state") (int fsmInitial)
            , assign (self "_conn") (text "sqlite3" <>
                                     char '.' <>
                                     text "connect" <>
                                     parens (text "db_path"))
            ]

        ] ++ concat

        [ [ text "", mkEnum e ] | e <- fsmEnums ] ++

        [ text ""
        , defMove fsmInputs fsmOutputs

        , text ""
        , defError fsmInputs
        ]

    , text ""
    ]

mkSchema :: StateVars -> StateVars -> Doc
mkSchema inps outs = text "CREATE TABLE salty" <+>
                     parens (fsep (punctuate comma decls)) <>
                     char ';'
  where decls = [ text "state INTEGER", text "next_state INTEGER" ] ++ inDecls ++ outDecls
        inDecls = [ text "in_" <> sqliteType n info
                  | (n, info) <- Map.toList inps
                  ]
        outDecls = [ text "out_" <> sqliteType n info
                   | (n, info) <- Map.toList outs
                   ]

mkData :: Map.Map Int Node -> Doc
mkData nodes = vcat (zipWith mkNode [0..] (Map.toList nodes))
  where
    mkNode n (_, node) = vcat (map (mkTrans n) (nodeTrans node))
    mkTrans n i =
      let Node { .. } = nodes Map.! i
      in text "INSERT INTO salty VALUES" <+>
         parens (fsep (punctuate comma (integer n : pp i : values nodeInputs nodeOutputs))) <>
         char ';'
    values ins outs = [ sqliteValue val | (_,val) <- Map.toList ins ] ++
                      [ sqliteValue val | (_,val) <- Map.toList outs ]

sqliteType :: Name -> VarInfo -> Doc
sqliteType name info = pythonName name <+> typ
  where typ = case viType info of
          VTBool   -> text "BOOLEAN"
          VTEnum n -> pythonName (eName n)
          VTInt {} -> text "INTEGER"

sqliteValue :: Value -> Doc
sqliteValue (VBool True) = integer 1
sqliteValue (VBool False) = integer 0
sqliteValue (VCon n) =
  doubleQuotes $ case nameOrigin n of
    FromController _ -> upper n
    FromDecl _ _     -> upper n
    FromParam _ _    -> upper n
    Generated _      -> pythonName n
sqliteValue (VNum i) = integer i



mkEnum :: EnumDef -> Doc
mkEnum EnumDef { .. } =
  block (cls (pythonName eName) (text "Enum"))
        (vcat (zipWith mkCon [0 ..] eCons))

mkCon :: Int -> Name -> Doc
mkCon i n =
  case nameOutText n of
    Just out -> assign (pp out)  (int i)
    Nothing  -> assign (upper n) (int i)

mkQuery :: StateVars -> StateVars -> Doc
mkQuery inps outs =
  doubleQuotes (text "SELECT" <+> outputs <+>
                text "FROM salty WHERE" <+> condition)
  where
  outputs = hcat $ punctuate comma $
              text "next_state" : [ text "out_" <> pythonName n
                                  | (n,_) <- Map.toList outs
                                  ]
  condition = hcat $ punctuate (text " AND ")
    [ v <+> char '=' <+> char '?'
    | v <- text "state" : [ text "in_" <> pythonName n | (n,_) <- Map.toList inps ]
    ]

defMove :: StateVars -> StateVars -> Doc
defMove inps outs =
  block (def "move" (text "self" : inpVars))
        (vcat stmts)

  where

  inpVars = [ pythonName n | (n,_) <- Map.toList inps ]
  inpExprs = [ toQueryExpr n info | (n, info) <- Map.toList inps ]
  outpVars = [ pythonName n | (n,_) <- Map.toList outs ]
  outpExprs = [ toPythonExpr n info | (n, info) <- Map.toList outs ]

  toQueryExpr n info = case viType info of
    VTBool    -> pythonName n
    VTInt {}  -> pythonName n
    VTEnum {} -> pythonName n <> char '.' <> text "name"

  toPythonExpr n info = case viType info of
    VTBool   -> text "bool" <> parens (pythonName n)
    VTInt {} -> pythonName n
    VTEnum e -> text "self" <> char '.' <>
                text "__class__" <> char '.' <>
                pythonName (eName e) <>
                brackets (pythonName n)


  queryInput = case (text "self._state") : inpExprs of
    []   -> error "[unreachable]"
    [st] -> parens (st <> char ',')
    ls   -> parens (hcat (punctuate comma ls))

  stmts =
    [ block (text "try") $ vcat
      [ assign
          (text "query")
          (text "list" <>
           parens ( text "self" <> char '.' <> text "_conn" <> char '.' <> text "execute" <>
                    parens (mkQuery inps outs <> char ',' <+> queryInput)))
      , assign (text "new_state") (text "query" <>
                                   brackets (text "0") <>
                                   brackets (text "0"))
      , assign (fsep (punctuate comma outpVars))
          (text "query" <>
            brackets (text "0") <>
            brackets (case length outpVars of
                        1 -> text "1"
                        _ -> text "1:"))
      , assign (self "_state") (text "new_state")
      , mkResult (parens (fsep (punctuate comma outpExprs)))
      ]

    , text ""
    , block (text "except IndexError") $ vcat
      [ text "raise Exception(\"Unrecognized internal state: \" + str(self._state))"
      ]

    , text ""
    , block (text "except Exception") $ vcat
      [ self "_error" <> parens (hcat (punctuate comma inpVars))
      ]

    ]

  mkResult res
    | Map.size outs > 1 =
      hang (text "return {")
         2 (vcat [ doubleQuotes (pythonName n) <> char ':'
                   <+> res <> brackets (int i) <> comma
                 | (n,i) <- zip (Map.keys outs) [0 .. ] ])

        $$ char '}'

    | otherwise =
      text "return" <+> res



defError :: StateVars -> Doc
defError inps =
  block (def "_error" (text "self" : inpVars))
    $ hang (text "raise ValueError(\"Unrecognized input: \" + (")
         4 (hang (vcat fmt <> text ").format(") 4 (vcat (mkVars inpVars)))
  where
  inpVars = [ pythonName n | (n,_) <- Map.toList inps ]

  fmt = [ doubleQuotes (n <+> char '=' <+> braces n <> semicolon <> space)
        | n <- inpVars ]

  mkVars [var]  = [mkAssign var <> text "))"]
  mkVars (v:vs) = mkAssign v <> comma : mkVars vs
  mkVars []     = []

  mkAssign var = var <> char '=' <> var


-- Utils -----------------------------------------------------------------------

self :: String -> Doc
self field = text "self" <> char '.' <> text field

assign :: Doc -> Doc -> Doc
assign var e = var <+> char '=' <+> e

pythonName :: Name -> Doc
pythonName n = pp (fromMaybe (nameText n) (nameOutText n))

upper :: Name -> Doc
upper n = pp (T.toUpper (nameText n))

cls :: Doc -> Doc -> Doc
cls x super = text "class" <+> x <> parens super

block :: Doc -> Doc -> Doc
block hdr body = (hdr <> char ':') $$ indent 4 body

def :: String -> [Doc] -> Doc
def name args =
  text "def" <+> text name <> parens (hsep (punctuate comma args))
