{-# LANGUAGE RecordWildCards #-}

module CodeGen.Python (
    pythonFSM
  ) where

import PP
import Scope.Name
import Slugs.FSM
import TypeCheck.AST

import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as L


type Package = Map.Map FilePath Doc

-- | Generate a python class when given an FSM from slugs.
pythonFSM :: FSM -> Package
pythonFSM FSM { .. } =
  Map.fromList [ (render (pythonName fsmName <> text ".py"), impl) ]
  where
  cont = pythonName fsmName

  impl = vcat $
    [ text "from enum import Enum"
    , text ""
    , block (cls cont) $ vcat $
        [ text ""

        , block (def "__init__" [text "self"]) $ vcat
            [ assign (self "_state") (int 0)
            ]

        ] ++ concat

        [ [ text "", mkEnum e ] | e <- fsmEnums ] ++

        [ text ""
        , mkTable fsmNodes

        , text ""
        , defMove fsmInputs fsmOutputs

        , text ""
        , defError fsmInputs
        ]

    ]


mkTable :: Map.Map Int Node -> Doc
mkTable nodes =
  hang (text "_table" <+> char '=' <+> char '[')
     4 (vcat (map mkNode (Map.elems nodes)) $$ text "]")

  where

  mkNode node =
    char '{'
    $$ nest 4 (vcat (map mkTrans (nodeTrans node)))
    $$ text "},"

  mkTrans i =
    let Node { .. } = nodes Map.! i
     in valueTuple nodeInputs
        <> char ':'
       <+> parens (pp i <> comma <+> valueTuple nodeOutputs)
        <> comma


  valueTuple vals
    | Map.size vals > 1 =
      parens $ hcat
             $ punctuate comma
               [ pythonValue val | val <- Map.elems vals ]

    | Map.null vals =
      undefined

    | otherwise =
      let [val] = Map.elems vals
       in pythonValue val

mkEnum :: EnumDef -> Doc
mkEnum EnumDef { .. } =
  block (cls (pythonName eName) <> parens (text "Enum"))
        (vcat (zipWith mkCon [0 ..] eCons))

mkCon :: Int -> Name -> Doc
mkCon i n =
  case nameOutText n of
    Just out -> assign (pp out)  (int i)
    Nothing  -> assign (upper n) (int i)

defMove :: StateVars -> StateVars -> Doc
defMove inps outs =
  block (def "move" (text "self" : inpVars))
        (vcat stmts)

  where

  inpVars = [ pythonName n | (n,_) <- Map.toList inps ]

  inpTuple
    | length inpVars > 1 = parens (hcat (punctuate comma inpVars))
    | null inpVars       = text "nil"
    | otherwise          = hcat inpVars

  stmts =
    [ block (text "try") $ vcat
      [ assign (text "table") (self "_table" <> brackets (self "_state"))
      , assign (text "newState,res") (text "table" <> brackets inpTuple)
      , assign (self "_state") (text "newState")
      , mkResult (text "res")
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



pythonValue :: Value -> Doc
pythonValue (VBool b) = text (show b)

pythonValue (VCon n) =
  case nameOrigin n of
    FromController _ ->                             upper n
    FromDecl _ d     -> pythonName d <> char '.' <> upper n
    FromParam _ d    -> pythonName d <> char '.' <> upper n
    Generated _      -> pythonName n

pythonValue (VNum i) = integer i


-- Utils -----------------------------------------------------------------------

self :: String -> Doc
self field = text "self" <> char '.' <> text field

assign :: Doc -> Doc -> Doc
assign var e = var <+> char '=' <+> e

pythonName :: Name -> Doc
pythonName n = pp (fromMaybe (nameText n) (nameOutText n))

upper :: Name -> Doc
upper n = pp (L.toUpper (nameText n))

cls :: Doc -> Doc
cls x = text "class" <+> x

block :: Doc -> Doc -> Doc
block hdr body = (hdr <> char ':') $$ nest 4 body

def :: String -> [Doc] -> Doc
def name args =
  text "def" <+> text name <> parens (hsep (punctuate comma args))
