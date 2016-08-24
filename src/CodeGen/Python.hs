{-# LANGUAGE RecordWildCards #-}

module CodeGen.Python (
    pythonFSM
  ) where

import PP
import Scope.Name
import Slugs.FSM
import TypeCheck.AST

import qualified Data.Text.Lazy as L
import qualified Data.Map.Strict as Map


type Package = Map.Map FilePath Doc

-- | Generate a python class when given an FSM from slugs.
pythonFSM :: FSM -> Package
pythonFSM FSM { .. } =
  Map.fromList [ (render (pythonName fsmName <> text ".py"), impl) ]
  where
  impl =
    block (cls (pythonName fsmName)) $ vcat $
      [ text ""
      , block (def "__init__" [text "self"]) $ vcat
          [ assign (self "state") (int 0)
          ]

      ] ++ concat

      [ [ text "", mkEnum e ] | e <- fsmEnums ] ++

      [ text ""
      , defMove (pythonName fsmName) fsmInputs fsmNodes

      , text ""
      , defError fsmInputs
      ]


mkEnum :: EnumDef -> Doc
mkEnum EnumDef { .. } =
  block (cls (pythonName eName))
        (vcat (zipWith mkCon [0 ..] eCons))

mkCon :: Int -> Name -> Doc
mkCon i n = assign (upper n) (int i)

defMove :: Doc -> StateVars -> Map.Map Int Node -> Doc
defMove pfx inps nodes =
  block (def "move" (text "self" : inpVars))
        (vcat (mkStateCond "if" (Map.toList nodes)))

  where

  inpVars = [ pythonName n | (n,_) <- Map.toList inps ]

  mkStateCond p ((i,node):rest) =
    block (text p <+> self "state" <+> text "==" <+> int i)
          (vcat ( text "output = dict()"
                : text ""
                : mkNodeCond "if" (nodeTrans node)))
    : mkStateCond "elif" rest

  mkStateCond _ [] =
    [ block (text "else")
      (text "raise Exception(\"Unrecognized internal state: \" + str(self.state))")

    , text "return output"
    ]


  mkNodeCond p (i : rest) =
    let Node { .. } = nodes Map.! i
        cond = hcat
             $ punctuate (text " and")
             $ [ parens (pythonName n <+> text "==" <+> pythonValue pfx v)
               | (n,v) <- Map.toList nodeInputs ]

     in block (text p <+> cond) (vcat (mkResult i nodeOutputs))
        : mkNodeCond "elif" rest

  mkNodeCond _ [] =
    [ block (text "else")
      (self "_error" <> parens (hcat (punctuate comma inpVars)))
    ]


  mkResult i outs =
    [ assign (self "state") (int i)
    , text ""
    ] ++
    [ assign (text "output" <> brackets (doubleQuotes (pythonName n)))
             (pythonValue pfx v)
    | (n,v) <- Map.toList outs ]


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



pythonValue :: Doc -> Value -> Doc
pythonValue _ (VBool b) = text (show b)

pythonValue pfx (VCon n)  = 
  case nameOrigin n of
    FromController _ -> pfx <> char '.'                             <> upper n
    FromDecl _ d     -> pfx <> char '.' <> pythonName d <> char '.' <> upper n
    FromParam _ d    -> pfx <> char '.' <> pythonName d <> char '.' <> upper n
    Generated _      -> pythonName n

pythonValue _ (VNum i)  = integer i


-- Utils -----------------------------------------------------------------------

self :: String -> Doc
self field = text "self" <> char '.' <> text field

assign :: Doc -> Doc -> Doc
assign var e = hang (var <+> char '=') 2 e

pythonName :: Name -> Doc
pythonName n = pp (nameText n)

upper :: Name -> Doc
upper n = pp (L.toUpper (nameText n))

cls :: Doc -> Doc
cls x = text "class" <+> x

block :: Doc -> Doc -> Doc
block hdr body = (hdr <> char ':') $$ nest 4 body

def :: String -> [Doc] -> Doc
def name args =
  text "def" <+> text name <> parens (hsep (punctuate comma args))
