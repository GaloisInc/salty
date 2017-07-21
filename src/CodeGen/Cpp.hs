{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Cpp ( cppFSM ) where

import PP
import Scope.Name
import Slugs.FSM
import TypeCheck.AST

import           Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)


type Package = Map.Map FilePath Doc

cppFSM :: [String] -> FSM -> Package
cppFSM ns FSM { fsmName, fsmEnums, fsmInputs, fsmOutputs, fsmInitial, fsmNodes } =
  Map.fromList [ (headerName, header), (moduleName ++ ".cpp", source) ]

  where

  clsName = cppName fsmName

  moduleName = render clsName

  headerName = moduleName ++ ".h"

  guardVariable = text (map toUpper moduleName) <> text "_H"

  header = vcat $
    [ text "#pragma once"
    , text "#ifndef" <+> guardVariable
    , text "#define" <+> guardVariable
    , text ""
    , text "// NOTE: this uses C++11 features"
    , text ""
    , text "#include <map>"
    , text ""
    ] ++

    [ text "namespace" <+> text n <+> char '{' | n <- ns ] ++

    [ text "" ] ++

    mkEnums fsmEnums ++

    [ cls clsName
      (vcat (resTyDecl ++
             [ fun empty clsName [] <> semi
             , fun resTy (text "&move") [ mkInput i | i <- Map.toList fsmInputs ] <> semi
             ]))

      (vcat [ decl (text "int") (text "state") empty
            ])

    , text ""
    , text (replicate (length ns) '}')
    , text ""
    , text "#endif"
    , text ""
    ]

  resTy
    | Map.size fsmOutputs > 1  = text "result"
    | otherwise                = cppType (viType (head (Map.elems fsmOutputs)))

  resTyDecl
    | Map.size fsmOutputs == 1 = []
    | otherwise                =
      [ struct (text "result")
          [ cppType viType <+> cppName n | (n, VarInfo { viType }) <- Map.toList fsmOutputs ]
      , text ""
      ]

  keyTy
    | Map.size fsmInputs > 1 = text "key"
    | otherwise              = cppType (viType (head (Map.elems fsmInputs)))

  keyTyDecl
    | Map.size fsmInputs == 1 = []
    | otherwise               =
      [ text ""
      , struct (text "key") $
          [ cppType viType <+> cppName n | (n, VarInfo { viType }) <- Map.toList fsmInputs ] ++
          [ vcat
            [ fun (text "bool")
                  (text "operator<")
                  [ text "const" <+> keyTy <+> text "&other" ]
                  <+> text "const {"
            , nest 2 $ hsep $
              text "return" :
              punctuate (text " &&") (map mkLt (Map.keys fsmInputs)) ++
              [ semi ]
            , char '}'
            ]
          ]
      , text "" ]

  srcResTy | null resTyDecl = resTy
           | otherwise      = clsName <> text "::" <> resTy

  mapTy = template (text "std::map")
          [ keyTy
          , template (text "std::pair")
            [ text "int"
            , srcResTy
            ]
          ]

  source = vcat $
    [ text "#include" <+> doubleQuotes (text headerName)
    , text ""
    ] ++

    [ text "namespace" <+> text n <+> char '{' | n <- ns ] ++

    keyTyDecl ++

    [ text ""
    , typedef mapTy (text "transitions")
    , text ""
    , mkTable fsmNodes
    , text ""
    , clsName <> text "::" <> clsName
      <> text "() : state" <> parens (pp fsmInitial) <+> text "{}"
    , text ""
    , mkMoveFun clsName srcResTy fsmInputs
    , text ""
    , text (replicate (length ns) '}')
    , text ""
    ]

  mkLt n = text "this->" <> n' <+> char '<' <+> text "other." <> n'
    where
    n' = cppName n

mkInput :: (Name, VarInfo) -> Doc
mkInput (n, VarInfo { viType }) = hsep [ cppType viType <+> cppName n ]

mkEnums :: [EnumDef] -> [Doc]
mkEnums [] = []
mkEnums es =
  [ text "namespace enums" <+> char '{' ] ++

  [ nest 2 (mkEnum e) | e <- es ] ++

  [ text "}"
  , text ""
  ]

mkEnum :: EnumDef -> Doc
mkEnum EnumDef { eName, eCons } =
  vcat [ hang (text "enum" <+> cppName eName <+> char '{')
            2 (vcat (commas (map cppName eCons)))
       , text "};"
       , text "" ]


mkTable :: Map.Map Int Node -> Doc
mkTable ts = text "static" <+> decl (text "transitions") (text "table[]") table
  where
  table = vcat [ char '{' <+> nest 2 (vcat (commas [ mkTransition n | n <- Map.elems ts ]))
               , char '}' ]

  mkTransition self =
    vcat [ char '{' <+> nest 2 (vcat (commas (map mkEntry (nodeTrans self))))
         , char '}' ]

  mkEntry i =
    let Node { nodeInputs, nodeOutputs } = ts Map.! i
     in initializer [ mkInit nodeInputs
                    , initializer [ pp i , mkInit nodeOutputs ] ]

  mkInit is
    | Map.size is == 1 = cppValue (head (Map.elems is))
    | otherwise        = initializer (map cppValue (Map.elems is))

mkMoveFun :: Doc -> Doc -> StateVars -> Doc
mkMoveFun clsName resTy inputs =
  vcat [ fun resTy (char '&' <> clsName <> text "::move") params <+> char '{'
       , nest 2 $ vcat
         [ text "auto &ts = table[this->state];"
         , text "auto &next = ts.at" <> parens key <> semi
         , text "this->state = next.first;"
         , text "return next.second;"
         ]
       , char '}' ]

  where

  params = [ cppType (viType vi) <+> cppName n | (n,vi) <- Map.toList inputs ]

  key = initializer [ cppName n | n <- Map.keys inputs ]


-- Utilities -------------------------------------------------------------------

struct :: Doc -> [Doc] -> Doc
struct n def =
  vcat [ text "struct" <+> n <+> char '{'
       , nest 2 (vcat (semis def))
       , text "};" ]

cls :: Doc -> Doc -> Doc -> Doc
cls n pub priv =
  vcat [ text "class" <+> n <+> char '{'
       , nest 2 (vcat [ section "public" pub
                      , section "private" priv
                      ])
       , text "};"
       , text "" ]

  where
  section l d | isEmpty d = empty
              | otherwise = vcat [ text l <> char ':'
                                 , nest 2 d ]

-- | Function prototype.
fun :: Doc -> Doc -> [Doc] -> Doc
fun rty n params = rty <+> n <> parens (fsep (commas params))

decl :: Doc -> Doc -> Doc -> Doc
decl ty name ival
  | isEmpty ival = hdr <> semi
  | otherwise    = hang hdr 2 (char '=' <+> ival <> semi)
  where
  hdr = ty <+> name

typedef :: Doc -> Doc -> Doc
typedef ty new = text "typedef" <+> ty <+> new <> semi

template :: Doc -> [Doc] -> Doc
template n [] = n
template n ps = n <> angles (space <> fsep (commas ps) <> space)

initializer :: [Doc] -> Doc
initializer ds = braces (fsep (commas ds))

cppName :: Name -> Doc
cppName n = pp (fromMaybe (nameText n) (nameOutText n))

cppType :: VType -> Doc
cppType VTBool      = text "bool"
cppType (VTInt _ _) = text "int"
cppType (VTEnum e)  = cppEnumName (eName e)

cppValue :: Value -> Doc
cppValue (VBool True)  = text "true"
cppValue (VBool False) = text "false"
cppValue (VCon n)      = cppEnumName n
cppValue (VNum i)      = pp i

cppEnumName :: Name -> Doc
cppEnumName n = text "enums::" <> cppName n
