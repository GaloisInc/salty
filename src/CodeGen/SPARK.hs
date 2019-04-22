{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.SPARK ( sparkFSM ) where

import PP
import Scope.Name
import Slugs.FSM
import TypeCheck.AST

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

type Package = Map.Map FilePath Doc

data Mode = In
          | InOut
          | Out
          deriving (Show)

sparkFSM :: FSM -> Package
sparkFSM FSM { fsmName, fsmEnums, fsmInputs, fsmOutputs, fsmInitial, fsmNodes } =
  Map.fromList [ ((render pkgName) ++ ".ads", spec), ((render pkgName) ++ ".adb", body) ]

  where

  pkgName = sparkName fsmName

  spec = packageSpec pkgName

    (vcat $
      [ declType cntlrType (text "private")
      , text ""
      ] ++
      (declEnums fsmEnums) ++
      -- TODO: subtype declarations for unique input integer ranges (how to name)? pre-conditions?
      resTyDecl ++
      [ text ""
      , declProc (text "Move") -- TODO: 1) consider line breaks to separate adt, params, result 2) consolidating fsmInputs by type
          ([ declParam cntlrName InOut cntlrType ] ++
           [ declParam (sparkName n) In (sparkType viType) | (n, VarInfo { viType }) <- Map.toList fsmInputs ] ++
           [ declParam resName Out resTy])
      ])

    (vcat [ declSubtype stType (text "Integer range 1 .." <+> pp (fsmInitial + 1))
          , text ""
          , declRecord cntlrType [ stName <> colon <+> stType ]
          , text ""
          , statement (cntlrName <> colon <+> cntlrType) (namedAgg [ (stName, pp $ fsmInitial + 1) ])
          ])

  cntlrType = text "Controller_Type"

  cntlrName = text "Controller"

  stType = text "State_Type"

  stName = text "State"

  resType = text "Result_Type"

  resName = text "Result"

  resTyDecl
    | Map.size fsmOutputs == 1 = []
    | otherwise                =
      [ text ""
      , declRecord resType
        [ sparkName n <> colon <+> sparkType viType | (n, VarInfo { viType }) <- Map.toList fsmOutputs ]
      ]

  resTy
    | Map.size fsmOutputs > 1  = resType
    | otherwise                = sparkType . viType . head $ Map.elems fsmOutputs

  body = vcat $
    [ text "package body" <+> pkgName <+> text "with SPARK_Mode is"
    , nest 2 (text "-- generated")
    ] ++

    [ text "end" <+> pkgName <> semi ]

sparkName :: Name -> Doc
sparkName n = pp $ fromMaybe (nameText n) (nameOutText n)

sparkType :: VType -> Doc
sparkType VTBool      = text "Boolean"
sparkType (VTInt _ _) = text "Integer"
sparkType (VTEnum e)  = sparkName $ eName e

packageSpec :: Doc -> Doc -> Doc -> Doc
packageSpec n vis priv =
  vcat [ text "package" <+> n <+> text "with SPARK_Mode is"
       , indent 2 vis
       , section "private" priv
       , text "end" <+> n <> semi
       ]
  where
  section l d
    | isEmpty d = empty
    | otherwise = vcat [ text ""
                       , text l
                       , indent 2 d ]

declType :: Doc -> Doc -> Doc
declType n ty = text "type" <+> n <+> text "is" <+> ty <> semi

declSubtype :: Doc -> Doc -> Doc
declSubtype n ty = text "sub" <> (declType n ty)

declEnums :: [EnumDef] -> [Doc]
declEnums [] = []
declEnums es = punctuate hardline [ declEnum e | e <- es ]
  where
  declEnum EnumDef { eName, eCons } =
    (declType (sparkName eName) (parens . align . hcat $ punctuate (comma <> space) (map sparkName eCons)))

declRecord :: Doc -> [Doc] -> Doc
declRecord n def =
  declType n
    (vcat [ text ""
          , (indent 2 $ vcat [ text "record"
                             , indent 2 (vcat $ semis def)
                             , text "end record"
                             ])
         ])

mode :: Mode -> Doc
mode m =
  case m of In    -> text "in"
            InOut -> text "in out"
            Out   -> text "out"

declParam :: Doc -> Mode -> Doc -> Doc
declParam n m ty = n <> colon <+> mode m <+> ty

-- declFun :: Doc -> [Doc] -> Doc -> Doc
-- declFun n params rtnTy =
--   text "function" <+> n <+> parens (fsep (punctuate semi params)) <+> text "return" <+> rtnTy <> semi

declProc :: Doc -> [Doc] -> Doc
declProc n params =
  text "procedure" <+> n <+> parens (fsep (punctuate semi params)) <> semi

namedAgg :: [(Doc, Doc)] -> Doc
namedAgg ev = agg $ [ e <+> text "=>" <+> v | (e, v) <- ev]

agg :: [Doc] -> Doc
agg cs = parens . hsep $ commas cs

statement :: Doc -> Doc -> Doc
statement l r = l <+> text ":=" <+> r <> semi