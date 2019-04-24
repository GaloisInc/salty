{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.SPARK ( sparkFSM ) where

import           PP
import           Scope.Name
import           Slugs.FSM
import           TypeCheck.AST

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)

type Package = Map.Map FilePath Doc

data Mode = In
          | InOut
          | Out

data Annotation = Pre Doc
                | Post Doc
                | CC [(Doc,Doc)]
                | Ghost

sparkFSM :: FSM -> Package
sparkFSM FSM { fsmName, fsmEnums, fsmInputs, fsmOutputs, fsmInitial, fsmNodes } =
  Map.fromList [ (render pkgName ++ ".ads", spec), (render pkgName ++ ".adb", body) ]
  where
  pkgName = sparkName fsmName

  spec =
    vcat [ text "package" <+> pkgName <+> text "with SPARK_Mode is"
         , indent 2 $ doubleSpace pub
         , vcat [ PP.line <> text "private"
                , indent 2 $ doubleSpace priv ]
         , text "end" <+> pkgName <> semi ]
    where
    pub =
      [ declType cntlrType $ text "private"
      , vcat $ declEnums fsmEnums
      , optRecordTyDecl fsmInputs envType
      , optRecordTyDecl fsmOutputs sysType
      , statement $ function (text "Is_Init") [ declVar cntlrName cntlrType ] (sparkType VTBool)
      , expFunction (text "Env_Init")
          [ declVar envName (optRecordTy fsmInputs envType) ]
          (sparkType VTBool)
          (parens $ text "expression") -- TODO
          []
      , expFunction (text "Sys_Init")
          [ declVar sysName (optRecordTy fsmOutputs sysType) ]
          (sparkType VTBool)
          (parens $ text "expression") -- TODO
          [Ghost]
      , statement $ vcat $ function (text "Env_Trans")
                             [ declVar cntlrName cntlrType
                             , declVar envName (optRecordTy fsmInputs envType)]
                             (sparkType VTBool)
                           : annotate [ Pre $ text "not Is_Init" <+> parens cntlrName ]
      , statement $ vcat $ function (text "Sys_Trans")
                             [ declVar cntlrName cntlrType
                             , declVar envName (optRecordTy fsmInputs envType)
                             , declVar sysName (optRecordTy fsmOutputs sysType) ]
                             (sparkType VTBool)
                           : annotate [ Pre $ text "not Is_Init" <+> parens cntlrName
                                      , Ghost ]
      , statement $ vcat $
          procedure (text "Move")
            [ declParam cntlrName InOut cntlrType
            , declParam envName In (optRecordTy fsmInputs envType)
            , declParam sysName Out (optRecordTy fsmOutputs sysType) ]
          : annotate [ CC [ (text "Is_Init" <+> parens cntlrName,
                               parens $ text "if Env_Init" <+> parens envName <+> text "then Sys_Init"
                               <+> parens sysName <+> text "and" <+> parens (text "not Is_Init" <+> parens cntlrName))
                          , (text "others",
                               parens $ text "if Env_Trans" <+> parens (cntlrName <> text "'Old" <> comma <+> envName)
                               <+> text "then Sys_Trans" <+> parens (cntlrName <> text "'Old" <> comma <+> envName
                               <> comma <+> sysName) <+> text "and" <+> parens (text "not Is_Init" <+> parens cntlrName)) ] ] ]

    priv =
      [ declSubtype stType (text "Integer range 1.." <> pp (fsmInitial + 1))
      , declRecord cntlrType [ assign (declVar stName stType) (stType <> text "'Last")
                             , statement $ declVar envName envType
                             , statement $ declVar sysName sysType ] ]

  cntlrType = text "Controller"

  cntlrName = text "C"

  envType = text "Environment"

  envName = optRecordName fsmInputs (text "E")

  stType = text "State_Num"

  stName = text "State"

  sysType = text "System"

  sysName = optRecordName fsmOutputs (text "S")

  body =
    vcat [ text "package body" <+> pkgName <+> text "with SPARK_Mode is"
         , nest 2 $ text "-- generated"
         , mkTable fsmNodes
         , text "end" <+> pkgName <> semi ]

sparkName :: Name -> Doc
sparkName n = pp $ fromMaybe (nameText n) (nameOutText n)

sparkType :: VType -> Doc
sparkType VTBool      = text "Boolean"
sparkType (VTInt l h) = text "Integer range" <+> pp l <+> text ".." <+> pp h
sparkType (VTEnum e)  = sparkName $ eName e

sparkValue :: Value -> Doc
sparkValue (VBool True)  = text "True"
sparkValue (VBool False) = text "False"
sparkValue (VCon n)      = sparkName n
sparkValue (VNum i)      = pp i

doubleSpace :: [Doc] -> Doc
doubleSpace xs =
  concatWith (surround (PP.line <> PP.line)) (filter (not . isEmpty) xs)

declType :: Doc -> Doc -> Doc
declType n t = text "type" <+> n <+> text "is" <+> t <> semi

declSubtype :: Doc -> Doc -> Doc
declSubtype n t = text "sub" <> declType n t

declEnums :: [EnumDef] -> [Doc]
declEnums [] = []
declEnums es = punctuate hardline [ declEnum e | e <- es ]
  where
  declEnum EnumDef { eName, eCons } =
    declType (sparkName eName) (parens . align . hcat $ punctuate (comma <> space) (map sparkName eCons))

optRecordTyDecl :: StateVars -> Doc -> Doc
optRecordTyDecl vars t
  | Map.size vars == 1 = PP.empty
  | otherwise          =
      declRecord t
        [ statement $ declVar (sparkName n) (sparkType viType) | (n, VarInfo { viType }) <- Map.toList vars ]

optRecordTy :: StateVars -> Doc -> Doc
optRecordTy vars t
  | Map.size vars > 1 = t
  | otherwise         = sparkType . viType . head $ Map.elems vars

optRecordName :: StateVars -> Doc  -> Doc
optRecordName vars name
  | Map.size vars > 1 = name
  | otherwise         = sparkName . head $ Map.keys vars

declRecord :: Doc -> [Doc] -> Doc
declRecord name fs =
  declType name $ vcat [ text ""
                       , indent 2 $ vcat [ text "record"
                                         , indent 2 $ vcat fs
                                         , text "end record" ] ]

declVar :: Doc -> Doc -> Doc
declVar name ty = name <> colon <+> ty

statement :: Doc -> Doc
statement e = e <> semi

assign :: Doc -> Doc -> Doc
assign l r = statement $ l <+> text ":=" <+> r

mode :: Mode -> Doc
mode m =
  case m of
    In    -> text "in"
    InOut -> text "in out"
    Out   -> text "out"

declParam :: Doc -> Mode -> Doc -> Doc
declParam name m t = name <> colon <+> mode m <+> t

-- TODO: alignment in contract cases
annotation :: Annotation -> Doc
annotation a =
  case a of
    Pre e  -> text "Pre" <+> text "=>" <+> parens e
    Post e -> text "Post" <+> text "=>" <+> parens e
    CC cs  -> vcat [ text "Contract_Cases" <+> text "=>"
                   , indent 2 $ parens $ vcat $ punctuate comma [ pre <+> text "=>" <+> post | (pre, post) <- cs ] ]
    Ghost  -> text "Ghost" <+> text "=>" <+> sparkValue (VBool True)

-- TODO: alignment
annotate :: [Annotation] -> [Doc]
annotate [] = []
annotate as = [ text "with"
              , indent 2 $ vcat $ punctuate comma $ map annotation as]

function :: Doc -> [Doc] -> Doc -> Doc
function name ps ret =
  text "function" <+> name <+> (parens . fsep $ punctuate semi ps) <+> text "return" <+> ret

expFunction :: Doc -> [Doc] -> Doc -> Doc -> [Annotation] -> Doc
expFunction name ps ret e as =
  statement . vcat $
    vcat [ function name ps ret
         , text "is"
         , indent 2 e ]
    : annotate as

procedure :: Doc -> [Doc] -> Doc
procedure name ps =
  text "procedure" <+> name <+> (parens . fsep $ punctuate semi ps)

mkTable :: Map.Map Int Node -> Doc
mkTable ts = nest 2 $ vcat [text "", table]
  where
  table = vcat [ vcat $ text "" : map (mkEntry s) (nodeTrans n) | (s,n) <- Map.toList ts ]

  mkEntry s i =
    let Node { nodeInputs, nodeOutputs } = ts Map.! i
      in text "Transition_Maps.Insert"
         <+> initializer [ text "Transitions" <> parens (pp (s + 1))
                         , mkInit nodeInputs
                         , initializer [ pp (i + 1), mkInit nodeOutputs ] ]
         <> semi

  initializer ds = parens . fsep $ commas ds

  mkInit is
    | Map.size is == 1 = sparkValue . head $ Map.elems is
    | otherwise        = initializer $ map sparkValue $ Map.elems is
