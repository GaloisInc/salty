{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.SPARK ( sparkFSM ) where

import           PP
import           Scope.Name
import           Slugs.FSM
import qualified TypeCheck.AST   as TC

import qualified Data.Char       as C
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import qualified Data.Text       as T

type Package = Map.Map FilePath Doc

data Mode = In
          | InOut
          | Out

data Annotation = Pre Doc
                | Post Doc
                | CC [(Doc,Doc)]
                | Ghost
                | TypeInvariant Doc

data SpecVar = SpecVar { svName  :: Doc
                       , svType  :: Doc
                       , svIsRec :: Bool
                       } deriving (Show)

sparkFSM :: FSM -> TC.Controller -> Package
sparkFSM FSM { fsmName, fsmEnums, fsmInputs, fsmOutputs, fsmInitial, fsmNodes } cont =
  Map.fromList [ (map C.toLower $ render pkgName ++ ".ads", spec), (map C.toLower $ render pkgName ++ ".adb", body) ]
  where
  pkgName = sparkName fsmName

  cntlrType = text "Controller"
  cntlrName = text "C"

  env = mkSpecVar fsmInputs "E" "Environment"
  sys = mkSpecVar fsmOutputs "S" "System"

  stType = text "State_Num"
  stName = text "State"

  nextPost = text "_Next"

  spec =
    vcat [ text "package" <+> pkgName <+> text "with SPARK_Mode is"
         , indent 2 $ doubleSpace pub
         , vcat [ PP.line <> text "private"
                , indent 2 $ doubleSpace priv ]
         , text "end" <+> pkgName <> semi ]
    where
    pub =
      [ statement $ declType cntlrType $ text "private"
      , vcat $ declEnums fsmEnums
      , optIOTypeDecl fsmInputs env
      , optIOTypeDecl fsmOutputs sys
      , statement $ func (text "Is_Init") [ declVar cntlrName cntlrType ] (sparkType VTBool)
      , eFunc (text "Env_Init")
          [ declVar (svName env) (svType env) ]
          (sparkType VTBool)
          (sparkExpr (fsmInputs, fsmOutputs) env sys nextPost $ map snd (TC.sEnvInit $ TC.cSpec cont))
          []
      , eFunc (text "Sys_Init")
          [ declVar (svName sys) (svType sys) ]
          (sparkType VTBool)
          (sparkExpr (fsmInputs, fsmOutputs) env sys nextPost $ map snd (TC.sSysInit $ TC.cSpec cont))
          [Ghost]
      , statement $ vcat [ func (text "Env_Trans")
                             [ declVar cntlrName cntlrType
                             , declVar (svName env) (svType env)]
                             (sparkType VTBool)
                         , annotate [ Pre $ text "not Is_Init" <+> parens cntlrName ] ]
      , statement $ vcat [ func (text "Sys_Trans")
                             [ declVar cntlrName cntlrType
                             , declVar (svName env) (svType env)
                             , declVar (svName sys) (svType sys) ]
                             (sparkType VTBool)
                         , annotate [ Pre $ text "not Is_Init" <+> parens cntlrName
                                    , Ghost ] ]
      , statement $ vcat [ proc (text "Move")
                             [ declParam cntlrName InOut cntlrType
                             , declParam (svName env) In (svType env)
                             , declParam (svName sys) Out (svType sys) ]
                         , annotate [ Pre $ text "if Is_Init" <+> parens cntlrName
                                        <+> text "then Env_Init" <+> parens (svName env)
                                        <+> text "else Env_Trans" <+> parens (cntlrName <> comma <+> svName env)
                                    , CC [ (text "Is_Init" <+> parens cntlrName,
                                              text "Sys_Init" <+> parens (svName sys) <+> text "and" <+> parens (text "not Is_Init" <+> parens cntlrName))
                                         , (text "others",
                                              text "Sys_Trans" <+> parens (cntlrName <> text "'Old" <> comma <+> svName env
                                              <> comma <+> svName sys) <+> text "and" <+> parens (text "not Is_Init" <+> parens cntlrName)) ] ] ] ]

    priv =
      [ statement $ vcat [ func (text "State_To_Input_Mapping")
                             [ declVar cntlrName cntlrType ]
                             (sparkType VTBool)
                         , annotate [ Ghost ] ]
      , statement $ vcat [ func (text "State_To_Output_Mapping")
                             [ declVar cntlrName cntlrType ]
                             (sparkType VTBool)
                         , annotate [ Ghost ] ]
      , statement $ declSubtype stType (text "Integer range 1 .." <+> pp (fsmInitial + 1))
      , statement $ vcat [ declRecord cntlrType
                             [ statement $ assign (declVar stName stType) (stType <> text "'Last")
                             , statement $ declVar (svName env) (svType env)
                             , statement $ declVar (svName sys) (svType sys) ]
                          , indent 2 $ annotate [ TypeInvariant $ text "State_To_Input_Mapping" <+> parens cntlrType
                                                    <+> text "and" <+> text "State_To_Output_Mapping" <+> parens cntlrType ] ] ]

  body =
    vcat [ text "package body" <+> pkgName <+> text "with SPARK_Mode is"
         , indent 2 $ doubleSpace defs
         , text "end" <+> pkgName <> semi ]
    where
    defs = [ statement $ vcat [ declType (text "SI_Equivalence_Array") (text "array" <+> parens (stType <> text "'First .." <+> stType <> text "'Last-1") <+> text "of" <+> svType env)
                              , annotate [ Ghost ] ]
           , statement $ vcat [ assign (declVar (text "State_To_Input_Equivalence") (text "constant SI_Equivalence_Array"))
                                  $ vcat [ PP.empty
                                         , indent 2 $ mkStateInputEq fsmNodes (svType env) ]
                              , annotate [ Ghost ] ]
           , statement $ vcat [ declType (text "SO_Equivalence_Array") (text "array" <+> parens (stType <> text "'First .." <+> stType <> text "'Last-1") <+> text "of" <+> svType sys)
                                     , annotate [ Ghost ] ]
           , statement $ vcat [ assign (declVar (text "State_To_Output_Equivalence") (text "constant SO_Equivalence_Array"))
                                  $ vcat [ PP.empty
                                         , indent 2 $ mkStateOutputEq fsmNodes (svType sys) ]
                              , annotate [ Ghost ] ]
           , eFunc (text "State_To_Input_Mapping")
               [ declVar cntlrName cntlrType ]
               (sparkType VTBool)
               (text "if" <+> qualify (Just cntlrName) stName <+> text "/=" <+> stType <> text "'Last then"
                  <+> qualify (Just cntlrName) (svName env) <+> text "= State_To_Input_Equivalence" <+> parens (qualify (Just cntlrName) stName))
               []
           , eFunc (text "State_To_Output_Mapping")
               [ declVar cntlrName cntlrType ]
               (sparkType VTBool)
               (text "if" <+> qualify (Just cntlrName) stName <+> text "/=" <+> stType <> text "'Last then"
                  <+> qualify (Just cntlrName) (svName sys) <+> text "= State_To_Output_Equivalence" <+> parens (qualify (Just cntlrName) stName))
               []
           , eFunc (text "Is_Init")
               [ declVar cntlrName cntlrType ]
               (sparkType VTBool)
               (qualify (Just cntlrName) stName <+> text "=" <+> stType <> text "'Last")
               []
           , eFunc (text "Env_Trans")
               [ declVars [ svName env, svName env <> nextPost ] (svType env)
               , declVar (svName sys) (svType sys) ]
               (sparkType VTBool)
               (sparkExpr (fsmInputs, fsmOutputs) env sys nextPost $ map snd (TC.sEnvTrans $ TC.cSpec cont))
               []
           , eFunc (text "Env_Trans")
               [ declVar cntlrName cntlrType
               , declVar (svName env) (svType env) ]
               (sparkType VTBool)
               (text "Env_Trans" <+> parens (fsep $ punctuate comma
                 [ qualify (Just cntlrName) (svName env)
                 , svName env
                 , qualify (Just cntlrName) (svName sys) ]))
               []
           , eFunc (text "Sys_Trans")
               [ declVars [ svName env, svName env <> nextPost ] (svType env)
               , declVars [ svName sys, svName sys <> nextPost ] (svType sys) ]
               (sparkType VTBool)
               (sparkExpr (fsmInputs, fsmOutputs) env sys nextPost $ map snd (TC.sSysTrans $ TC.cSpec cont))
               []
           , eFunc (text "Sys_Trans")
               [ declVar cntlrName cntlrType
               , declVar (svName env) (svType env)
               , declVar (svName sys) (svType sys) ]
               (sparkType VTBool)
               (text "Sys_Trans" <+> parens (fsep $ punctuate comma
                 [ qualify (Just cntlrName) (svName env)
                 , svName env
                 , qualify (Just cntlrName) (svName sys)
                 , svName sys ]))
               []
           , vcat [ proc (text "Move")
                      [ declParam cntlrName InOut cntlrType
                      , declParam (svName env) In (svType env)
                      , declParam (svName sys) Out (svType sys) ]
                  , subBody (text "Move")
                      [ ]
                      [ mkNested fsmNodes (qualify (Just cntlrName) stName)
                      , statement $ assign (qualify (Just cntlrName) (svName env)) (svName env)
                      , statement $ assign (svName sys) (qualify (Just cntlrName) (svName sys)) ] ] ]

sparkName :: Name -> Doc
sparkName n = deconflict name
  where
  name = fromMaybe (nameText n) (nameOutText n)

  reserved = ["abort", "abs", "abstract", "accept", "access", "aliased", "all", "and", "array", "at", "begin", "body",
              "case", "constant", "declare", "delay", "delta", "digits", "do", "else", "elsif", "end", "entry",
              "exception", "exit", "for", "function", "generic", "goto", "if", "in", "interface", "is", "limited",
              "loop", "mod", "new", "not", "null", "of", "or", "others", "out", "overriding", "package", "pragma",
              "private", "procedure", "protected", "raise", "range", "record", "rem", "renames", "requeue", "return",
              "reverse", "select", "separate", "some", "subtype", "synchronized", "tagged", "task", "terminate",
              "then", "type", "until", "use", "when", "while", "with", "xor"]

  --TODO: handle clashing with boilerplate generated identifiers

  deconflict n'
    | map C.toLower (T.unpack n') `elem` reserved = pp n' <> text "_N" --TODO: Capitalization
    | otherwise = pp n'

sparkType :: VType -> Doc
sparkType VTBool      = text "Boolean"
sparkType (VTInt l h) = text "Integer range" <+> pp l <+> text ".." <+> pp h
sparkType (VTEnum e)  = sparkName (TC.eName e) <> text "_Type" --TODO: Capitalization

sparkValue :: Value -> Doc
sparkValue (VBool True)  = text "True"
sparkValue (VBool False) = text "False"
sparkValue (VCon n)      = sparkName n
sparkValue (VNum i)      = pp i

doubleSpace :: [Doc] -> Doc
doubleSpace xs =
  concatWith (surround (PP.line <> PP.line)) (filter (not . isEmpty) xs)

declType :: Doc -> Doc -> Doc
declType n t = text "type" <+> n <+> text "is" <+> t

declSubtype :: Doc -> Doc -> Doc
declSubtype n t = text "sub" <> declType n t

declEnums :: [TC.EnumDef] -> [Doc]
declEnums [] = []
declEnums es = punctuate hardline [ declEnum e | e <- es ]
  where
  declEnum TC.EnumDef { TC.eName, TC.eCons } =
    statement $ declType (sparkName eName <> text "_Type") (parens . align . hcat $ punctuate (comma <> space) (map sparkName eCons))

mkSpecVar :: StateVars -> String -> String -> SpecVar
mkSpecVar v n t
  | Map.size v > 1 =
      SpecVar { svName = text n, svType = text t, svIsRec = True }
  | otherwise =
      SpecVar { svName = n', svType = t', svIsRec = False }
        where
        n' = sparkName . head $ Map.keys v
        t' =
          case viType $ head $ Map.elems v of
            VTInt _ _ -> n' <> text "_Type" --TODO: Capitalization
            ty        -> sparkType ty

optIOTypeDecl :: StateVars -> SpecVar -> Doc
optIOTypeDecl vars v
  | svIsRec v =
      statement $ declRecord (svType v)
                    [ statement $ declVar (sparkName n) (sparkType viType) | (n, VarInfo { viType }) <- Map.toList vars ]
  | otherwise =
      case viType $ head $ Map.elems vars of
        (VTInt _ _) -> statement $ declSubtype (svType v) (sparkType $ viType (head $ Map.elems vars))
        _           -> PP.empty

declRecord :: Doc -> [Doc] -> Doc
declRecord name fs =
  declType name $ vcat [ text ""
                       , indent 2 $ vcat [ text "record"
                                         , indent 2 $ vcat fs
                                         , text "end record" ] ]

declVar :: Doc -> Doc -> Doc
declVar name ty = name <> colon <+> ty

declVars :: [Doc] -> Doc -> Doc
declVars ns = declVar (fsep $ punctuate comma ns)

qualify :: Maybe Doc -> Doc -> Doc
qualify pre name =
  case pre of
    Just p  -> p <> text "." <> name
    Nothing -> name

statement :: Doc -> Doc
statement e = e <> semi

assign :: Doc -> Doc -> Doc
assign l r = l <+> text ":=" <+> r

mode :: Mode -> Doc
mode m =
  case m of
    In    -> text "in"
    InOut -> text "in out"
    Out   -> text "out"

declParam :: Doc -> Mode -> Doc -> Doc
declParam name m t = name <> colon <+> mode m <+> t

annotation :: Annotation -> Doc
annotation a =
  case a of
    Pre e           -> text "Pre" <+> text "=>" <+> parens e
    Post e          -> text "Post" <+> text "=>" <+> parens e
    CC cs           -> vcat [ text "Contract_Cases" <+> text "=>"
                            , indent 2 $ parens $ vcat $ punctuate comma [ pre <+> text "=>" <+> post | (pre, post) <- cs ] ]
    Ghost           -> text "Ghost"
    TypeInvariant e -> text "Type_Invariant" <+> text "=>" <+> parens e

annotate :: [Annotation] -> Doc
annotate [] = PP.empty
annotate as = vcat [ text "with"
                   , indent 2 $ vcat $ punctuate comma $ map annotation as]

func :: Doc -> [Doc] -> Doc -> Doc
func name [] ret =
  text "function" <+> name <+> text "return" <+> ret
func name ps ret =
  text "function" <+> name <+> (parens . fsep $ punctuate semi ps) <+> text "return" <+> ret

eFunc :: Doc -> [Doc] -> Doc -> Doc -> [Annotation] -> Doc
eFunc name ps ret e as =
  statement $ vcat $ [ func name ps ret
                     , text "is"
                     , indent 2 $ parens e ] ++ annotations
  where
  annotations =
    case as of
      [] -> []
      _  -> [ annotate as ]

proc :: Doc -> [Doc] -> Doc
proc name ps =
  text "procedure" <+> name <+> (parens . fsep $ punctuate semi ps)

subBody :: Doc -> [Doc] -> [Doc] -> Doc
subBody name vs stmts =
  vcat $ [ text "is" ] ++
         ( case vs of
             [] -> []
             xs -> [ indent 2 $ vcat xs ] ) ++
         [ text "begin"
         , indent 2 $ vcat stmts
         , text "end" <+> name <> semi ]

-- TODO: verify correct precedence
sparkExpr :: (StateVars,StateVars) -> SpecVar -> SpecVar -> Doc -> [TC.Expr] -> Doc
sparkExpr (fsmInputs,_) env sys next es =
  conj $ map mkExpr es
  where
  conj []  = sparkValue (VBool True)
  conj [d] = d
  conj ds  = foldr1 (\l r -> l <+> text "and" <+> r) ds

  unaryOp op a = op <+> parens (mkExpr a)

  binaryOp op a b = parens $ mkExpr a <+> op <+> mkExpr b

  mkExpr TC.ETrue = sparkValue (VBool True)
  mkExpr TC.EFalse = sparkValue (VBool False)
  -- TODO: idiomatic simplifications?
  mkExpr (TC.EEq _ a TC.ETrue) = mkExpr a
  mkExpr (TC.EEq _ a TC.EFalse) = mkExpr (TC.ENot a)
  mkExpr (TC.EEq _ a b) = binaryOp (text "=") a b

  mkExpr (TC.ENot a) = unaryOp (text "not") a
  mkExpr (TC.EAnd a b) = binaryOp (text "and") a b
  mkExpr (TC.EOr a b) = binaryOp (text "or") a b
  mkExpr (TC.EXor a b) = binaryOp (text "xor") a b

  mkExpr (TC.ENum i) = pp i
  mkExpr (TC.EPlus a b) = binaryOp (text "+") a b

  mkExpr (TC.EVar _ v) =
    qualify (if svIsRec pre then Just (svName pre) else Nothing) $ sparkName v
    where
    pre = if Map.member v fsmInputs then env else sys

  mkExpr (TC.ENext _ (TC.EVar _ v)) =
    if svIsRec pre then
      qualify (Just $ svName pre <> next) $ sparkName v
    else
      sparkName v <> next
    where
    pre = if Map.member v fsmInputs then env else sys

  mkExpr (TC.ECon _ n) = sparkName n

  mkExpr e = text "UNDEFINED" <+> pp (show e)

mkStateInputEq :: Map.Map Int Node -> Doc -> Doc
mkStateInputEq ns env =
  parens $ vcat $ punctuate comma
    [ mkEntry nodeInputs | (_, Node { nodeInputs }) <- take (Map.size ns - 1) (Map.toList ns) ]
  where
  mkEntry inputs = env <> text "'" <> parens (fsep $ punctuate comma
    [ sparkName n <+> text "=>" <+> sparkValue v | (n,v) <- Map.toList inputs ])

mkStateOutputEq :: Map.Map Int Node -> Doc -> Doc
mkStateOutputEq ns sys =
  parens $ vcat $ punctuate comma
    [ mkEntry nodeOutputs | (_, Node { nodeOutputs }) <- take (Map.size ns - 1) (Map.toList ns) ]
  where
  mkEntry outputs = sys <> text "'" <> parens (fsep $ punctuate comma
    [ sparkName n <+> text "=>" <+> sparkValue v | (n,v) <- Map.toList outputs ])

mkNested :: Map.Map Int Node -> Doc -> Doc
mkNested ns st = vcat [ mkCase st [(pp (s + 1),n) | (s,n) <- Map.toList ns ] ]
-- TODO: recursive? enumerate all input combinations & assign outputs
  where
  mkCase var whens = vcat [ text "case" <+> var <+> text "is"
                          , indent 2 $ vcat [ vcat [ text "when" <+> val <+> text "=>"
                                                   , indent 2 (text "test") ] | (val, _) <- whens ]
                                                   -- , indent 2 ex ] | (val, ex) <- whens ] ]
                          , statement $ text "end case" ]

mkTable :: Map.Map Int Node -> Doc
mkTable ts = vcat [ vcat $ map (mkEntry s) (nodeTrans n) | (s,n) <- Map.toList ts ]
  where
  mkEntry s i =
    let Node { nodeInputs, nodeOutputs } = ts Map.! i
      in statement $ text "Transition_Maps.Insert" <+>
           tuple [ text "Transitions" <> parens (pp (s + 1))
                 , mkVal nodeInputs
                 , tuple [ pp (i + 1), mkVal nodeOutputs ] ]

  tuple ds = parens . fsep $ commas ds

  mkVal is
    | Map.size is == 1 = sparkValue . head $ Map.elems is
    | otherwise        = tuple $ map sparkValue $ Map.elems is

mkTablePost :: Map.Map Int Node -> Doc -> Doc
mkTablePost ts t = vcat [ vcat [ vcat $ map (mkContains s) (nodeTrans n) | (s,n) <- Map.toList ts ]
                        , text ""
                        , vcat [ vcat $ map (mkElement s) (nodeTrans n) | (s,n) <- Map.toList ts ] ]
  where
  mkContains s i =
    let Node { nodeInputs } = ts Map.! i
      in text "and Transition_Maps.Contains" <+>
           tuple [ text "Init'Result" <> parens (pp (s + 1))
                 , mkVal nodeInputs ]

  mkElement s i =
    let Node { nodeInputs, nodeOutputs } = ts Map.! i
      in text "and" <+> parens (text "Transition_Maps.Element" <+>
           tuple [ text "Init'Result" <> parens (pp (s + 1))
                 , t <> text "'" <> parens (mkVal nodeInputs) ]
           <+> text "=" <+> tuple [ pp (i + 1), mkVal nodeOutputs ])

  tuple ds = parens . fsep $ commas ds

  mkVal is
    | Map.size is == 1 = sparkValue . head $ Map.elems is
    | otherwise        = tuple $ map sparkValue $ Map.elems is
