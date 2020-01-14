{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.SPARK ( sparkFSM ) where

import           PP
import           Scope.Name
import           Slugs.FSM
import qualified TypeCheck.AST   as TC

import qualified Data.Char       as C
import           Data.Function   (on)
import           Data.List       (sortBy)
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
                                         , indent 2 $ mkStateInputEq fsmNodes env ]
                              , annotate [ Ghost ] ]
           , statement $ vcat [ declType (text "SO_Equivalence_Array") (text "array" <+> parens (stType <> text "'First .." <+> stType <> text "'Last-1") <+> text "of" <+> svType sys)
                                     , annotate [ Ghost ] ]
           , statement $ vcat [ assign (declVar (text "State_To_Output_Equivalence") (text "constant SO_Equivalence_Array"))
                                  $ vcat [ PP.empty
                                         , indent 2 $ mkStateOutputEq fsmNodes sys ]
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
                      [ mkNested fsmNodes cntlrName stName env sys
                      , statement $ assign (qualify (Just cntlrName) (svName env)) (svName env)
                      , statement $ assign (svName sys) (qualify (Just cntlrName) (svName sys)) ] ] ]

-- NOTE: Ada is case-insensitive
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

  gen = ["c", "controller", "e", "environment", "s", "system", "state", "state_Num"]

  deconflict n'
    | map C.toLower (T.unpack n') `elem` reserved = pp n' <> text "_N"
    | map C.toLower (T.unpack n') `elem` gen = pp n' <> text "_N"
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
  | Map.size v == 0 =
      SpecVar { svName = text n, svType = text t, svIsRec = False }
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
  | Map.size vars == 0 =
      statement $ declSubtype (svType v) (sparkType VTBool)
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

mkStateInputEq :: Map.Map Int Node -> SpecVar -> Doc
mkStateInputEq ns env =
  parens $ vcat $ punctuate comma
    [ mkVarVals nodeInputs env | (_, Node { nodeInputs }) <- take (Map.size ns - 1) (Map.toList ns) ]

mkStateOutputEq :: Map.Map Int Node -> SpecVar -> Doc
mkStateOutputEq ns sys =
  parens $ vcat $ punctuate comma
    [ mkVarVals nodeOutputs sys | (_, Node { nodeOutputs }) <- take (Map.size ns - 1) (Map.toList ns) ]

mkVarVals :: Map.Map Name Value -> SpecVar -> Doc
mkVarVals vars SpecVar { svType, svIsRec = True } =
  svType <> text "'" <> parens (fsep $
    punctuate comma
      [ sparkName n <+> text "=>" <+> sparkValue v | (n,v) <- Map.toList vars ])
mkVarVals vars _
  | Map.size vars == 0 =
      sparkValue (VBool True)
  | otherwise =
      sparkValue $ snd $ head $ Map.toList vars

-- NOTE: assumes inputs are ordered the same across nodes
mkNested :: Map.Map Int Node -> Doc -> Doc -> SpecVar -> SpecVar -> Doc
mkNested ns cntlrName stName env sys =
  vcat [ text "case" <+> qualify (Just cntlrName) stName <+> text "is"
       , indent 2 $ vcat [ vcat [ text "when" <+> pp (s+1) <+> text "=>"
                                , indent 2 $ mkNested' ns cntlrName stName env sys (sort nodeTrans) [] False PP.empty ]
                           | (s, Node { nodeTrans }) <- Map.toList ns ]
       , statement $ text "end case" ]
  where
  sort ts = map fst $
              sortBy (compare `on` snd)
                (zip ts
                  [ map snd (Map.toList xs) |
                    xs <- [ nodeInputs | Node { nodeInputs } <- map (ns Map.!) ts ] ])

mkNested' :: Map.Map Int Node -> Doc -> Doc -> SpecVar -> SpecVar -> [Int] -> [Value] -> Bool -> Doc -> Doc
mkNested' _ _ _ _ _ [] [] _ d = d
mkNested' ns cntlrName stName env sys [] p _ d =
  mkNested' ns cntlrName stName env sys [] (init p) True $
    vcat [ d
         , indent ((length p - 1) * 4) $ vcat
           [ indent 2 $ vcat
             [ text "when others =>"
             , indent 2 (statement $ text "raise Program_Error") ]
           , statement $ text "end case" ] ]
mkNested' ns cntlrName stName env sys ts p b d =
  let Node { nodeInputs, nodeOutputs } = ns Map.! head ts
    in mkCase (Map.toList nodeInputs) nodeOutputs
  where
  values = map snd

  mkVarName SpecVar { svIsRec = False } n = n
  mkVarName SpecVar { svName } n = qualify (Just svName) n

  getNonMatching [] = []
  getNonMatching [_] = []
  getNonMatching (_:ts') =
    let Node { nodeInputs } = ns Map.! head ts'
      in (if values (Map.toList nodeInputs) == p then getNonMatching (tail ts') else ts')

  mkCase xs nodeOutputs
    | values xs == p = -- matches prefix (leaf)
        mkNested' ns cntlrName stName env sys (getNonMatching ts) p False $
          vcat [ d
               , indent (length p * 4) $ vcat $
                 statement
                    (assign (qualify (Just cntlrName) stName) (pp (head ts + 1)))
                 : [ statement $
                       assign
                         (qualify (Just cntlrName) (mkVarName sys (sparkName n)))
                         (sparkValue v)
                     | (n,v) <- Map.toList nodeOutputs ] ]
    | take (length p) (values xs) == p = -- matches prefix
        let when = indent 2 $ text "when" <+> sparkValue (snd $ xs !! length p) <+> text "=>"
          in mkNested' ns cntlrName stName env sys ts (take (length p + 1) (values xs)) False $
               vcat $
                 (if PP.isEmpty d then [] else [d]) ++
                   [ indent (length p * 4)
                       (if not b -- descending
                         then vcat [ text "case" <+> mkVarName env (sparkName (fst $ xs !! length p)) <+> text "is"
                                   , when ]
                         else when) ]
    | b = -- doesn't match prefix (backtracking)
        mkNested' ns cntlrName stName env sys ts (init p) True $
          vcat [ d
               , indent (length p * 4) $ vcat
                 [ indent 2 $ vcat
                   [ text "when others =>"
                   , indent 2 (statement $ text "raise Program_Error") ]
                 , statement $ text "end case" ] ]
    | otherwise = -- doesn't match prefix (leaf)
        mkNested' ns cntlrName stName env sys ts (init p) True d
