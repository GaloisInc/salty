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

  outType = text "Output"

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
      , statement $ vcat $ func (text "Env_Trans")
                             [ declVar cntlrName cntlrType
                             , declVar (svName env) (svType env)]
                             (sparkType VTBool)
                           : annotate [ Pre $ text "not Is_Init" <+> parens cntlrName ]
      , statement $ vcat $ func (text "Sys_Trans")
                             [ declVar cntlrName cntlrType
                             , declVar (svName env) (svType env)
                             , declVar (svName sys) (svType sys) ]
                             (sparkType VTBool)
                           : annotate [ Pre $ text "not Is_Init" <+> parens cntlrName
                                      , Ghost ]
      , statement $ vcat $
          proc (text "Move")
            [ declParam cntlrName InOut cntlrType
            , declParam (svName env) In (svType env)
            , declParam (svName sys) Out (svType sys) ]
          : annotate [ CC [ (text "Is_Init" <+> parens cntlrName,
                               parens $ text "if Env_Init" <+> parens (svName env) <+> text "then Sys_Init"
                               <+> parens (svName sys) <+> text "and" <+> parens (text "not Is_Init" <+> parens cntlrName))
                          , (text "others",
                               parens $ text "if Env_Trans" <+> parens (cntlrName <> text "'Old" <> comma <+> svName env)
                               <+> text "then Sys_Trans" <+> parens (cntlrName <> text "'Old" <> comma <+> svName env
                               <> comma <+> svName sys) <+> text "and" <+> parens (text "not Is_Init" <+> parens cntlrName)) ] ] ]

    priv =
      [ statement $ vcat $ func (text "State_To_Input_Mapping")
                             [ declVar cntlrName cntlrType ]
                             (sparkType VTBool)
                           : annotate [ Ghost ]
      , statement $ vcat $ func (text "State_To_Output_Mapping")
                             [ declVar cntlrName cntlrType ]
                             (sparkType VTBool)
                           : annotate [ Ghost ]
      , statement $ declSubtype stType (text "Integer range 1.." <> pp (fsmInitial + 1))
      , statement $ vcat [ declRecord cntlrType
                             [ assign (declVar stName stType) (stType <> text "'Last")
                             , statement $ declVar (svName env) (svType env)
                             , statement $ declVar (svName sys) (svType sys) ]
                          , indent 2 $ vcat $ annotate [ TypeInvariant $ text "State_To_Input_Mapping" <+> parens cntlrType
                                                           <+> text "and" <+> text "State_To_Output_Mapping" <+> parens cntlrType ] ] ]

  body =
    vcat [ text "with Ada.Containers; use Ada.Containers;"
         , text "with Ada.Containers.Formal_Hashed_Maps;"
         , text ""
         , text "package body" <+> pkgName <+> text "with SPARK_Mode is"
         , indent 2 $ doubleSpace defs
         , text "end" <+> pkgName <> semi ]
    where
    defs = [ statement $ declRecord outType [ statement $ declVar stName stType
                                            , statement $ declVar (svName sys) (svType sys) ]
           , vcat [ func (text "Hash") [ declVar (svName env) (svType env) ] (text "Ada.Containers.Hash_Type")
                  , subBody (text "Hash")
                      [ assign (declVar (text "Hash") (text "Hash_Type")) (text "17") ]
                      [ vcat [ assign (text "Hash")
                                 (text "Hash * 31 + Hash_Type" <+> parens (sparkRootType viType <> text "'Pos" <+>
                                    parens (qualify (if Map.size fsmInputs > 1 then Just (svName env) else Nothing) (sparkName n))))
                                 | (n, VarInfo { viType }) <- Map.toList fsmInputs ]
                      , text "return Hash;" ] ]
           , let comp | Map.size fsmInputs == 1 = text "Left = Right"
                      | otherwise               = concatWith (surround (text " and then "))
                                                    [ parens $ qualify (Just $ text "Left") (sparkName n) <+>
                                                        text "=" <+> qualify (Just $ text "Right") (sparkName n)
                                                        | (n, _) <- Map.toList fsmInputs ]
               in eFunc (text "Equivalent_Keys")
                    [ declVars [ text "Left", text "Right" ] (svType env) ]
                    (sparkType VTBool)
                    comp
                    []
           , vcat [ text "package Transition_Maps is new Ada.Containers.Formal_Hashed_Maps"
                  , indent 2 $ vcat [ text "(Key_Type        =>" <+> svType env <> comma
                                    , text " Element_Type    =>" <+> outType <> comma
                                    , text " Hash            => Hash,"
                                    , text " Equivalent_Keys => Equivalent_Keys);" ] ]
           , assign (declVar (text "Transition_Map_Max_Capacity") (text "constant")) (stType <> text "'Last-1")
           , vcat [ text "subtype Transition_Map is Transition_Maps.Map"
                  , indent 2 $ vcat [ text "(Capacity => Transition_Map_Max_Capacity,"
                                    , text " Modulus  => Transition_Maps.Default_Modulus (Transition_Map_Max_Capacity));" ] ]
           , statement $ declType (text "Transition_Lookup") (text "array" <+> parens stType <+> text "of Transition_Map")
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
           , eFunc (text "Initial_States_Valid")
               [ declVar (text "T") (text "Transition_Lookup") ]
               (sparkType VTBool)
               (vcat [ text "for all K of Transition_Maps.Formal_Model.Keys (T(T'Last)) =>"
                     , indent 2 $ text "Env_Init (K) and Sys_Init (Transition_Maps.Element (T(T'Last), K)." <> svName sys <> text ")" ] )
               [Ghost]
           , eFunc (text "Transitions_Valid")
               [ declVar (text "T") (text "Transition_Lookup") ]
               (sparkType VTBool)
               (vcat [ text "for all I in T'Range =>"
                     , indent 2 $
                         vcat [ text "(for all J of Transition_Maps.Formal_Model.Keys (T(I)) =>"
                              , indent 2 $
                                  vcat [ text "(for all K of Transition_Maps.Formal_Model.Keys (T(Transition_Maps.Element (T(I), J)." <> stName <> text ")) =>"
                                       , indent 2 $
                                           vcat [ text "Env_Trans (J, K, Transition_Maps.Element (T(I), J)." <> svName sys <> text ") and"
                                                , text "Sys_Trans (J, K, Transition_Maps.Element (T(I), J)." <> svName sys <>
                                                    text ", Transition_Maps.Element (T(Transition_Maps.Element (T(I), J)." <> stName <>
                                                    text "), K)." <> svName sys <> text ")))" ] ] ] ] )
               [Ghost]
           , vcat $
               func (text "Init") [] (text "Transition_Lookup")
               : annotate [ Post (vcat [ text ""
                                       , indent 2 $
                                           vcat [ text "Initial_States_Valid (Init'Result)"
                                                , text "and Transitions_Valid (Init'Result)"
                                                , text ""
                                                , mkTablePost fsmNodes (svType env) ] ] ) ]
               ++ [ subBody (text "Init")
                     [ statement $ declVar (text "Transitions") (text "Transition_Lookup") ]
                     [ mkTable fsmNodes
                     , text ""
                     , statement $ text "return Transitions"] ]
           , assign (declVar (text "Transitions") (text "constant Transition_Lookup")) (text "Init")
           , vcat [ proc (text "Move")
                      [ declParam cntlrName InOut cntlrType
                      , declParam (svName env) In (svType env)
                      , declParam (svName sys) Out (svType sys) ]
                  , subBody (text "Move")
                      [ statement $ declVar (text "O") outType ]
                      [ vcat [ text "if Transition_Maps.Contains (Transitions(" <> qualify (Just cntlrName) stName <> text ")" <> comma <+> svName env <> text ") then"
                             , indent 2 $
                                 vcat [ assign (text "O") (text "Transition_Maps.Element (Transitions(" <> qualify (Just cntlrName) stName <> text ")" <> comma <+> svName env <> text ")")
                                      , assign (svName sys) (qualify (Just $ text "O") (svName sys))
                                      , assign cntlrName
                                          (parens . fsep $ punctuate comma [ stName <+> text "=>" <+> qualify (Just $ text "O") stName
                                                                           , svName env <+> text "=>" <+> svName env
                                                                           , svName sys <+> text "=>" <+> qualify (Just $ text "O") (svName sys) ] ) ]
                             , text "end if;" ] ] ] ]

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

sparkRootType :: VType -> Doc
sparkRootType (VTInt _ _) = text "Integer"
sparkRootType t           = sparkType t

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
assign l r = statement $ l <+> text ":=" <+> r

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

annotate :: [Annotation] -> [Doc]
annotate [] = []
annotate as = [ text "with"
              , indent 2 $ vcat $ punctuate comma $ map annotation as]

func :: Doc -> [Doc] -> Doc -> Doc
func name [] ret =
  text "function" <+> name <+> text "return" <+> ret
func name ps ret =
  text "function" <+> name <+> (parens . fsep $ punctuate semi ps) <+> text "return" <+> ret

eFunc :: Doc -> [Doc] -> Doc -> Doc -> [Annotation] -> Doc
eFunc name ps ret e as =
  statement . vcat $
    vcat [ func name ps ret
         , text "is"
         , indent 2 $ parens e ]
    : annotate as

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
