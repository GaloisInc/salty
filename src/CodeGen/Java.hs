{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module CodeGen.Java where

import Scope.Name
import PP
import Slugs.FSM
import TypeCheck.AST (EnumDef(..))

import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           System.FilePath ((<.>),pathSeparator)
import           Text.Location (HasLoc(..))


-- | Package layout for generated classes.
type Package = Map.Map FilePath Doc

type PackageName = String

-- | Generate a java package from an FSM. This includes, the controller itself,
-- exception class for invalid transitions, and all enum definitions.
javaFSM :: PackageName -> FSM -> Package
javaFSM pkgName FSM { .. } =
  Map.fromList [ (qnameToPath (render kname),doc)
               | (kname,doc) <- controller : exception : enums ]
  where

  controller = (text pkgName <> char '.' <> pp fsmName,klass)
    where
    klass = mkClass pkgName (map fst (exception : enums))
          $ block (public (cls (javaName fsmName)))
          $ vcat
          $ [ private (text "int" <+> text "mState" <> end)
            , text ""
            , block (public (pp fsmName <> parens empty)) $ vcat
              [ assign (text "mState") (int 0)
              ]
            , text ""
            ] ++ moveFun fsmInputs fsmOutputs fsmNodes

  exception = (text pkgName <> char '.' <> exnName, klass)
    where
    klass = mkClass pkgName (importEnums enumPkg fsmInputs)
                    (mkException fsmInputs)

  enumPkg = pkgName <.> "enum"
  enums = map (javaEnum enumPkg) fsmEnums


-- | Turn a qualified class name into a java source path.
qnameToPath :: String -> FilePath
qnameToPath qname = map translate qname <.> "java"
  where
  translate '.' = pathSeparator
  translate c   = c

-- | Generate the module names for imported enum types.
importEnums :: PackageName -> StateVars -> Imports
importEnums pkgName vars = [ text pkgName <> char '.' <> pp n | n <- enums ]
  where
  enums = nub [ eName d | (_,VarInfo { viType = VTEnum d }) <- Map.toList vars ]


type Imports = [Doc]

mkClass :: PackageName
        -> Imports
        -> Doc -- ^ Class/enum declaration
        -> Doc
mkClass pkgName imps decl = vcat $
  [ text "package" <+> text pkgName <> end ] ++
  [ text "import" <+> imp <> end | imp <- imps ] ++
  [ text "", decl ]


javaEnum :: PackageName -> EnumDef -> (Doc,Doc)
javaEnum pkgName EnumDef { .. } = (name,klass)
  where
  name  = text pkgName <> char '.' <> pp eName
  klass = mkClass pkgName []
        $ block (public (enum (javaName eName)))
                (vcat (punctuate comma (map javaName eCons)))


-- | Generate the move function, as well as the return struct, if it's
-- necessary.
moveFun :: StateVars -> StateVars -> Map.Map Int Node -> [Doc]
moveFun inps outs nodes
  | Map.null nodes = []
  | otherwise      =
    [ block (public (final (cls retCls))) $ vcat $
      [ public (final out) <> end | out <- outDecls ] ++
      [ text ""
      , block (public (retCls <> parens (fsep (punctuate comma outDecls))))
              (vcat [ assign (this var) var | (_,var) <- outVars ])
      ]

    | needsRetCls ] ++

    [ text "" | needsRetCls ] ++

    [ block (public (retCls <+> text "move" <+> argList)) $ vcat
      [ block (text "switch" <> parens (this (text "mState")))
              (vcat (map (mkState mkRes nodes) (Map.toList nodes)))
      , text "throw" <+> text "new" <+> inputExn <> end ]
    ]

  where

  inputExn = text "InputException" <> parens (fsep (punctuate comma (map snd inVars)))

  inVars  = map mkVar (Map.toList inps)
  argList = parens (fsep (punctuate comma [ ty <+> var | (ty,var) <- inVars ]))

  (needsRetCls,retCls,mkRes) =
    case Map.size outs of
      0 -> (False,text "int", returnState)
      1 -> (False,javaVType (viType (head (Map.elems outs))), returnValue)
      _ -> (True, text "Return", returnStruct)

  returnState state _ =
    vcat [ assign (this (text "mState")) (int state)
         , text "return" <+> this (text "mState") <> end ]

  returnValue state vals =
    vcat [ assign (this (text "mState")) (int state)
         , text "return" <+> hsep (map javaValue vals) <> end ]

  returnStruct state vals =
    vcat [ assign (this (text "mState")) (int state)
         , sep [ text "return new", retCls <> parens (fsep (punctuate comma (map javaValue vals))) <> end ]]

  outVars  = map mkVar (Map.toList outs)
  outDecls = [ ty <+> var | (ty,var) <- outVars ]

javaVType :: VType -> Doc
javaVType VTBool      = text "boolean"
javaVType (VTEnum e)  = enumName e
javaVType (VTInt _ _) = text "int"

mkVar :: (Name,VarInfo) -> (Doc,Doc)
mkVar (n,VarInfo { .. }) = (javaVType viType, (javaName n))


type MkRes = Int -> [Value] -> Doc

mkState :: MkRes -> Map.Map Int Node -> (Int,Node) -> Doc
mkState mkRes nodes (n,Node { .. }) =
  text "case" <+> int n <> colon
  $$ nest 2 (vcat stmts)

  where
  stmts = mkTransBlock mkRes nodes nodeTrans ++ [text "break" <> end]

-- | Turn a transition block into a bunch of conditionals on the environment
-- variables.
mkTransBlock :: MkRes -> Map.Map Int Node -> [Int] -> [Doc]
mkTransBlock mkRes nodes trans =
  case branches of
    b : rest -> mkCond "if" b : map (mkCond "else if") rest
    _        -> []
  where
  branches = [ mkTrans mkRes to (nodes Map.! to) | to <- trans ]

  mkCond t (c,b) = block (text t <> parens c) b

-- | Return the conditional and body of a single transition.
mkTrans :: MkRes -> Int -> Node -> (Doc,Doc)
mkTrans mkRes i Node { .. } = (cond,mkRes i (Map.elems nodeOutputs))
  where
  cond = fsep
       $ punctuate (text " &&")
         [ mkGuard var val | (var,val) <- Map.toList nodeInputs ]


mkGuard :: Name -> Value -> Doc
mkGuard var val = javaName var <+> text "==" <+> javaValue val


javaValue :: Value -> Doc
javaValue (VBool True)  = text "true"
javaValue (VBool False) = text "false"

javaValue (VCon n)      =
  case nameOrigin n of
    FromController o -> text "Enum" <> char '.' <> pp o <> char '.' <> pp n
    FromDecl _ d     -> text "Enum" <> char '.' <> pp d <> char '.' <> pp n
    FromParam _ d    -> text "Enum" <> char '.' <> pp d <> char '.' <> pp n

javaValue (VNum n)      = pp n


exnName :: Doc
exnName  = text "InputException"

-- | Generate a local exception class that can be used thrown when the
-- inputs don't match the current state.
mkException :: StateVars -> Doc
mkException inps =
  block (public (cls exnName) <+> text "extends RuntimeException")
    $ vcat
    $ [ public (final (ty <+> var)) <> end | (ty,var) <- inVars ]
   ++ [ text ""
      , block (public exnName <+> argList)
              (vcat [ assign (this var) var | (_,var) <- inVars ])
      ]

  where

  inVars  = map mkVar (Map.toList inps)
  argList = parens (fsep (punctuate comma [ ty <+> var | (ty,var) <- inVars ]))


-- Java Utils ------------------------------------------------------------------

-- | Output the @Generated@ attribute for something that provides location
-- information.
generated :: (PP (LocSource loc), HasLoc loc) => loc -> Doc
generated loc = text "@Generated"
             <> parens (text "value" <> char '=' <> doubleQuotes (pp (getLoc loc)))

block :: Doc -> Doc -> Doc
block hdr body =
  vcat [ hdr <+> char '{'
       , nest 2 body
       , char '}' ]

end :: Doc
end  = char ';'

this :: Doc -> Doc
this m = text "this." <> m

assign :: Doc -> Doc -> Doc
assign r a = r <+> char '=' <+> a <> end

final :: Doc -> Doc
final k = text "final" <+> k

public :: Doc -> Doc
public k = text "public" <+> k

private :: Doc -> Doc
private k = text "private" <+> k

cls :: Doc -> Doc
cls k = text "class" <+> k

enum :: Doc -> Doc
enum k = text "enum" <+> k

javaName :: Name -> Doc
javaName n = pp (nameText n)

enumName :: EnumDef -> Doc
enumName EnumDef { .. } = text "Enum" <> char '.' <> javaName eName
