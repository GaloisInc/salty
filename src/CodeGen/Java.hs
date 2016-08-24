{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module CodeGen.Java where

import Scope.Name
import PP
import Slugs.FSM
import TypeCheck.AST (EnumDef(..))

import           Control.Monad (guard)
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
               | (kname,doc) <- controller : exception : result ++ enums ]
  where

  controller = (text pkgName <> char '.' <> javaName fsmName,klass)
    where
    klass = mkClass pkgName (map fst (exception:result))
          $ block (public (cls (javaName fsmName)))
          $ vcat
          $ [ private (text "int" <+> text "mState" <> end)
            , text ""
            , block (public (javaName fsmName <> parens empty)) $ vcat
              [ assign (text "mState") (int 0)
              ]
            , text ""
            , moveFun enumPkg fsmInputs fsmOutputs fsmNodes
            ]

  exception = (text pkgName <> char '.' <> exnName, klass)
    where
    klass = mkClass pkgName []
                    (mkException enumPkg fsmInputs)

  result =
    do guard (Map.size fsmOutputs > 1)
       let klass = mkClass pkgName [] (mkResult enumPkg fsmOutputs)
       return (text pkgName <> char '.' <> text "Result", klass)

  enumPkg = pkgName <.> "enums"
  enums = map (javaEnum enumPkg) fsmEnums


-- | Turn a qualified class name into a java source path.
qnameToPath :: String -> FilePath
qnameToPath qname = map translate qname <.> "java"
  where
  translate '.' = pathSeparator
  translate c   = c

-- | Generate the module names for imported enum types.
importEnums :: PackageName -> StateVars -> Imports
importEnums pkgName vars = [ text pkgName <> char '.' <> javaName n | n <- enums ]
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
  name  = text pkgName <> char '.' <> javaName eName
  klass = mkClass pkgName []
        $ block (public (enum (javaName eName)))
                (vcat (punctuate comma (map javaName eCons)))


-- | Generate the move function.
moveFun :: PackageName -> StateVars -> StateVars -> Map.Map Int Node -> Doc
moveFun enumPkg inps outs nodes =
  block (public (retCls <+> text "move" <+> argList)) $ vcat
    [ block (text "switch" <> parens (this (text "mState")))
            (vcat (map (mkState enumPkg mkRes nodes) (Map.toList nodes)))
    , text "throw" <+> text "new" <+> inputExn <> end ]

  where

  inputExn = text "InputException" <> parens (fsep (punctuate comma (map snd inVars)))

  inVars  = map (mkVar enumPkg) (Map.toList inps)
  argList = parens (fsep (punctuate comma [ ty <+> var | (ty,var) <- inVars ]))

  (retCls,mkRes) =
    case Map.size outs of
      0 -> (text "int", returnState)
      1 -> (javaVType enumPkg (viType (head (Map.elems outs))), returnValue)
      _ -> (text "Result", returnStruct)

  returnState state _ =
    vcat [ assign (this (text "mState")) (int state)
         , text "return" <+> this (text "mState") <> end ]

  returnValue state vals =
    vcat [ assign (this (text "mState")) (int state)
         , text "return" <+> hsep (map (javaValue enumPkg) vals) <> end ]

  returnStruct state vals =
    vcat [ assign (this (text "mState")) (int state)
         , sep [ text "return new"
               , text "Result" <> parens (fsep (punctuate comma (map (javaValue enumPkg) vals))) <> end ]]

javaVType :: PackageName -> VType -> Doc
javaVType _     VTBool      = text "boolean"
javaVType enums (VTEnum e)  = text enums <> char '.' <> javaName (eName e)
javaVType _     (VTInt _ _) = text "int"

mkVar :: PackageName -> (Name,VarInfo) -> (Doc,Doc)
mkVar enumPkg (n,VarInfo { .. }) = (javaVType enumPkg viType, (javaName n))


type MkRes = Int -> [Value] -> Doc

mkState :: PackageName -> MkRes -> Map.Map Int Node -> (Int,Node) -> Doc
mkState enumPkg mkRes nodes (n,Node { .. }) =
  text "case" <+> int n <> colon
  $$ nest 2 (vcat stmts)

  where
  stmts = mkTransBlock enumPkg mkRes nodes nodeTrans ++ [text "break" <> end]

-- | Turn a transition block into a bunch of conditionals on the environment
-- variables.
mkTransBlock :: PackageName -> MkRes -> Map.Map Int Node -> [Int] -> [Doc]
mkTransBlock enumPkg mkRes nodes trans =
  case branches of
    [(_,b)]  -> [b]
    b : rest -> mkCond "if" b : map (mkCond "else if") rest
    _        -> []
  where
  branches = [ mkTrans enumPkg mkRes to (nodes Map.! to) | to <- trans ]

  mkCond t (c,b) = block (text t <> parens c) b

-- | Return the conditional and body of a single transition.
mkTrans :: PackageName -> MkRes -> Int -> Node -> (Doc,Doc)
mkTrans enumPkg mkRes i Node { .. } = (cond,mkRes i (Map.elems nodeOutputs))
  where
  cond = fsep
       $ punctuate (text " &&")
         [ mkGuard enumPkg var val | (var,val) <- Map.toList nodeInputs ]


mkGuard :: PackageName -> Name -> Value -> Doc
mkGuard enumPkg var val = javaName var <+> text "==" <+> javaValue enumPkg val


javaValue :: PackageName -> Value -> Doc
javaValue _ (VBool True)  = text "true"
javaValue _ (VBool False) = text "false"

javaValue enumPkg (VCon n) =
  case nameOrigin n of
    FromController _ -> text enumPkg <> char '.'                           <> javaName n
    FromDecl _ d     -> text enumPkg <> char '.' <> javaName d <> char '.' <> javaName n
    FromParam _ d    -> text enumPkg <> char '.' <> javaName d <> char '.' <> javaName n
    Generated _      -> javaName n

javaValue _ (VNum n) = pp n


exnName :: Doc
exnName  = text "InputException"

-- | Generate a local exception class that can be used thrown when the
-- inputs don't match the current state.
mkException :: PackageName -> StateVars -> Doc
mkException enumPkg inps =
  block (public (cls exnName) <+> text "extends RuntimeException")
    $ vcat
    $ [ public (final (ty <+> var)) <> end | (ty,var) <- inVars ]
   ++ [ text ""
      , block (public exnName <+> argList)
              (vcat [ assign (this var) var | (_,var) <- inVars ])
      ]

  where

  inVars  = map (mkVar enumPkg) (Map.toList inps)
  argList = parens (fsep (punctuate comma [ ty <+> var | (ty,var) <- inVars ]))


mkResult :: PackageName -> StateVars -> Doc
mkResult enumPkg outs =
  block (public (final (cls resCls))) $ vcat $
    [ public (final out) <> end | out <- outDecls ] ++
    [ text ""
    , block (public (resCls <> parens (fsep (punctuate comma outDecls))))
            (vcat [ assign (this var) var | (_,var) <- outVars ])
    ]
  where
  resCls = text "Result"

  outVars  = map (mkVar enumPkg) (Map.toList outs)
  outDecls = [ ty <+> var | (ty,var) <- outVars ]


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
