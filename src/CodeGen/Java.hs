{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module CodeGen.Java where

import Scope.Name
import PP
import Slugs.FSM
import TypeCheck.AST (EnumDef(..))

import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import           Text.Location (HasLoc(..))


javaFSM :: FSM -> Doc
javaFSM FSM { .. } = vcat
  [ text "import javax.annotation.Generated;"
  , text ""
  , generated fsmName
  , block (public (final (cls name))) $ vcat $
    [ private (text "int" <+> text "mState" <> end)
    , text ""
    ] ++

    intersperse (text "") (map javaEnum fsmEnums) ++

    [ text ""
    , mkException fsmInputs

    , text ""
    , block (public (name <> parens empty)) $ vcat
      [ assign (text "mState") (int 0)
      ]

    , text ""
    ] ++

    moveFun fsmInputs fsmOutputs fsmNodes
  ]

  where
  name = javaName fsmName


javaEnum :: EnumDef -> Doc
javaEnum EnumDef { .. } =
  generated eName $$
  block (public (enum (javaName eName)))
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
         , sep [ text "return", retCls <> parens (fsep (punctuate comma (map javaValue vals))) <> end ]]

  outVars  = map mkVar (Map.toList outs)
  outDecls = [ ty <+> var | (ty,var) <- outVars ]

javaVType :: VType -> Doc
javaVType VTBool                  = text "boolean"
javaVType (VTEnum EnumDef { .. }) = javaName eName
javaVType (VTInt _ _)             = text "int"

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
    FromController o -> pp o <> char '.' <> pp n
    FromDecl _ d     -> pp d <> char '.' <> pp n
    FromParam _ d    -> pp d <> char '.' <> pp n

javaValue (VNum n)      = pp n


-- | Generate a local exception class that can be used thrown when the
-- inputs don't match the current state.
mkException :: StateVars -> Doc
mkException inps =
  block (public (cls clsName) <+> text "extends RuntimeException")
    $ vcat
    $ [ public (final (ty <+> var)) <> end | (ty,var) <- inVars ]
   ++ [ text ""
      , block (public clsName <+> argList)
              (vcat [ assign (this var) var | (_,var) <- inVars ])
      ]

  where

  clsName = text "InputException"

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
