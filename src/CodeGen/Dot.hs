{-# LANGUAGE RecordWildCards #-}

module CodeGen.Dot where

import CodeGen.Java (Package)
import PP
import Scope.Name (Name,nameText)
import Slugs.FSM

import qualified Data.Map as Map


dotFSM :: FSM -> Package
dotFSM FSM { .. } = Map.singleton (render (dotName fsmName <> text ".dot"))
                  $ graph fsmName
                  $ vcat
                  $ punctuate (char ';')
                  $ concatMap mkEdges
                  $ Map.toList fsmNodes


mkEdges :: (Int,Node) -> [Doc]
mkEdges (src,Node { .. }) =
  [ edge (pp src) (pp dst) | dst <- nodeTrans ]



-- Utils -----------------------------------------------------------------------

block :: Doc -> Doc -> Doc
block header body =
  hang (header <+> char '{') 2 body
  $$ char '}'

graph :: Name -> Doc -> Doc
graph n = block (text "digraph" <+> dotName n)

dotName :: Name -> Doc
dotName n = pp (nameText n)

edge :: Doc -> Doc -> Doc
edge a b = a <+> text "->" <+> b
