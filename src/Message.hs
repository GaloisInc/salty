module Message where

import PP
import Syntax.AST (Loc)

import Text.Location (thing,getLoc)


ppError :: PP a => Loc a -> Doc
ppError loc = vcat [ banner, nest 2 (pp (thing loc)), text " " ]
  where
  banner = text "[error]" <+> pp (getLoc loc)
