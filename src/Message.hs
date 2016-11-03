module Message where

import PP
import Syntax.AST (Loc)

import Text.Location (thing,getLoc)


ppMessage :: String -> Doc -> Doc
ppMessage msg body =
  vcat [ text "--" <+> text msg <+> text (replicate (76 - length msg) '-')
       , nest 2 body
       , text " " ]

ppError :: PP a => Loc a -> Doc
ppError loc = ppMessage ("[error] " ++ render (getLoc loc)) (pp (thing loc))
