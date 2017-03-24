module Message where

import PP
import SrcLoc



ppMessage :: String -> Doc -> Doc
ppMessage msg body =
  vcat [ text "--" <+> text msg <+> text (replicate (76 - length msg) '-')
       , nest 2 body
       , text " " ]

ppError :: (HasSrcLoc a, PP a) => a -> Doc
ppError loc = ppMessage ("[error] " ++ render (srcLoc loc)) (pp loc)
