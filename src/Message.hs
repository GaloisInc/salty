module Message where

import PP
import SrcLoc



ppMessage :: String -> Doc -> Doc
ppMessage msg body =
  vcat [ text "--" <+> text msg <+> text (replicate (76 - length msg) '-')
       , nest 2 body
       , text " " ]

ppError :: PP a => SrcLoc -> a -> Doc
ppError loc a = ppMessage ("[error] " ++ render (srcLoc loc)) (pp a)

ppWarning :: PP a => SrcLoc -> a -> Doc
ppWarning loc a = ppMessage ("[warning] " ++ render (srcLoc loc)) (pp a)
