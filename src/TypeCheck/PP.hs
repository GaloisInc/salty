{-# LANGUAGE RecordWildCards #-}

module TypeCheck.PP (
    PP(..),
    pp,
    optParens,
    ticks,

    module Text.PrettyPrint.HughesPJ
  ) where

import Scope.Name
import TypeCheck.AST

import qualified Data.Foldable as F
import qualified Data.Text.Lazy as L
import           Data.Int (Int64)
import           Text.PrettyPrint.HughesPJ
import           Text.Location


pp :: PP a => a -> Doc
pp  = ppPrec 0

optParens :: Bool -> Doc -> Doc
optParens True  = parens
optParens False = id

ticks :: Doc -> Doc
ticks d = char '`' <> d <> char '`'

class PP a where
  ppPrec :: Int -> a -> Doc

  ppList :: [a] -> Doc
  ppList as = brackets (fsep (punctuate comma (map pp as)))

instance PP a => PP [a] where
  ppPrec _ = ppList
  ppList   = ppList

instance PP Int where
  ppPrec _ = int

instance PP Int64 where
  ppPrec _ = integer . toInteger

instance PP Char where
  ppPrec _ = char
  ppList   = text

instance PP L.Text where
  ppPrec _ = text . L.unpack

instance PP Name where
  ppPrec _ = pp . nameText

instance PP Integer where
  ppPrec _ = integer

instance PP Controller where
  ppPrec _ Controller { .. } =
    vcat $ [ text "controller" <+> pp cName <+> text "where" ]
        ++ map pp cEnums
        ++ concatMap (map pp . F.toList) cFuns
        ++ map (ppStateVar "input")  cInputs
        ++ map (ppStateVar "output") cOutputs
        ++ [ hang (text "env_trans")    2 (pp cEnvTrans)
           , hang (text "env_liveness") 2 (pp cEnvLiveness)
           , hang (text "sys_trans")    2 (pp cSysTrans)
           , hang (text "sys_liveness") 2 (pp cSysLiveness) ]

instance PP EnumDef where
  ppPrec _ EnumDef { .. } =
    hang (text "enum" <+> pp eName)
       2 (vcat (zipWith (\c con -> pp c <+> pp con) ('=' : repeat '|') eCons))

ppStateVar :: String -> StateVar -> Doc
ppStateVar lab StateVar { .. } =
  hang (text lab <+> pp svName <+> char ':' <+> pp svType)
     2 (maybe empty (\i -> char '=' <+> pp i) svInit)

instance PP Fun where
  ppPrec _ Fun { .. } =
    hang (pp fName
          <+> parens (fsep (punctuate comma (map ppParam fParams)))
          <> colon
          <+> pp fResult
          <+> char '=')
       2 (pp fBody)
    where
    ppParam (p,ty) = pp p <> colon <+> pp ty

instance PP Expr where
  ppPrec _ ETrue      = text "True"
  ppPrec _ EFalse     = text "False"
  ppPrec _ (EVar n)   = pp n
  ppPrec _ (ECon n)   = pp n
  ppPrec _ (ENum i)   = pp i
  ppPrec p (EAnd l r) = ppBinop p l (text "&&") r
  ppPrec p (EOr  l r) = ppBinop p l (text "||") r
  ppPrec p (EEq l r)  = ppBinop p l (text "=")  r
  ppPrec _ (ENot a)   = text "!" <> ppPrec 10 a
  ppPrec p (EApp f x) = optParens (p >= 10) (hang (pp f) 2 (ppPrec 10 x))
  ppPrec _ (ENext e)  = char 'X' <> parens (pp e)

ppBinop :: (PP a, PP b) => Int -> a -> Doc -> b -> Doc
ppBinop p a x b = optParens (p >= 10) (sep [ppPrec 10 a, x, ppPrec 10 b])

instance PP TVar where
  ppPrec _ TVar { .. } = char '?' <> pp tvUnique

instance PP Type where
  ppPrec _ (TFree v)  = pp v
  ppPrec _ TBool      = text "Bool"
  ppPrec _ TInt       = text "Num"
  ppPrec _ (TEnum n)  = pp n
  ppPrec p (TFun a b) = optParens (p >= 10) (sep [ ppPrec 10 a <+> text "->", pp b ])

instance PP Position where
  ppPrec _ Position { .. } =
    pp posRow <> char ',' <> pp posCol

instance PP src => PP (Range src) where
  ppPrec _ Range { .. } =
    maybe (text "<no location>") pp rangeSource <> char ':'
    <> pp rangeStart <> char '-' <> pp rangeEnd
