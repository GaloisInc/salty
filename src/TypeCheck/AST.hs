{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module TypeCheck.AST where

import Scope.Name (Name)
import PP

import qualified Data.Foldable as F
import           Data.Function (on)
import           Language.Slugs.Lens


data TVar = TVar { tvOrigin :: !(Maybe Name)
                 , tvUnique :: !Int
                 } deriving (Show)

instance Eq TVar where
  (==) = (==) `on` tvUnique
  (/=) = (/=) `on` tvUnique

instance Ord TVar where
  compare = compare `on` tvUnique

data Type = TFree TVar
          | TSet Type
          | TBool
          | TInt
          | TEnum Name
          | TFun Type Type
            deriving (Eq,Ord,Show)

data Controller = Controller { cName        :: !Name
                             , cInputs      :: [StateVar]
                             , cOutputs     :: [StateVar]
                             , cEnvTrans    :: Expr
                             , cEnvLiveness :: Expr
                             , cSysTrans    :: Expr
                             , cSysLiveness :: Expr
                             , cFuns        :: [Group Fun]
                             , cEnums       :: [EnumDef]
                             } deriving (Show)

emptyController :: Name -> Controller
emptyController cName =
  Controller { cName
             , cInputs      = []
             , cOutputs     = []
             , cEnvTrans    = ETrue
             , cEnvLiveness = ETrue
             , cSysTrans    = ETrue
             , cSysLiveness = ETrue
             , cFuns        = []
             , cEnums       = [] }

data Group a = NonRecursive a
             | Recursive [a]
               deriving (Functor,Foldable,Traversable,Show)

data EnumDef = EnumDef { eName :: !Name
                       , eCons :: [Name]
                       } deriving (Show)

data StateVar = StateVar { svName   :: !Name
                         , svType   :: !Type
                         , svBounds :: !(Maybe (Int,Int))
                           -- ^ When the state var is of type TInt, the bounds
                           -- are the min and max values that can appear.
                         , svInit   :: !(Maybe Expr)
                         } deriving (Show)

data Fun = Fun { fName   :: !Name
               , fParams :: [(Name,Type)]
               , fResult :: Type
               , fBody   :: Expr
               } deriving (Show)

data Expr = ETrue
          | EFalse
          | EVar Name
          | ECon Name
          | ENum Integer
          | EApp Expr Expr
          | EAnd Expr Expr
          | EOr  Expr Expr
          | ENot Expr
          | ENext Expr
          | EEq Expr Expr
          | ESet [Expr]
          | EIn Expr Expr
            deriving (Show,Eq,Ord)


destTFun :: Type -> ([Type],Type)
destTFun (TFun l r) =
  let (args,res) = destTFun r
   in (l:args,res)
destTFun ty = ([],ty)

destEApp :: Expr -> (Expr,[Expr])
destEApp = go []
  where
  go acc (EApp f x) = go (x:acc) f
  go acc e          = (e, acc)


eAnd :: [Expr] -> Expr
eAnd []  = ETrue
eAnd [e] = e
eAnd es  = foldl1 EAnd es

eNot :: Expr -> Expr
eNot  = ENot

eOr :: [Expr] -> Expr
eOr []  = EFalse
eOr [e] = e
eOr es  = foldl1 EOr es

eImp :: Expr -> Expr -> Expr
eImp a b = eOr [ eNot a, b ]

eIf :: Expr -> Expr -> Expr -> Expr
eIf p t f = eAnd [ eImp p t, eImp (eNot p) f ]


-- Traversals ------------------------------------------------------------------

traverseExpr :: Traversal' Expr Expr
traverseExpr _ ETrue      = pure ETrue
traverseExpr _ EFalse     = pure EFalse
traverseExpr _ e@EVar{}   = pure e
traverseExpr _ e@ECon{}   = pure e
traverseExpr _ e@ENum{}   = pure e
traverseExpr f (EApp a b) = EApp  <$> f a <*> f b
traverseExpr f (EAnd a b) = EAnd  <$> f a <*> f b
traverseExpr f (EOr  a b) = EOr   <$> f a <*> f b
traverseExpr f (ENot a)   = ENot  <$> f a
traverseExpr f (ENext a)  = ENext <$> f a
traverseExpr f (EEq a b)  = EEq   <$> f a <*> f b
traverseExpr f (EIn a b)  = EIn   <$> f a <*> f b
traverseExpr f (ESet es)  = ESet  <$> traverse f es


-- Pretty-printing -------------------------------------------------------------

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
