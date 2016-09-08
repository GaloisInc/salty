{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeCheck.AST where

import Scope.Name (Name)
import PP
import Panic (panic)

import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.Map.Strict as Map
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
          | TSpec
            -- ^ The typ of top-level specifications (things that use sys_trans,
            -- etc.)
            deriving (Eq,Ord,Show)

data Controller = Controller { cName        :: !Name
                             , cInputs      :: [StateVar]
                             , cOutputs     :: [StateVar]
                             , cSpec        :: Spec
                             , cTopExprs    :: [Expr]
                             , cFuns        :: [Group Fun]
                             , cEnums       :: [EnumDef]
                             } deriving (Show)

data Spec = Spec { sEnvTrans    :: Expr
                 , sEnvLiveness :: Expr
                 , sSysTrans    :: Expr
                 , sSysLiveness :: Expr
                 } deriving (Show)

instance Monoid Spec where
  mempty = Spec { sEnvTrans    = ETrue
                , sEnvLiveness = ETrue
                , sSysTrans    = ETrue
                , sSysLiveness = ETrue
                }

  mappend a b = Spec { sEnvTrans    = merge sEnvTrans
                     , sEnvLiveness = merge sEnvLiveness
                     , sSysTrans    = merge sSysTrans
                     , sSysLiveness = merge sSysLiveness
                     }
    where
    merge p = EAnd (p a) (p b)

emptyController :: Name -> Controller
emptyController cName =
  Controller { cName
             , cInputs      = []
             , cOutputs     = []
             , cSpec        = mempty
             , cFuns        = []
             , cEnums       = []
             , cTopExprs    = [] }

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
               , fBody   :: FunBody
               } deriving (Show)

data FunBody = FunSpec Spec
             | FunExpr Expr
               deriving (Show)

data Prim = PAnd
          | POr
          | PNot
          | PNext Type
          | PEq   Type
          | PIn   Type
          | PAny
          | PAll
          | PMutex
            deriving (Show,Eq,Ord)

data Expr = ETrue
          | EFalse
          | EVar Type Name
          | ECon Type Name
          | ENum Integer
          | EApp Expr Expr
          | ESet Type [Expr]
          | ELet Name Type Expr Expr
          | EPrim Prim
            deriving (Show,Eq,Ord)

primTypeOf :: Prim -> Type
primTypeOf PAnd       = tFun [TBool,TBool,TBool]
primTypeOf POr        = tFun [TBool,TBool,TBool]
primTypeOf PNot       = tFun [TBool,TBool]
primTypeOf (PNext ty) = tFun [ty,ty]
primTypeOf (PEq ty)   = tFun [ty,ty,TBool]
primTypeOf (PIn ty)   = tFun [ty, TSet ty, TBool]
primTypeOf PAny       = tFun [TSet TBool, TBool]
primTypeOf PAll       = tFun [TSet TBool, TBool]
primTypeOf PMutex     = tFun [TSet TBool, TBool]

typeOf :: Expr -> Type
typeOf ETrue          = TBool
typeOf EFalse         = TBool
typeOf (EVar ty _)    = ty
typeOf (ECon ty _)    = ty
typeOf (ENum _)       = TInt
typeOf (EApp f _)     =
  case typeOf f of
    TFun _ r -> r
    _        -> panic "typeOf: Non-function in application position"

typeOf (ESet ty _)    = TSet ty
typeOf (ELet _ _ _ e) = typeOf e
typeOf (EPrim p)      = primTypeOf p


pattern EAnd :: Expr -> Expr -> Expr
pattern EAnd a b = EApp (EApp (EPrim PAnd) a) b

pattern EOr :: Expr -> Expr -> Expr
pattern EOr a b = EApp (EApp (EPrim POr) a) b

pattern ENot :: Expr -> Expr
pattern ENot a = EApp (EPrim PNot) a

pattern EEq :: Type -> Expr -> Expr -> Expr
pattern EEq ty a b = EApp (EApp (EPrim (PEq ty)) a) b

pattern ENext :: Type -> Expr -> Expr
pattern ENext ty a = EApp (EPrim (PNext ty)) a

pattern EIn :: Type -> Expr -> Expr -> Expr
pattern EIn t a b = EApp (EApp (EPrim (PIn t)) a) b

pattern EAny :: Expr -> Expr
pattern EAny a = EApp (EPrim PAny) a

pattern EAll :: Expr -> Expr
pattern EAll a = EApp (EPrim PAll) a

pattern EMutex :: Expr -> Expr
pattern EMutex a = EApp (EPrim PMutex) a

destTFun :: Type -> ([Type],Type)
destTFun (TFun l r) =
  let (args,res) = destTFun r
   in (l:args,res)
destTFun ty = ([],ty)

destEApp :: Expr -> (Expr,[Expr])
destEApp  = go []
  where
  go acc (EApp f x) = go (x:acc) f
  go acc f          = (f, acc)

destELet :: Expr -> ([(Name,Type,Expr)], Expr)
destELet  = go []
  where
  go acc (ELet n t b e) = go ((n,t,b) : acc) e
  go acc e              = (reverse acc, e)

destEAnd :: Expr -> [Expr]
destEAnd (EAnd a b) = destEAnd a ++ destEAnd b
destEAnd e          = [e]

destEOr :: Expr -> [Expr]
destEOr (EOr a b) = destEOr a ++ destEOr b
destEOr e         = [e]


tFun :: [Type] -> Type
tFun  = foldr1 TFun

eAnd :: [Expr] -> Expr
eAnd []  = ETrue
eAnd [e] = e
eAnd es  = foldr1 EAnd es

eNot :: Expr -> Expr
eNot  = ENot

eOr :: [Expr] -> Expr
eOr []  = EFalse
eOr [e] = e
eOr es  = foldr1 EOr es

eImp :: Expr -> Expr -> Expr
eImp a b = eOr [ eNot a, b ]

eIf :: Expr -> Expr -> Expr -> Expr
eIf p t f = eAnd [ eImp p t, eImp (eNot p) f ]

eApp :: Expr -> [Expr] -> Expr
eApp  = foldl EApp


-- Traversals ------------------------------------------------------------------

traverseExpr :: Traversal' Expr Expr
traverseExpr _ ETrue          = pure ETrue
traverseExpr _ EFalse         = pure EFalse
traverseExpr _ e@EVar{}       = pure e
traverseExpr _ e@ECon{}       = pure e
traverseExpr _ e@ENum{}       = pure e
traverseExpr f (EApp p x)     = EApp     <$> f p <*> f x
traverseExpr f (ESet t es)    = ESet t   <$> traverse f es
traverseExpr f (ELet n t b e) = ELet n t <$> f b <*> f e
traverseExpr _ e@EPrim{}      = pure e

class Subst a where
  subst :: Map.Map Name Expr -> a -> a

instance Subst FunBody where
  subst env (FunSpec s) = FunSpec (subst env s)
  subst env (FunExpr e) = FunExpr (subst env e)

instance Subst Spec where
  subst env Spec { .. } =
    Spec { sEnvTrans    = subst env sEnvTrans
         , sEnvLiveness = subst env sEnvLiveness
         , sSysTrans    = subst env sSysTrans
         , sSysLiveness = subst env sSysLiveness }

instance Subst Expr where
  subst env = rewriteOf traverseExpr f
    where
    f (EVar _ v) = Map.lookup v env
    f _          = Nothing


-- Pretty-printing -------------------------------------------------------------

instance PP Controller where
  ppPrec _ Controller { .. } =
    vcat $ [ text "controller" <+> pp cName <+> text "where" ]
        ++ map pp cEnums
        ++ concatMap (map pp . F.toList) cFuns
        ++ map (ppStateVar "input")  cInputs
        ++ map (ppStateVar "output") cOutputs
        ++ [pp cSpec]

instance PP Spec where
  ppPrec p Spec { .. } = optParens (p >= 10) $ vcat
    [ hang (text "env_trans")    2 (pp sEnvTrans)
    , hang (text "env_liveness") 2 (pp sEnvLiveness)
    , hang (text "sys_trans")    2 (pp sSysTrans)
    , hang (text "sys_liveness") 2 (pp sSysLiveness) ]

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

instance PP FunBody where
  ppPrec _ (FunSpec s) = pp s
  ppPrec _ (FunExpr e) = pp e

instance PP Prim where
  ppPrec _ PAnd      = text "&&"
  ppPrec _ POr       = text "||"
  ppPrec _ PNot      = text "!"
  ppPrec _ (PNext _) = text "X"
  ppPrec _ (PEq _)   = text "=="
  ppPrec _ (PIn _)   = text "in"
  ppPrec _ PAny      = text "any"
  ppPrec _ PAll      = text "all"
  ppPrec _ PMutex    = text "mutex"

instance PP Expr where
  ppPrec _ ETrue        = text "True"
  ppPrec _ EFalse       = text "False"
  ppPrec _ (EVar _ n)   = pp n
  ppPrec _ (ECon _ n)   = pp n
  ppPrec _ (ENum i)     = pp i
  ppPrec p (EAnd l r)   = ppBinop p l (text "&&") r
  ppPrec p (EOr  l r)   = ppBinop p l (text "||") r
  ppPrec p (EEq _ l r)  = ppBinop p l (text "==")  r
  ppPrec _ (ENot a)     = text "!" <> ppPrec 10 a
  ppPrec _ (EIn ty e s) = text "in@" <> pp ty <+> ppPrec 10 e <+> ppPrec 10 s
  ppPrec _ (EAll s)     = text "all" <+> ppPrec 10 s
  ppPrec _ (EAny s)     = text "any" <+> ppPrec 10 s
  ppPrec p (EApp f x)   = optParens (p >= 10) (hang (pp f) 2 (ppPrec 10 x))
  ppPrec _ (ENext _ e)  = char 'X' <> parens (pp e)
  ppPrec _ (ESet _ es)  = braces (ppList es)
  ppPrec p (EPrim e)    = ppPrec p e

  ppPrec p (ELet n t b e) =
    optParens (p >= 10)
      ((text "let" <+> pp n <+> char ':' <+> pp t
                   <+> char '=' <+> pp b <+> text "in") $$ (pp e))

ppBinop :: (PP a, PP b) => Int -> a -> Doc -> b -> Doc
ppBinop p a x b = optParens (p >= 10) (sep [ppPrec 10 a, x, ppPrec 10 b])

instance PP TVar where
  ppPrec _ TVar { .. } = char '?' <> pp tvUnique

instance PP Type where
  ppPrec _ (TFree v)  = pp v
  ppPrec _ TBool      = text "Bool"
  ppPrec _ TInt       = text "Num"
  ppPrec _ (TEnum n)  = pp n
  ppPrec _ (TSet a)   = braces (pp a)
  ppPrec p (TFun a b) = optParens (p >= 10) (sep [ ppPrec 10 a <+> text "->", pp b ])
  ppPrec _ TSpec      = text "Spec"
