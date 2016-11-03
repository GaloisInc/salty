{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module TypeCheck.AST (
    module TypeCheck.AST,
    Loc, thing, HasLoc(getLoc), at, noLoc, SrcRange
  ) where

import Scope.Name (Name)
import Syntax.AST (SrcRange,Loc,noLoc)
import PP
import Panic (panic)

import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.Map.Strict as Map
import           Language.Slugs.Lens
import           Text.Location (at,thing,HasLoc(getLoc,LocSource))


data TVar = TVar { tvOrigin :: !(Maybe Name)
                 , tvUnique :: !Int
                   -- ^ When the TVar is used for a free type variable, this
                   -- index is globally unique. When it's used for a generalized
                   -- variable, this variable is the index into the parameter
                   -- list.
                 } deriving (Show)

instance Eq TVar where
  (==) = (==) `on` tvUnique
  (/=) = (/=) `on` tvUnique

instance Ord TVar where
  compare = compare `on` tvUnique

data Type = TFree TVar
          | TGen TVar -- ^ Bound type variables
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
                             , cTopExprs    :: [Loc Expr]
                             , cFuns        :: [Group Fun]
                             , cEnums       :: [EnumDef]
                             } deriving (Show)

data Spec = Spec { sEnvTrans    :: [Loc Expr]
                 , sEnvLiveness :: [Loc Expr]
                 , sSysTrans    :: [Loc Expr]
                 , sSysLiveness :: [Loc Expr]
                 } deriving (Show)

-- | Overwrite all location information in the spec with the location
-- information passed in.
setSpecLoc :: (HasLoc loc, LocSource loc ~ FilePath) => loc -> Spec -> Spec
setSpecLoc loc spec =
  Spec { sEnvTrans    = upd sEnvTrans
       , sEnvLiveness = upd sEnvLiveness
       , sSysTrans    = upd sSysTrans
       , sSysLiveness = upd sSysLiveness
       }
  where
  range = getLoc loc

  upd p = [ thing l `at` range | l <- p spec ]

instance Monoid Spec where
  mempty = Spec { sEnvTrans    = []
                , sEnvLiveness = []
                , sSysTrans    = []
                , sSysLiveness = []
                }

  mappend a b = Spec { sEnvTrans    = merge sEnvTrans
                     , sEnvLiveness = merge sEnvLiveness
                     , sSysTrans    = merge sSysTrans
                     , sSysLiveness = merge sSysLiveness
                     }
    where
    merge p = p a ++ p b

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

data Schema = Forall [TVar] Type
              deriving (Show)

data Fun = Fun { fName   :: !Name
               , fSchema :: Schema
               , fParams :: [Name]
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
          | ETApp Expr Type
          | ESet Type [Expr]
          | ELet Name Type Expr Expr
          | EPrim Prim
            deriving (Show,Eq,Ord)


class TypeInst a where
  typeInst :: [Type] -> a -> a

instance TypeInst a => TypeInst [a] where
  typeInst ts = map (typeInst ts)

instance TypeInst Type where
  typeInst gs = go
    where
    go (TGen v)   = gs !! tvUnique v
    go (TSet t)   = TSet (go t)
    go (TFun a b) = TFun (go a) (go b)
    go t@TFree{}  = t
    go t@TBool    = t
    go t@TInt     = t
    go t@TEnum{}  = t
    go t@TSpec    = t

instance TypeInst Expr where
  typeInst gs (EVar ty n)     = EVar (typeInst gs ty) n
  typeInst gs (ECon ty n)     = ECon (typeInst gs ty) n
  typeInst gs (EApp f x)      = EApp (typeInst gs f) (typeInst gs x)
  typeInst gs (ETApp e ty)    = ETApp (typeInst gs e) (typeInst gs ty)
  typeInst gs (ESet ty es)    = ESet (typeInst gs ty) (typeInst gs es)
  typeInst gs (ELet n ty a b) = ELet n (typeInst gs ty)
                                       (typeInst gs a)
                                       (typeInst gs b)
  typeInst gs (EPrim p)       = EPrim (typeInst gs p)
  typeInst _  t@ETrue         = t
  typeInst _  t@EFalse        = t
  typeInst _  t@ENum{}        = t

instance TypeInst Prim where
  typeInst gs (PNext ty) = PNext (typeInst gs ty)
  typeInst gs (PEq   ty) = PEq   (typeInst gs ty)
  typeInst gs (PIn   ty) = PIn   (typeInst gs ty)
  typeInst _  t@PAnd     = t
  typeInst _  t@POr      = t
  typeInst _  t@PNot     = t
  typeInst _  t@PAny     = t
  typeInst _  t@PAll     = t
  typeInst _  t@PMutex   = t

typeOf :: Expr -> Type
typeOf  = typeOf' []

typeOf' :: [Type] -> Expr -> Type
typeOf'  = go
  where
  go _  ETrue          = TBool
  go _  EFalse         = TBool
  go gs (EVar ty _)    = typeInst gs ty
  go gs (ECon ty _)    = typeInst gs ty
  go _  (ENum _)       = TInt
  go gs (EApp f _)     =
    case go gs f of
      TFun _ r -> r
      _        -> panic "typeOf: Non-function in application position"

  go gs (ETApp f t)    = go (gs ++ [t]) f

  go gs (ESet ty _)    = TSet (typeInst gs ty)
  go gs (ELet _ _ _ e) = go gs e
  go gs (EPrim p)      = primTypeOf gs p

  primTypeOf _  PAnd       = tFun [TBool,TBool,TBool]
  primTypeOf _  POr        = tFun [TBool,TBool,TBool]
  primTypeOf _  PNot       = tFun [TBool,TBool]
  primTypeOf gs (PNext ty) = tFun [typeInst gs ty,typeInst gs ty]
  primTypeOf gs (PEq ty)   = tFun [typeInst gs ty,typeInst gs ty,TBool]
  primTypeOf gs (PIn ty)   = tFun [typeInst gs ty, TSet (typeInst gs ty), TBool]
  primTypeOf _  PAny       = tFun [TSet TBool, TBool]
  primTypeOf _  PAll       = tFun [TSet TBool, TBool]
  primTypeOf _  PMutex     = tFun [TSet TBool, TBool]



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

destApp :: Expr -> (Expr,[Type],[Expr])
destApp e =
  let (a,es) = destEApp e
      (b,ts) = destETApp a
   in (b,ts,es)

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

destETApp :: Expr -> (Expr,[Type])
destETApp  = go []
  where
  go acc (ETApp e ty) = go (ty:acc) e
  go acc e            = (e, reverse acc)


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
traverseExpr f (ETApp e ty)   = ETApp    <$> f e <*> pure ty

class Subst a where
  subst :: Map.Map Name Expr -> a -> a

instance Subst a => Subst [a] where
  subst env = map (subst env)

instance Subst a => Subst (Loc a) where
  subst env = fmap (subst env)

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
    [ hang (text "env_trans")    2 (ppList (map thing sEnvTrans))
    , hang (text "env_liveness") 2 (ppList (map thing sEnvLiveness))
    , hang (text "sys_trans")    2 (ppList (map thing sSysTrans))
    , hang (text "sys_liveness") 2 (ppList (map thing sSysLiveness)) ]

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
          <+> parens (fsep (punctuate comma (map pp fParams)))
           <> colon
          <+> pp fSchema
          <+> char '=')
       2 (pp fBody)

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
  ppPrec p e@ETApp{}    = optParens (p >= 10) $
    let (e', ts) = destETApp e
        params   = braces (fsep (punctuate comma (map pp ts)))
      in optParens (p >= 10) (hang (ppPrec 10 e' <> char '@') 2 params)

  ppPrec p (ELet n t b e) =
    optParens (p >= 10)
      ((text "let" <+> pp n <+> char ':' <+> pp t
                   <+> char '=' <+> pp b <+> text "in") $$ (pp e))

ppBinop :: (PP a, PP b) => Int -> a -> Doc -> b -> Doc
ppBinop p a x b = optParens (p >= 10) (sep [ppPrec 10 a, x, ppPrec 10 b])

instance PP TVar where
  ppPrec _ TVar { .. } = char '?' <> pp tvUnique

instance PP Schema where
  ppPrec p (Forall ps ty) = optParens (p >= 10) $ (vars <+> pp ty)
    where
    vars | null ps   = empty
         | otherwise = text "forall"
                   <+> fsep (punctuate (char ',') (map pp ps))
                    <> char '.'

instance PP Type where
  ppPrec _ (TFree v)  = pp v
  ppPrec _ (TGen v)   = pp v
  ppPrec _ TBool      = text "Bool"
  ppPrec _ TInt       = text "Num"
  ppPrec _ (TEnum n)  = pp n
  ppPrec _ (TSet a)   = braces (pp a)
  ppPrec p (TFun a b) = optParens (p >= 10) (sep [ ppPrec 10 a <+> text "->", pp b ])
  ppPrec _ TSpec      = text "Spec"
