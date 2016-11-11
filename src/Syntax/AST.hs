{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module Syntax.AST where

import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import           Text.Location


type SrcRange = Range FilePath

type Loc = Located FilePath

noLoc :: a -> Loc a
noLoc a = a `at` (mempty :: SrcRange)

-- | Parsed names.
type PName = L.Text

data Controller name =
  Controller { cName  :: Loc name
             , cDecls :: [TopDecl name]
             } deriving (Functor,Show)


data TopDecl name = TDEnum (EnumDef name)
                  | TDFun (Fun name)
                  | TDInput (StateVar name)
                  | TDOutput (StateVar name)
                  | TDSpec (Spec name)
                  | TDExpr (Expr name)
                  | TDLoc (Loc (TopDecl name))
                    deriving (Functor,Show)

data Spec name = SEnvTrans    [Expr name]
               | SEnvLiveness [Expr name]
               | SSysTrans    [Expr name]
               | SSysLiveness [Expr name]
               | SLoc (Loc (Spec name))
                 deriving (Functor,Show)


isFun :: TopDecl name -> Bool
isFun (TDLoc loc) = isFun (thing loc)
isFun TDFun{}     = True
isFun _           = False


data EnumDef name = EnumDef { eName :: Loc name
                            , eCons :: [(Loc name, Maybe (Loc L.Text))]
                            } deriving (Functor,Show)


data Fun name = Fun { fAnn    :: Maybe Ann
                    , fName   :: Loc name
                    , fParams :: [Loc name]
                    , fBody   :: FunBody name
                    } deriving (Functor,Show)

data Ann = AnnSym L.Text
         | AnnApp L.Text [Ann]
         | AnnArr [Ann]
         | AnnObj [(L.Text,Ann)]
         | AnnStr L.Text
         | AnnCode L.Text L.Text
         | AnnLoc (Loc Ann)
           deriving (Show)

data StateVar name = StateVar { svName   :: Loc name
                              , svType   :: Type name
                              , svBounds :: Maybe (Loc (Int,Int))
                              , svInit   :: Maybe (Expr name)
                              , svOutName:: Maybe (Loc L.Text)
                              } deriving (Functor,Show)

data FunBody name = FBSpec [Spec name]
                  | FBExpr (Expr name)
                    deriving (Functor,Show)

data Type name = TBool
               | TInt
               | TEnum name
               | TFun (Type name) (Type name)
               | TLoc (Loc (Type name))
                 deriving (Functor,Show)

data Expr name = EVar name
               | ECon name
               | ENum Integer
               | ETrue
               | EFalse
               | EAnd (Expr name) (Expr name)
               | EOr  (Expr name) (Expr name)
               | ENot (Expr name)
               | EIf (Expr name) (Expr name) (Expr name)
               | ECase (Expr name) [Case name]
               | EApp (Expr name) (Expr name)
                 -- ^ Function application
               | ENext (Expr name)
               | EEq (Expr name) (Expr name)
               | ENeq (Expr name) (Expr name)
               | EImp (Expr name) (Expr name)
               | EIff (Expr name) (Expr name)

               | ESet [Expr name]
               | EIn (Expr name) (Expr name)
                 -- ^ @ x <- { a, b, c } @

               | EAny
               | EAll
               | EMutex

               | ELoc (Loc (Expr name))
                 deriving (Functor,Show)

data Case name = CPat (Pat name) (Expr name)
               | CDefault (Expr name)
               | CLoc (Loc (Case name))
                 deriving (Functor,Show)

data Pat name = PCon name
              | PNum Integer
              | PLoc (Loc (Pat name))
                deriving (Functor,Show)


-- Location Helpers ------------------------------------------------------------

instance HasLoc (Controller name) where
  type LocSource (Controller name) = FilePath
  getLoc Controller { cName, cDecls } = mappend (getLoc cName) (getLoc cDecls)

instance HasLoc (TopDecl name) where
  type LocSource (TopDecl name) = FilePath
  getLoc (TDLoc loc) = getLoc loc
  getLoc _           = mempty

instance HasLoc (Spec name) where
  type LocSource (Spec name) = FilePath
  getLoc (SLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (EnumDef name) where
  type LocSource (EnumDef name) = FilePath
  getLoc EnumDef { eName, eCons } =
    mconcat (getLoc eName : [ getLoc con | (con,_) <- eCons ])

instance HasLoc Ann where
  type LocSource Ann = FilePath
  getLoc (AnnLoc loc) = getLoc loc
  getLoc _            = mempty

instance HasLoc (FunBody name) where
  type LocSource (FunBody name) = FilePath
  getLoc (FBSpec xs) = getLoc xs
  getLoc (FBExpr e)  = getLoc e

instance HasLoc (Expr name) where
  type LocSource (Expr name) = FilePath
  getLoc (ELoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (Type name) where
  type LocSource (Type name) = FilePath
  getLoc (TLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (StateVar name) where
  type LocSource (StateVar name) = FilePath
  getLoc StateVar { svName, svType, svInit } =
    mconcat [ getLoc svName, getLoc svType, getLoc svInit ]

instance HasLoc (Case name) where
  type LocSource (Case name) = FilePath
  getLoc (CLoc loc) = getLoc loc
  getLoc _          = mempty

instance HasLoc (Pat name) where
  type LocSource (Pat name) = FilePath
  getLoc (PLoc loc) = getLoc loc
  getLoc _          = mempty


-- Name Functions --------------------------------------------------------------

-- | Give the value-level names introduced by this declaration.
topDeclDs :: Ord name => TopDecl name -> Set.Set name
topDeclDs (TDEnum enum)   = enumDs enum
topDeclDs (TDFun fun)     = funDs fun
topDeclDs (TDInput sv)    = stateVarDs sv
topDeclDs (TDOutput sv)   = stateVarDs sv
topDeclDs TDSpec{}        = Set.empty
topDeclDs TDExpr{}        = Set.empty
topDeclDs (TDLoc loc)     = topDeclDs (thing loc)


-- | The names of all the constructors.
enumDs :: Ord name => EnumDef name -> Set.Set name
enumDs EnumDef { eCons } = Set.fromList [ thing con | (con,_) <- eCons ]


-- | The name of this function.
funDs :: Fun name -> Set.Set name
funDs Fun { fName } = Set.singleton (thing fName)


-- | The name of this state var
stateVarDs :: StateVar name -> Set.Set name
stateVarDs StateVar { svName } = Set.singleton (thing svName)


-- | Free variables in this top-level declaration.
topDeclFvs :: Ord name => TopDecl name -> Set.Set name
topDeclFvs TDEnum {}          = Set.empty
topDeclFvs (TDFun fun)        = funFvs fun
topDeclFvs (TDInput sv)       = stateVarFvs sv
topDeclFvs (TDOutput sv)      = stateVarFvs sv
topDeclFvs (TDSpec s)         = specFvs s
topDeclFvs (TDExpr s)         = exprFvs s
topDeclFvs (TDLoc loc)        = topDeclFvs (thing loc)

-- | Free variables used in this specification.
specFvs :: Ord name => Spec name -> Set.Set name
specFvs (SSysTrans    es) = foldMap exprFvs es
specFvs (SSysLiveness es) = foldMap exprFvs es
specFvs (SEnvTrans    es) = foldMap exprFvs es
specFvs (SEnvLiveness es) = foldMap exprFvs es
specFvs (SLoc loc)        = foldMap specFvs loc 

-- | The free variables of the function body.
funFvs :: Ord name => Fun name -> Set.Set name
funFvs Fun { fParams, fBody } = 
  let fvs = funBodyFvs fBody
      bvs = Set.fromList (map thing fParams)
   in fvs Set.\\ bvs

funBodyFvs :: Ord name => FunBody name -> Set.Set name
funBodyFvs (FBSpec ps) = foldMap specFvs ps
funBodyFvs (FBExpr e)  = exprFvs e

-- | The free variables of the state-variable initializer.
stateVarFvs :: Ord name => StateVar name -> Set.Set name
stateVarFvs StateVar { svName, svInit } =
  let fvs = maybe Set.empty exprFvs svInit
   in Set.delete (thing svName) fvs

-- | The names that appear free in this expression.
exprFvs :: Ord name => Expr name -> Set.Set name
exprFvs (EVar name) = Set.singleton name
exprFvs (ECon name) = Set.singleton name
exprFvs ENum{}      = Set.empty
exprFvs ETrue       = Set.empty
exprFvs EFalse      = Set.empty
exprFvs (EAnd l r)  = Set.union (exprFvs l) (exprFvs r)
exprFvs (EOr  l r)  = Set.union (exprFvs l) (exprFvs r)
exprFvs (ENot e)    = exprFvs e
exprFvs (EIf a b c) = Set.unions [ exprFvs a, exprFvs b, exprFvs c ]
exprFvs (EApp f x)  = Set.union (exprFvs f) (exprFvs x)
exprFvs (ENext e)   = exprFvs e
exprFvs (EEq a b)   = Set.union (exprFvs a) (exprFvs b)
exprFvs (ENeq a b)  = Set.union (exprFvs a) (exprFvs b)
exprFvs (EImp a b)  = Set.union (exprFvs a) (exprFvs b)
exprFvs (EIff a b)  = Set.union (exprFvs a) (exprFvs b)
exprFvs (ECase e gs) = Set.union (exprFvs e) (foldMap caseFvs gs)
exprFvs (ESet es)   = Set.unions (map exprFvs es)
exprFvs (EIn a b)   = Set.union (exprFvs a) (exprFvs b)
exprFvs EAny        = Set.empty
exprFvs EAll        = Set.empty
exprFvs EMutex      = Set.empty
exprFvs (ELoc loc)  = exprFvs (thing loc)

caseFvs :: Ord name => Case name -> Set.Set name
caseFvs (CPat p e)   = Set.union (patFvs p) (exprFvs e)
caseFvs (CDefault e) = exprFvs e
caseFvs (CLoc loc)   = caseFvs (thing loc)

patFvs :: Ord name => Pat name -> Set.Set name
patFvs (PCon p)   = Set.singleton p
patFvs (PNum _)   = Set.empty
patFvs (PLoc loc) = patFvs (thing loc)
