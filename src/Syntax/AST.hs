{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module Syntax.AST where

import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import           Text.Location


type Loc = Located FilePath

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
                  | TDSysTrans (Expr name)
                  | TDSysLiveness (Expr name)
                  | TDEnvTrans (Expr name)
                  | TDEnvLiveness (Expr name)
                  | TDLoc (Loc (TopDecl name))
                    deriving (Functor,Show)


isFun :: TopDecl name -> Bool
isFun (TDLoc loc) = isFun (thing loc)
isFun TDFun{}     = True
isFun _           = False


data EnumDef name = EnumDef { eName :: Loc name
                            , eCons :: [Loc name]
                            } deriving (Functor,Show)


data Fun name = Fun { fName :: Loc name
                    , fParams :: [Loc name]
                    , fBody :: [Guard name]
                    } deriving (Functor,Show)

data StateVar name = StateVar { svName :: Loc name
                              , svType :: Type name
                              , svInit :: Maybe (Expr name)
                              } deriving (Functor,Show)

data Guard name = GGuard (Expr name) (Expr name)
                | GExpr (Expr name)
                | GLoc (Loc (Guard name))
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
               | ECase [Guard name]
               | EApp (Expr name) (Expr name)
                 -- ^ Function application
               | ENext (Expr name)
               | EEq (Expr name) (Expr name)
               | ENeq (Expr name) (Expr name)
               | EImp (Expr name) (Expr name)
               | ELoc (Loc (Expr name))
                 deriving (Functor,Show)


-- Location Helpers ------------------------------------------------------------

instance HasLoc (Controller name) where
  type LocSource (Controller name) = FilePath
  getLoc Controller { cName, cDecls } = mappend (getLoc cName) (getLoc cDecls)

instance HasLoc (TopDecl name) where
  type LocSource (TopDecl name) = FilePath
  getLoc (TDLoc loc) = getLoc loc
  getLoc _           = mempty

instance HasLoc (EnumDef name) where
  type LocSource (EnumDef name) = FilePath
  getLoc EnumDef { eName, eCons } = foldMap getLoc (eName : eCons)

instance HasLoc (Guard name) where
  type LocSource (Guard name) = FilePath
  getLoc (GLoc loc) = getLoc loc
  getLoc _          = mempty

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


-- Name Functions --------------------------------------------------------------

-- | Give the value-level names introduced by this declaration.
topDeclDs :: Ord name => TopDecl name -> Set.Set name
topDeclDs (TDEnum enum)   = enumDs enum
topDeclDs (TDFun fun)     = funDs fun
topDeclDs (TDInput sv)    = stateVarDs sv
topDeclDs (TDOutput sv)   = stateVarDs sv
topDeclDs TDSysTrans   {} = Set.empty
topDeclDs TDSysLiveness{} = Set.empty
topDeclDs TDEnvTrans   {} = Set.empty
topDeclDs TDEnvLiveness{} = Set.empty
topDeclDs (TDLoc loc)     = topDeclDs (thing loc)


-- | The names of all the constructors.
enumDs :: Ord name => EnumDef name -> Set.Set name
enumDs EnumDef { eCons } = Set.fromList [ thing con | con <- eCons ]


-- | The name of this function.
funDs :: Fun name -> Set.Set name
funDs Fun { fName } = Set.singleton (thing fName)


-- | The name of this state var
stateVarDs :: StateVar name -> Set.Set name
stateVarDs StateVar { svName } = Set.singleton (thing svName)


-- | Free variables in this top-level declaration.
topDeclFvs :: Ord name => TopDecl name -> Set.Set name
topDeclFvs TDEnum {}         = Set.empty
topDeclFvs (TDFun fun)       = funFvs fun
topDeclFvs (TDInput sv)      = stateVarFvs sv
topDeclFvs (TDOutput sv)     = stateVarFvs sv
topDeclFvs (TDSysTrans    e) = exprFvs e
topDeclFvs (TDSysLiveness e) = exprFvs e
topDeclFvs (TDEnvTrans    e) = exprFvs e
topDeclFvs (TDEnvLiveness e) = exprFvs e
topDeclFvs (TDLoc loc)       = topDeclFvs (thing loc)

-- | The free variables of the function body.
funFvs :: Ord name => Fun name -> Set.Set name
funFvs Fun { fParams, fBody } = 
  let fvs = Set.unions (map guardFvs fBody)
      bvs = Set.fromList (map thing fParams)
   in fvs Set.\\ bvs

guardFvs :: Ord name => Guard name -> Set.Set name
guardFvs (GGuard p e)  = Set.union (exprFvs p)  (exprFvs e)
guardFvs (GExpr e)     = exprFvs e
guardFvs (GLoc loc)    = guardFvs (thing loc)

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
exprFvs (ELoc loc)  = exprFvs (thing loc)
