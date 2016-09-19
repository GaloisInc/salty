{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Scope.Check (scopeCheck) where

import Scope.Name
import Syntax.AST

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as L
import           MonadLib
import           Text.Location (HasLoc(..),Located(..),Range,at,thing)


-- | Resolve names to their binding sites.
scopeCheck :: Supply -> Controller PName
           -> Either [ScopeError] (Controller Name,Supply)
scopeCheck sup c =
  let origin    = FromController (getLoc c)
      (cont,s') = mkName origin (locValue (cName c)) Nothing sup
      (ds',rw)  = runM (unSC (checkTopDecls (cDecls c)))
                       RW { rwEnv  = Map.empty
                          , rwErrs = []
                          , rwSup  = s'
                          , rwLoc  = locRange (cName c)
                          , rwCont = cont }
  in case rwErrs rw of
       [] -> Right (Controller (cont `at` cName c) ds', rwSup rw)
       es -> Left es


data ScopeError = Unknown (Loc PName)
                | Duplicate PName [Name]
                | Ambiguous (Loc PName) [Name]
                  deriving (Eq,Show)


-- Monad -----------------------------------------------------------------------

newtype SC a = SC { unSC :: StateT RW Lift a
                  } deriving (Functor,Applicative,Monad)

data RW = RW { rwEnv  :: !Names
             , rwErrs :: [ScopeError]
             , rwSup  :: !Supply
             , rwLoc  :: !(Range FilePath)
             , rwCont :: !Name
             }

withLoc :: (LocSource range ~ FilePath, HasLoc range) => range -> SC a -> SC a
withLoc range m = SC $
  do rw  <- get
     set $! rw { rwLoc = getLoc range }
     a   <- unSC m
     rw' <- get
     set rw' { rwLoc = rwLoc rw }
     return a

withLoc_ :: (a -> SC b) -> Loc a -> SC b
withLoc_ f loc = withLoc loc (f (thing loc))

askLoc :: SC (Range FilePath)
askLoc  = SC $
  do RW { .. } <- get
     return rwLoc

addErrs :: [ScopeError] -> SC ()
addErrs errs = SC $
  do RW { .. } <- get
     set $! RW { rwErrs = errs ++ rwErrs, .. }

withNames :: Names -> SC a -> SC a
withNames ns m = SC $
  do rw  <- get
     set $! rw { rwEnv = Map.unionWith (++) ns (rwEnv rw) }
     a   <- unSC m
     rw' <- get
     set $! rw' { rwEnv = rwEnv rw }
     return a


-- Name Introduction -----------------------------------------------------------

newName :: Origin -> L.Text -> Maybe L.Text -> SC Name
newName from txt mbOut = SC $
  do RW { .. } <- get
     let (n,s') = mkName from txt mbOut rwSup
     set RW { rwSup = s', .. }
     return n

newParam :: Name -> L.Text -> SC Name
newParam fun name =
  do loc <- askLoc
     newName (FromParam loc fun) name Nothing

newDecl :: Maybe Name -> L.Text -> SC Name
newDecl mbParent name =
  do RW { .. } <- SC get
     newName (FromDecl rwLoc (fromMaybe rwCont mbParent)) name Nothing

newStateVar :: L.Text -> Maybe L.Text -> SC Name
newStateVar name mbOutName =
  do RW { .. } <- SC get
     newName (FromDecl rwLoc rwCont) name mbOutName


-- Name Mappings ---------------------------------------------------------------

type Names = Map.Map PName [Name]

-- | Merge naming environments, producing errors on overlap. As the errors are
-- eagerly produced, the environment is updated to lack overlap.
mergeWithErrors :: [Names] -> (Names,[ScopeError])
mergeWithErrors envs =
  let env  = Map.unionsWith (++) envs
      dups = Map.filter (\ns -> length ns > 1) env
      errs = [ Duplicate k ns | (k,ns) <- Map.toList dups ]
      mkSingleton ns = case ns of
                         [_] -> ns
                         []  -> []
                         _   -> [head ns]
   in (fmap mkSingleton env,errs)


type NamesFrom f = f PName -> SC Names

-- | Names defined by a top-level declaration.
topDeclNames :: NamesFrom TopDecl

topDeclNames (TDEnum enum) = enumNames enum
topDeclNames (TDFun fun)   = funNames Nothing fun
topDeclNames (TDInput sv)  = stateVarNames sv
topDeclNames (TDOutput sv) = stateVarNames sv
-- specifications and top-level expressions don't introduce names
topDeclNames TDSpec{}      = return Map.empty
topDeclNames TDExpr{}      = return Map.empty
topDeclNames (TDLoc loc)   = topDeclNames (thing loc)

enumNames :: NamesFrom EnumDef
enumNames EnumDef { .. } =
  do tyName <- withLoc_ (newDecl Nothing) eName
     cs     <- traverse (withLoc_ (newDecl (Just tyName))) eCons
     return $ Map.fromList
            $ (thing eName, [tyName])
            : zip (map thing eCons) (map pure cs)

funNames :: Maybe Name -> NamesFrom Fun
funNames mbParent Fun { .. } =
  do name <- withLoc_ (newDecl mbParent) fName
     return (Map.singleton (thing fName) [name])

stateVarNames :: NamesFrom StateVar
stateVarNames StateVar { .. } =
  do name <- withLoc_ (`newStateVar` fmap thing svOutName) svName
     return (Map.singleton (thing svName) [name])


-- Name Resolution -------------------------------------------------------------

resolve :: PName -> SC Name
resolve pn =
  do RW { .. } <- SC get
     case Map.lookup pn rwEnv of
       Just [n] -> return n
       Just []  -> error "Invalid naming environment"
       Just ns  -> do addErrs [Ambiguous (pn `at` rwLoc) ns]
                      return (head ns)

       -- name missing from the environment
       Nothing  -> do n <- newDecl Nothing pn
                      addErrs [Unknown (pn `at` rwLoc)]
                      return n

checkLoc :: Loc a -> (a -> SC b) -> SC (Loc b)
checkLoc Located { .. } check = withLoc locRange $
  do v' <- check locValue
     return Located { locValue = v', .. }


type Check f = f PName -> SC (f Name)

checkTopDecls :: [TopDecl PName] -> SC [TopDecl Name]
checkTopDecls ds =
  do envs <- traverse topDeclNames ds
     let (env,errs) = mergeWithErrors envs
     addErrs errs

     withNames env (traverse checkTopDecl ds)


checkTopDecl :: Check TopDecl
checkTopDecl (TDEnum enum)     = TDEnum        `fmap` checkEnum enum
checkTopDecl (TDFun fun)       = TDFun         `fmap` checkFun  fun
checkTopDecl (TDInput sv)      = TDInput       `fmap` checkStateVar sv
checkTopDecl (TDOutput sv)     = TDOutput      `fmap` checkStateVar sv
checkTopDecl (TDSpec s)        = TDSpec        `fmap` checkSpec s
checkTopDecl (TDExpr e)        = TDExpr        `fmap` checkExpr e
checkTopDecl (TDLoc loc)       = TDLoc         `fmap` checkLoc loc checkTopDecl

checkSpec :: Check Spec
checkSpec (SSysTrans e)    = SSysTrans    `fmap` traverse checkExpr e
checkSpec (SEnvTrans e)    = SEnvTrans    `fmap` traverse checkExpr e
checkSpec (SSysLiveness e) = SSysLiveness `fmap` traverse checkExpr e
checkSpec (SEnvLiveness e) = SEnvLiveness `fmap` traverse checkExpr e
checkSpec (SLoc loc)       = SLoc         `fmap` checkLoc loc checkSpec

checkEnum :: Check EnumDef
checkEnum EnumDef { .. } =
  do n'  <- checkLoc eName resolve
     cs' <- traverse (`checkLoc` resolve) eCons
     return EnumDef { eName = n', eCons = cs' }


checkFun :: Check Fun
checkFun Fun { .. } =
  do n' <- checkLoc fName resolve
     withParams (thing n') fParams $ \ps' ->
       do b' <- checkFunBody fBody
          return Fun { fName   = n'
                     , fParams = ps'
                     , fBody   = b' }


checkStateVar :: Check StateVar
checkStateVar StateVar { .. } =
  do n'    <- checkLoc svName resolve
     ty'   <- checkType svType
     init' <- traverse checkExpr svInit
     return $! StateVar { svName = n', svType = ty', svInit = init', .. }


withParams :: Name -> [Loc PName] -> ([Loc Name] -> SC a) -> SC a
withParams fun ps k = go [] ps
  where
  go acc [] =
    let rev = reverse acc
        env = Map.fromList [ (old,[thing new]) | (old,new) <- rev ]
        -- XXX collect shadowing warnings
     in withNames env (k (map snd rev))

  go acc (n:ns) =
    do n' <- checkLoc n (newParam fun)
       go ((thing n,n'):acc) ns


checkFunBody :: Check FunBody
checkFunBody (FBSpec ps) = FBSpec `fmap` traverse checkSpec ps
checkFunBody (FBExpr e)  = FBExpr `fmap` checkExpr e

checkExpr :: Check Expr
checkExpr (EVar n) = EVar `fmap` resolve n
checkExpr (ECon n) = ECon `fmap` resolve n
checkExpr (ENum i) = return (ENum i)
checkExpr ETrue    = return ETrue
checkExpr EFalse   = return EFalse


checkExpr (EAnd l r)  = EAnd `fmap` checkExpr l <*> checkExpr r
checkExpr (EOr  l r)  = EOr  `fmap` checkExpr l <*> checkExpr r
checkExpr (ENot e)    = ENot `fmap` checkExpr e
checkExpr (EIf p t f) = EIf  `fmap` checkExpr p <*> checkExpr t <*> checkExpr f
checkExpr (EApp f x)  = EApp `fmap` checkExpr f <*> checkExpr x
checkExpr (ELoc loc)  = ELoc `fmap` checkLoc loc checkExpr
checkExpr (ENext e)   = ENext `fmap` checkExpr e
checkExpr (EEq a b)   = EEq  `fmap` checkExpr a <*> checkExpr b
checkExpr (ENeq a b)  = ENeq `fmap` checkExpr a <*> checkExpr b
checkExpr (EImp a b)  = EImp `fmap` checkExpr a <*> checkExpr b
checkExpr (EIff a b)  = EIff `fmap` checkExpr a <*> checkExpr b
checkExpr (ECase e cs)= ECase `fmap` checkExpr e <*> traverse checkCase cs

checkExpr EAny        = pure EAny
checkExpr EAll        = pure EAll
checkExpr EMutex      = pure EMutex

checkExpr (ESet es)   = ESet  `fmap` traverse checkExpr es
checkExpr (EIn a b)   = EIn   `fmap` checkExpr a <*> checkExpr b

checkCase :: Check Case
checkCase (CPat p e)   = CPat     `fmap` checkPat p <*> checkExpr e
checkCase (CDefault e) = CDefault `fmap` checkExpr e
checkCase (CLoc loc)   = CLoc     `fmap` checkLoc loc checkCase

checkPat :: Check Pat
checkPat (PCon n)   = PCon `fmap` resolve n
checkPat (PNum n)   = pure (PNum n)
checkPat (PLoc loc) = PLoc `fmap` checkLoc loc checkPat


checkType :: Check Type
checkType TBool      = return TBool
checkType TInt       = return TInt
checkType (TEnum n)  = TEnum `fmap` resolve n
checkType (TFun a b) = TFun  `fmap` checkType a <*> checkType b
checkType (TLoc loc) = TLoc  `fmap` checkLoc loc checkType
