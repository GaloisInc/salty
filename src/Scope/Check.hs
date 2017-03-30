{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Scope.Check (Renamed,scopeCheck) where

import SrcLoc (HasSrcLoc(..),SrcLoc())
import Scope.Name
import Syntax.AST

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           MonadLib
import           PP


data Renamed

type instance AnnotOf Renamed = SrcLoc
type instance NameOf  Renamed = Name


-- | Resolve names to their binding sites.
scopeCheck :: Supply -> Controller Parsed
           -> Either [ScopeError] (Controller Renamed,Supply)
scopeCheck sup c =
  let origin    = FromController (annLoc c)
      (cont,s') = mkName origin (pnameText (cName c)) Nothing sup
      (ds',rw)  = runM (unSC (checkTopDecls (cDecls c)))
                       RW { rwEnv  = Map.empty
                          , rwErrs = []
                          , rwSup  = s'
                          , rwLoc  = cAnnot c
                          , rwCont = cont }
  in case rwErrs rw of
       [] -> Right (Controller (cAnnot c) cont ds', rwSup rw)
       es -> Left es


data ScopeError = Unknown PName
                | Duplicate PName [Name]
                | Ambiguous PName [Name]
                  deriving (Show)

instance HasSrcLoc ScopeError where
  srcLoc (Unknown pn)     = srcLoc pn
  srcLoc (Duplicate pn _) = srcLoc pn
  srcLoc (Ambiguous pn _) = srcLoc pn

instance PP ScopeError where
  ppPrec _ (Unknown pn) =
    text "Unknown identifier:" <+> pp pn

  ppPrec _ (Duplicate x ns) =
    hang (text "Identifier " <+> ticks (pp x) <+>
          text "defined in multiple places:")
       2 (vcat [ char '*' <+> ppOrigin (nameOrigin n) | n <- ns ])

  ppPrec _ (Ambiguous pn ns) =
    hang (text "Ambiguous use of " <+> ticks (pp pn) <+>
          text "could be one of:")
       2 (vcat [ char '*' <+> ppOrigin (nameOrigin n) | n <- ns ])


-- Monad -----------------------------------------------------------------------

newtype SC a = SC { unSC :: StateT RW Lift a
                  } deriving (Functor,Applicative,Monad)

data RW = RW { rwEnv  :: !Names
             , rwErrs :: [ScopeError]
             , rwSup  :: !Supply
             , rwLoc  :: !SrcLoc
             , rwCont :: !Name
             }

withLoc :: HasSrcLoc range => range -> SC a -> SC a
withLoc r m = SC $
  do rw  <- get
     set $! rw { rwLoc = srcLoc r }
     a   <- unSC m
     rw' <- get
     set rw' { rwLoc = rwLoc rw }
     return a

withLoc_ :: HasSrcLoc a => (a -> SC b) -> a -> SC b
withLoc_ f loc = withLoc (srcLoc loc) (f loc)

askLoc :: SC SrcLoc
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

newName :: Origin -> PName -> Maybe T.Text -> SC Name
newName from txt mbOut = SC $
  do RW { .. } <- get
     let (n,s') = mkName from (pnameText txt) mbOut rwSup
     set RW { rwSup = s', .. }
     return n

newParam :: Name -> PName -> SC Name
newParam fun name =
  do loc <- askLoc
     newName (FromParam loc fun) name Nothing

newDecl :: Maybe Name -> PName -> SC Name
newDecl mbParent name =
  do RW { .. } <- SC get
     newName (FromDecl rwLoc (fromMaybe rwCont mbParent)) name Nothing

newStateVar :: PName -> Maybe T.Text -> SC Name
newStateVar name mbOutName =
  do RW { .. } <- SC get
     newName (FromDecl rwLoc rwCont) name mbOutName

newConstr :: Name -> (PName, Maybe T.Text) -> SC Name
newConstr origin (name,mbOut) = withLoc name $
  do RW { .. } <- SC get
     newName (FromDecl rwLoc origin) name mbOut


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


type NamesFrom f = f Parsed -> SC Names

-- | Names defined by a top-level declaration.
topDeclNames :: NamesFrom TopDecl

topDeclNames (TDEnum   _ enum) = enumNames enum
topDeclNames (TDFun    _ fun)  = funNames Nothing fun
topDeclNames (TDInput  _ sv)   = stateVarNames sv
topDeclNames (TDOutput _ sv)   = stateVarNames sv

-- specifications and top-level expressions don't introduce names
topDeclNames TDSpec{}      = return Map.empty
topDeclNames TDExpr{}      = return Map.empty

enumNames :: NamesFrom EnumDef
enumNames EnumDef { .. } =
  do tyName <- withLoc_ (newDecl Nothing) eName
     cs     <- traverse (newConstr tyName) eCons
     return $ Map.fromList
            $ (eName, [tyName])
            : zip (map fst eCons) (map pure cs)

funNames :: Maybe Name -> NamesFrom Fun
funNames mbParent Fun { .. } =
  do name <- withLoc_ (newDecl mbParent) fName
     return (Map.singleton fName [name])

stateVarNames :: NamesFrom StateVar
stateVarNames StateVar { .. } =
  do name <- withLoc_ (`newStateVar` svOutName) svName
     return (Map.singleton svName [name])


-- Name Resolution -------------------------------------------------------------

resolve :: PName -> SC Name
resolve pn = withLoc pn $
  do RW { .. } <- SC get
     case Map.lookup pn rwEnv of
       Just [n] -> return n
       Just []  -> error "Invalid naming environment"
       Just ns  -> do addErrs [Ambiguous pn ns]
                      return (head ns)

       -- name missing from the environment
       Nothing  -> do n <- newDecl Nothing pn
                      addErrs [Unknown pn]
                      return n

type Check f = f Parsed -> SC (f Renamed)

checkTopDecls :: [TopDecl Parsed] -> SC [TopDecl Renamed]
checkTopDecls ds =
  do envs <- traverse topDeclNames ds
     let (env,errs) = mergeWithErrors envs
     addErrs errs

     withNames env (traverse checkTopDecl ds)

checkAnn :: Check Ann
checkAnn (AnnSym  loc x)    = pure (AnnSym loc x)
checkAnn (AnnStr  loc x)    = pure (AnnStr loc x)
checkAnn (AnnCode loc x y)  = pure (AnnCode loc x y)
checkAnn (AnnApp  loc x xs) = withLoc  loc (AnnApp  loc x `fmap` traverse checkAnn xs)
checkAnn (AnnArr  loc xs)   = withLoc  loc (AnnArr  loc   `fmap` traverse checkAnn xs)
checkAnn (AnnObj  loc ps)   = withLoc loc $
  do ps' <- forM ps $ \ (f,a) ->
            do a' <- checkAnn a
               return (f, a')
     pure (AnnObj loc ps')

checkTopDecl :: Check TopDecl
checkTopDecl (TDEnum   loc enum) = withLoc loc (TDEnum   loc `fmap` checkEnum enum)
checkTopDecl (TDFun    loc fun)  = withLoc loc (TDFun    loc `fmap` checkFun  fun)
checkTopDecl (TDInput  loc sv)   = withLoc loc (TDInput  loc `fmap` checkStateVar sv)
checkTopDecl (TDOutput loc sv)   = withLoc loc (TDOutput loc `fmap` checkStateVar sv)
checkTopDecl (TDSpec   loc s)    = withLoc loc (TDSpec   loc `fmap` checkSpec s)
checkTopDecl (TDExpr   loc e)    = withLoc loc (TDExpr   loc `fmap` checkExpr e)

checkSpec :: Check Spec
checkSpec (SSysTrans    loc e) = withLoc loc (SSysTrans    loc `fmap` traverse checkExpr e)
checkSpec (SEnvTrans    loc e) = withLoc loc (SEnvTrans    loc `fmap` traverse checkExpr e)
checkSpec (SSysLiveness loc e) = withLoc loc (SSysLiveness loc `fmap` traverse checkExpr e)
checkSpec (SEnvLiveness loc e) = withLoc loc (SEnvLiveness loc `fmap` traverse checkExpr e)
checkSpec (SSysInit     loc e) = withLoc loc (SSysInit     loc `fmap` traverse checkExpr e)
checkSpec (SEnvInit     loc e) = withLoc loc (SEnvInit     loc `fmap` traverse checkExpr e)

checkEnum :: Check EnumDef
checkEnum EnumDef { .. } = withLoc eAnnot $
  do a'  <- traverse checkAnn eAnn
     n'  <- resolve eName
     cs' <- traverse checkCon eCons
     return EnumDef { eAnn = a', eName = n', eCons = cs', .. }


checkCon :: (PName, Maybe T.Text) -> SC (Name, Maybe T.Text)
checkCon (lname, mbOut) =
  do lname' <- resolve lname
     return (lname', mbOut)


checkFun :: Check Fun
checkFun Fun { .. } =
  do a' <- traverse checkAnn fAnn
     n' <- resolve fName
     withParams n' fParams $ \ps' ->
       do b' <- checkFunBody fBody
          return Fun { fAnn    = a'
                     , fName   = n'
                     , fParams = ps'
                     , fBody   = b'
                     , .. }


checkStateVar :: Check StateVar
checkStateVar StateVar { .. } =
  do a'    <- traverse checkAnn svAnn
     n'    <- resolve svName
     ty'   <- checkType svType
     init' <- traverse checkExpr svInit
     bs'   <- traverse checkBounds svBounds
     return $! StateVar { svAnn = a', svName = n', svType = ty', svInit = init'
                        , svBounds = bs', .. }


-- | Rebuild the bounds as renamed.
checkBounds :: Check Bounds
checkBounds Bounds { .. } = pure Bounds { .. }


withParams :: Name -> [PName] -> ([Name] -> SC a) -> SC a
withParams fun ps k = go [] ps
  where
  go acc [] =
    let rev = reverse acc
        env = Map.fromList [ (old,[new]) | (old,new) <- rev ]
        -- XXX collect shadowing warnings
     in withNames env (k (map snd rev))

  go acc (n:ns) =
    do n' <- newParam fun n
       go ((n,n'):acc) ns


checkFunBody :: Check FunBody
checkFunBody (FBSpec loc ps) = withLoc loc (FBSpec loc `fmap` traverse checkSpec ps)
checkFunBody (FBExpr loc e)  = withLoc loc (FBExpr loc `fmap` checkExpr e)

checkExpr :: Check Expr
checkExpr (EVar  loc n)     = withLoc loc (EVar  loc `fmap` resolve n)
checkExpr (ECon  loc n)     = withLoc loc (ECon  loc `fmap` resolve n)
checkExpr (EAnd  loc l r)   = withLoc loc (EAnd  loc `fmap` checkExpr l <*> checkExpr r)
checkExpr (EOr   loc l r)   = withLoc loc (EOr   loc `fmap` checkExpr l <*> checkExpr r)
checkExpr (ENot  loc e)     = withLoc loc (ENot  loc `fmap` checkExpr e)
checkExpr (EIf   loc p t f) = withLoc loc (EIf   loc `fmap` checkExpr p <*> checkExpr t <*> checkExpr f)
checkExpr (EApp  loc f x)   = withLoc loc (EApp  loc `fmap` checkExpr f <*> checkExpr x)
checkExpr (ENext loc e)     = withLoc loc (ENext loc `fmap` checkExpr e)
checkExpr (EEq   loc a b)   = withLoc loc (EEq   loc `fmap` checkExpr a <*> checkExpr b)
checkExpr (ENeq  loc a b)   = withLoc loc (ENeq  loc `fmap` checkExpr a <*> checkExpr b)
checkExpr (EImp  loc a b)   = withLoc loc (EImp  loc `fmap` checkExpr a <*> checkExpr b)
checkExpr (EIff  loc a b)   = withLoc loc (EIff  loc `fmap` checkExpr a <*> checkExpr b)
checkExpr (ECase loc e cs)  = withLoc loc (ECase loc `fmap` checkExpr e <*> traverse checkCase cs)
checkExpr (ESet  loc es)    = withLoc loc (ESet  loc `fmap` traverse checkExpr es)
checkExpr (EIn   loc a b)   = withLoc loc (EIn   loc `fmap` checkExpr a <*> checkExpr b)

checkExpr (ENum   loc i) = pure (ENum   loc i)
checkExpr (ETrue  loc)   = pure (ETrue  loc)
checkExpr (EFalse loc)   = pure (EFalse loc)
checkExpr (EAny   loc)   = pure (EAny   loc)
checkExpr (EAll   loc)   = pure (EAll   loc)
checkExpr (EMutex loc)   = pure (EMutex loc)


checkCase :: Check Case
checkCase (CPat     loc p e) = withLoc loc (CPat     loc `fmap` checkPat p <*> checkExpr e)
checkCase (CDefault loc e)   = withLoc loc (CDefault loc `fmap` checkExpr e)

checkPat :: Check Pat
checkPat (PCon loc n) = withLoc loc (PCon loc `fmap` resolve n)
checkPat (PNum loc n) = pure (PNum loc n)


checkType :: Check Type
checkType (TBool loc)     = pure (TBool loc)
checkType (TInt  loc)     = pure (TInt loc)
checkType (TEnum loc n)   = withLoc loc (TEnum loc `fmap` resolve n)
checkType (TFun  loc a b) = withLoc loc (TFun  loc `fmap` checkType a <*> checkType b)
