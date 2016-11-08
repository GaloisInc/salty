-- |
-- Module      :  TypeCheck.Sat
-- Copyright   :  Galois, Inc. 2016
-- License     :  BSD3
--
-- Maintainer  :  trevor@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Sanity checking for GR(1) specifications using an SMT solver.
--

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}

module TypeCheck.Sanity (
    SanityMessage(..), sanityErrors, ppSanityMessage,
    sanityCheck,
  ) where

import           Message
import           PP hiding (ppOrigin)
import           Panic
import           Scope.Name (Name,nameText,nameUnique)
import           TypeCheck.AST

import           Control.Exception (bracket,catch)
import           Control.Monad (unless)
import           Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as L
import           Data.Traversable (forM)
import           MonadLib (StateT, runStateT, runStateT, get, set, inBase)
import qualified SimpleSMT as SMT


data SanityMessage = SafetyConflict [Loc Origin]
                     -- ^ Safety formulas at these locations are in conflict

                   | CouldntFindZ3
                     -- ^ Failed to start Z3, indicates that sanity checking was
                     -- not done.

                   | InitConflict [Loc Origin]
                     -- ^ Variable initialization is not consistent

                   | EnvLivenessSafety [Loc Origin]
                     -- ^ Environment liveness constraint will eventually cause
                     -- environmental safety constraints to be violated.

                   | SysLivenessSafety Bool [Loc Origin]
                     -- ^ A system liveness constraint will cause the system to
                     -- eventually violate safety.
                     deriving (Show)

ppSanityMessage :: SanityMessage -> Doc
ppSanityMessage (SafetyConflict locs) = ppMessage "[error]" $
  hang (text "Safety constraints are in conflict:")
     2 (bullets (map ppOrigin locs))

ppSanityMessage CouldntFindZ3 = ppMessage "[warning]" $
  text "Unable to find z3 (try passing the location with --z3)"

ppSanityMessage (InitConflict ms) = ppMessage "[error]" $
  hang (text "Initialization constraints violate safety:")
     2 (bullets (map ppOrigin ms))

ppSanityMessage (EnvLivenessSafety ms) = ppMessage "[warning]" $
  hang (text "Environment will eventually violate safety constraints:")
     2 (bullets (map ppOrigin ms))

ppSanityMessage (SysLivenessSafety err ms) = ppMessage ty $
  hang (text "System will eventually violate safety constraints:")
     2 (bullets (map ppOrigin ms))
  where
  ty | err       = "[error]"
     | otherwise = "[warning]"


isSanityError :: SanityMessage -> Bool
isSanityError SafetyConflict{}          = True
isSanityError InitConflict{}            = True
isSanityError (SysLivenessSafety err _) = err
isSanityError _                         = False

sanityErrors :: [SanityMessage] -> [SanityMessage]
sanityErrors  = filter isSanityError


-- | Run a sanity checking pass over a specification.
-- NOTE: this requires that the controller has been passed through the expand
-- pass, as it's unable to deal with macros.
sanityCheck :: Bool -> FilePath -> Controller -> IO [SanityMessage]
sanityCheck debug z3 cont =
  do mb <- runSMT debug z3 (checkController cont)
     case mb of
       Just (_,msgs) -> return msgs
       Nothing       -> return [CouldntFindZ3]

-- | Check that all of the safety constraints are satisfiable.
checkController :: Controller -> SMT ()
checkController cont@Controller { .. } = withScope $
  do mapM_ declareEnum cEnums

     mapM_ declareStateVar cInputs
     mapM_ declareStateVar cOutputs

     -- check safety constraints in all states
     safety <- assertSafety cont
     res    <- checkSat

     -- only continue checking if safety constraints are not violated, as
     -- everything else will fail without them.
     if res /= SMT.Sat
       then
         do ms <- getUnsatCore
            addMessage (SafetyConflict [ safety Map.! clause | clause <- ms ])

       else
         do withScope (checkInit cont safety)
            err <- withScope (checkEnvLiveness cont safety)
            withScope (checkSysLiveness err cont safety)

data Origin = SysTrans
            | EnvTrans
            | InputVar Name
            | OutputVar Name
            | EnvLiveness
            | SysLiveness
              deriving (Eq,Ord,Show)

ppOrigin :: Loc Origin -> Doc
ppOrigin loc =
  case thing loc of
    SysTrans    -> text "system safety constraint at"       <+> pp (getLoc loc)
    EnvTrans    -> text "environment safety constraint at"  <+> pp (getLoc loc)
    InputVar  n -> fsep [ text "initialization of input variable", ticks (pp (nameText n))
                        , text "at", pp (getLoc loc) ]
    OutputVar n -> fsep [ text "initialization output variable", ticks (pp (nameText n))
                        , text "at", pp (getLoc loc) ]

    EnvLiveness -> text "environment liveness constraint at" <+> pp (getLoc loc)
    SysLiveness -> text "system liveness constraint at" <+> pp (getLoc loc)


type LocMap = Map.Map String (Loc Origin)

-- | Assert all safety constraints from the controller.
assertSafety :: Controller -> SMT LocMap
assertSafety Controller { .. } =
 do es  <- traverse (exprToSMT . thing) (sEnvTrans cSpec)
    ss  <- traverse (exprToSMT . thing) (sSysTrans cSpec)
    ens <- assertNamed "env_trans" es
    sns <- assertNamed "sys_trans" ss
    return $ Map.fromList
           $ [ (n, SysTrans <$ loc) | n <- sns | loc <- sSysTrans cSpec ]
          ++ [ (n, EnvTrans <$ loc) | n <- ens | loc <- sEnvTrans cSpec ]



-- | Check that the initial state satisfies all safety constraints.
checkInit :: Controller -> LocMap -> SMT ()
checkInit cont locs =
  do locs' <- assertInit cont
     res   <- checkSat
     unless (res == SMT.Sat) $
       do ms <- getUnsatCore
          let env = locs `mappend` locs'
          addMessage (InitConflict [ env Map.! clause | clause <- ms ])

-- | Assert all variable initialization formulas.
assertInit :: Controller -> SMT LocMap
assertInit Controller { .. } =
  do ins  <- nameVars "input"  =<< traverse initInput  cInputs
     outs <- nameVars "output" =<< traverse initOutput cOutputs
     return (Map.fromList (ins ++ outs))
  where
  nameVars pfx mbs =
    do let (es,locs) = unzip (catMaybes mbs)
       ns <- assertNamed pfx es
       return (zip ns locs)

initInput :: StateVar -> SMT (Maybe (SMT.SExpr, Loc Origin))
initInput  = initVar InputVar

initOutput :: StateVar -> SMT (Maybe (SMT.SExpr, Loc Origin))
initOutput  = initVar OutputVar

initVar :: (Name -> Origin) -> StateVar -> SMT (Maybe (SMT.SExpr, Loc Origin))
initVar mkOrigin = \ StateVar { .. } ->
  case svInit of
    Just e ->
      do e' <- exprToSMT (EEq svType (EVar svType svName) e)
         return (Just (e', mkOrigin svName `at` getLoc svName))

    Nothing ->
         return Nothing


-- | Check for system liveness constraints that will eventually violate safety.
checkSysLiveness :: Bool -> Controller -> LocMap -> SMT ()
checkSysLiveness envWarns cont locs =
  do conflicts <- checkLiveness (sSysLiveness (cSpec cont)) locs
     forM_ conflicts $ \ (loc, safety) ->
       addMessage (SysLivenessSafety (not envWarns) (SysLiveness `at` loc : safety))

-- | Check for environmental liveness constraints that will eventually violate
-- environmental safety constraints.
checkEnvLiveness :: Controller -> LocMap -> SMT Bool
checkEnvLiveness cont locs =
  do conflicts <- checkLiveness (sEnvLiveness (cSpec cont)) locs
     forM_ conflicts $ \ (loc, safety) ->
       addMessage (EnvLivenessSafety (EnvLiveness `at` loc : safety))

     return (not (null conflicts))

-- | Assert liveness constraints one at a time, recording when they violate the
-- safety constraints of the specification.
checkLiveness :: [Loc Expr] -> LocMap -> SMT [(Loc SMT.SExpr, [Loc Origin])]
checkLiveness es safety =
  do rs <- forM es $ \ loc -> withScope $
        do e' <- exprToSMT (thing loc)
           assert e'
           res <- checkSat
           if res /= SMT.Sat
              then
                do ms <- getUnsatCore
                   return [ (e' `at` loc, [ safety Map.! m | m <- ms ]) ]
              else return []

     return (concat rs)



-- SMT Monad -------------------------------------------------------------------

type SMT = StateT RW IO

data RW = RW { rwSolver   :: !SMT.Solver
             , rwMessages :: [SanityMessage]
             }

addMessage :: SanityMessage -> SMT ()
addMessage msg =
  do RW { .. } <- get
     set $! RW { rwMessages = rwMessages ++ [msg], .. }

runSMT :: Bool -> FilePath -> SMT a -> IO (Maybe (a,[SanityMessage]))
runSMT debug z3 m =
  do logger <- debugLogger debug
     mb     <- bracket (SMT.newSolver z3 ["-smt2", "-in"] logger) SMT.stop body
                 `catch` handler

     return $ do (a,rw) <- mb
                 return (a, rwMessages rw)

  where

  body s =
    do SMT.setOption s ":produce-models" "true"
       SMT.setOption s ":produce-unsat-cores" "true"
       Just <$> runStateT RW { rwSolver   = s
                             , rwMessages = [] } m

  handler :: IOError -> IO (Maybe (a, RW))
  handler _ = return Nothing

debugLogger :: Bool -> IO (Maybe SMT.Logger)
debugLogger False = return Nothing
debugLogger True  =
  do l <- SMT.newLogger 0
     return (Just l)

withScope :: SMT a -> SMT a
withScope m =
  do rw      <- get
     (a,rw') <- inBase (bracket (SMT.push (rwSolver rw))
                                (\_ -> SMT.pop (rwSolver rw))
                                (\_ -> runStateT rw m))
     set $! rw'
     return a

withSolver :: (SMT.Solver -> IO a) -> SMT a
withSolver k =
  do RW { .. } <- get
     inBase (k rwSolver)

checkSat :: SMT SMT.Result
checkSat  = withSolver SMT.check

assert :: SMT.SExpr -> SMT ()
assert e = withSolver (`SMT.assert` e)

assertNamed :: String -> [SMT.SExpr] -> SMT [String]
assertNamed p es = withSolver (\s -> go s [] 0 es)
  where
  go _ names _ []     = return (reverse names)
  go s names i (x:xs) =
    do let name = p ++ "_" ++ show (i :: Int)
       SMT.assert s (SMT.fun "!" [x,SMT.Atom ":named",SMT.Atom name])
       go s (name:names) (i+1) xs


-- type Model = Map.Map Name SMT.Value

-- getModel :: Controller -> SMT Model
-- getModel Controller { .. } = withSolver $ \s ->
--   do xs <- SMT.getExprs s (map fst (inputs ++ outputs))
--      return (Map.fromList (resolve xs inputs ++ resolve xs outputs))
--   where
--   inputs  = [ (nameToVar svName, svName) | StateVar { .. } <- cInputs  ]
--   outputs = [ (nameToVar svName, svName) | StateVar { .. } <- cOutputs ]

--   resolve xs vars = [ (name, val) | (key, name) <- vars
--                                   , let Just val = lookup key xs ]


getUnsatCore :: SMT [String]
getUnsatCore  = withSolver $ \ s ->
  do res <- SMT.command s (SMT.List [SMT.Atom "get-unsat-core"])
     case res of
       SMT.List es -> return [ e | SMT.Atom e <- es ]
       _           -> panic ("Unexpected result from (get-unsat-core):\n" ++ show res)


-- Translation -----------------------------------------------------------------


-- | Translate a name to a unique name in the SMT embedding.
smtName :: Name -> String
smtName n = L.unpack (nameText n) ++ "_" ++ show (nameUnique n)


-- | Translate a name to a unique name in the SMT embedding.
nameToVar :: Name -> SMT.SExpr
nameToVar n = SMT.fun (smtName n) []


-- | Translate a type to an SMT type.
typeToSMT :: HasCallStack => Type -> SMT.SExpr
typeToSMT TBool     = SMT.tBool
typeToSMT TInt      = SMT.tInt
typeToSMT (TEnum n) = nameToVar n

typeToSMT TFun{}  = panic "Unexpected Fun type"
typeToSMT TSet{}  = panic "Unexpected Set type"
typeToSMT TSpec{} = panic "Unexpected Spec type"
typeToSMT TFree{} = panic "Unexpected free type variable"
typeToSMT TGen{}  = panic "Unexpected bound type variable"


-- | Translate an expression to an SMT expression.
exprToSMT :: HasCallStack => Expr -> SMT SMT.SExpr
exprToSMT ETrue      = return (SMT.bool True)
exprToSMT EFalse     = return (SMT.bool False)
exprToSMT (EVar _ n) = return (nameToVar n)

exprToSMT (ECon _ n) = return (nameToVar n)

exprToSMT (ENum n) = return (SMT.int n)

exprToSMT (ELet n _ e b) =
  do e' <- exprToSMT e
     b'  <- exprToSMT b
     return (letE [(n,e')] b')

exprToSMT (EAnd x y) =
  do x' <- exprToSMT x
     y' <- exprToSMT y
     return (SMT.and x' y')

exprToSMT (EOr x y) =
  do x' <- exprToSMT x
     y' <- exprToSMT y
     return (SMT.or x' y')

exprToSMT (EEq _ x y) =
  do x' <- exprToSMT x
     y' <- exprToSMT y
     return (SMT.eq x' y')

exprToSMT (ENot a) =
  do a' <- exprToSMT a
     return (SMT.not a')

exprToSMT (ENext _ (EVar _ n)) =
     return (SMT.Atom (smtName n ++ "_next"))

exprToSMT ETApp{} = panic "Unexpected ETApp"
exprToSMT ESet{}  = panic "Unexpected ESet"
exprToSMT EPrim{} = panic "Unexpected EPrim"
exprToSMT e@EApp{}= panic ("Unexpected EApp: " ++ show e)


-- | Introduce a locally let-bound variable.
letE :: [(Name,SMT.SExpr)] -> SMT.SExpr -> SMT.SExpr
letE []    e = e
letE binds e = SMT.fun "let" [ SMT.List (map mkBind binds), e ]
  where
  mkBind (n,b) = SMT.List [nameToVar n, b]


-- | Declare an enumeration as a datatype in Z3.
declareEnum :: EnumDef -> SMT ()
declareEnum EnumDef { .. } = withSolver $ \ s ->
  SMT.ackCommand s $ SMT.fun "declare-datatypes"
    [ SMT.List []
    , SMT.List [SMT.List (nameToVar eName : map nameToVar eCons)] ]


-- | Declare a state variable, including any bounding constraints.
declareStateVar :: StateVar -> SMT ()
declareStateVar StateVar { .. } =
  do _ <- declare  name
     _ <- declare (name ++ "_next")
     return ()

  where 
  name = smtName svName
  ty   = typeToSMT svType

  bounds =
    case svBounds of
      Just (lo,hi) ->
        let lo' = SMT.int (toInteger lo)
            hi' = SMT.int (toInteger hi)
         in \ n ->
              assert (SMT.and (SMT.geq (SMT.Atom n) lo')
                              (SMT.leq (SMT.Atom n) hi'))

      Nothing -> \ _ -> return ()

  declare n =
    do _ <- withSolver (\s -> SMT.declare s n ty)
       bounds n
