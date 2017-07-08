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
import           SrcLoc
import           TypeCheck.AST
import           TypeCheck.SMT

import           Control.Monad (when)
import           Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import           Data.Traversable (forM)


type Sanity = SMT SanityMessage

data SanityMessage = CouldntFindZ3
                     -- ^ Failed to start Z3, indicates that sanity checking was
                     -- not done.

                   | InitConflict [Origin]
                     -- ^ Variable initialization is not consistent

                   | EnvLivenessSafety [Origin]
                     -- ^ Environment liveness constraint will eventually cause
                     -- environmental safety constraints to be violated.

                   | SysLivenessSafety Bool [Origin]
                     -- ^ A system liveness constraint will cause the system to
                     -- eventually violate safety.

                   | UnsatEnv [Origin]
                     -- ^ The environment is never satisfiable. This will yield
                     -- a realizable, but useless controller, that will work at
                     -- the first step, and behave randomly for the rest of
                     -- time.

                   | UnsatSys [Origin]
                     -- ^ The system safety properties are never satisfiable.
                     deriving (Show)

ppSanityMessage :: SanityMessage -> Doc
ppSanityMessage CouldntFindZ3 = ppMessage "[error]" $
  text "Unable to find z3 (try passing the location with --z3)"

ppSanityMessage (InitConflict ms) = ppMessage "[error]" $
  hang (text "Initialization constraints violate safety:")
     2 (bullets (map ppOrigin ms))

ppSanityMessage (EnvLivenessSafety ms) = ppMessage "[warning]" $
  hang (text "Environment will eventually violate safety:")
     2 (bullets (map ppOrigin ms))

ppSanityMessage (SysLivenessSafety err ms) = ppMessage ty $
  hang (text "System will eventually violate safety:")
     2 (bullets (map ppOrigin ms))
  where
  ty | err       = "[error]"
     | otherwise = "[warning]"

ppSanityMessage (UnsatEnv ms) = ppMessage "[error]" $
  hang (text "Environment safety constraints are never satisfiable:")
     2 (bullets (map ppOrigin ms))

ppSanityMessage (UnsatSys ms) = ppMessage "[error]" $
  hang (text "System safety constraints are never satisfiable")
     2 (bullets (map ppOrigin ms))



isSanityError :: SanityMessage -> Bool
isSanityError CouldntFindZ3             = True
isSanityError InitConflict{}            = True
isSanityError (SysLivenessSafety err _) = err
isSanityError UnsatEnv{}                = True
isSanityError UnsatSys{}                = True
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
checkController :: Controller -> Sanity ()
checkController cont@Controller { .. } = withScope $
  do mapM_ declareEnum cEnums

     mapM_ declareStateVar cInputs
     mapM_ declareStateVar cOutputs

     -- check safety constraints in all states
     safe <- checkSafety cont

     -- only continue checking if safety constraints are not violated, as
     -- everything else will fail without them.
     when safe $ withScope $
       do safety <- assertSafety cont
          _      <- checkInit cont safety
          err    <- checkEnvLiveness cont safety
          checkSysLiveness err cont safety

data Origin = SysTrans SrcLoc
            | EnvTrans SrcLoc
            | InputVar SrcLoc
            | OutputVar SrcLoc
            | EnvLiveness SrcLoc
            | SysLiveness SrcLoc
              deriving (Eq,Show)

instance HasSrcLoc Origin where
  srcLoc (SysTrans loc)     = loc
  srcLoc (EnvTrans loc)     = loc
  srcLoc (InputVar loc)     = loc
  srcLoc (OutputVar loc)    = loc
  srcLoc (EnvLiveness loc)  = loc
  srcLoc (SysLiveness loc)  = loc

ppOrigin :: Origin -> Doc
ppOrigin origin =
  case origin of
    SysTrans l    -> text "system safety constraint at"              <+> pp l
    EnvTrans l    -> text "environment safety constraint at"         <+> pp l
    InputVar l    -> text "environment initialization constraint at" <+> pp l
    OutputVar l   -> text "system initialization constraint at"      <+> pp l
    EnvLiveness l -> text "environment liveness constraint at"       <+> pp l
    SysLiveness l -> text "system liveness constraint at"            <+> pp l


type LocMap = Map.Map String Origin


-- | Check for satisfiability, and generate a message using the unsat core if
-- not satisfiable.
check :: ([String] -> SanityMessage) -> Sanity Bool
check mkMessage =
  do res <- checkSat
     if res /= Sat
        then do ms <- getUnsatCore
                addMessage (mkMessage ms)
                return False

        else return True


-- Safety ----------------------------------------------------------------------

checkSafety :: Controller -> Sanity Bool
checkSafety cont =
  do -- check if the environmental safety constraints are satisfiable at all
     envSafe <- withScope (checkEnvSafety cont)

     -- check that the environment safety constraints imply the system safety
     -- constraints
     sysSafe <- withScope (checkSysSafety envSafe cont)

     return (envSafe && sysSafe)

-- | Check that the environmental safety constraints are satisfiable by
-- themselves.
checkEnvSafety :: Controller -> Sanity Bool
checkEnvSafety Controller { .. } =
  do env <- assertExprs EnvTrans "env_trans" Nothing (sEnvTrans cSpec)
     check (\ ms -> UnsatEnv [ env Map.! clause | clause <- ms ])

-- | Check that the environmental safety constraints imply the system safety
-- constraints.
checkSysSafety :: Bool -> Controller -> Sanity Bool
checkSysSafety envSafe Controller { .. } =
  do let cxt = case sEnvTrans cSpec of
                 es | envSafe && not (null es) -> Just (eAnd (map snd es))
                 _                             -> Nothing
     sys <- assertExprs SysTrans "sys_trans" cxt (sSysTrans cSpec)

     check (\ms -> UnsatSys [ sys Map.! clause | clause <- ms ])


-- | Assert the safety properties of a controller.
assertSafety :: Controller -> Sanity LocMap
assertSafety Controller { .. } =
  do env <- assertExprs EnvTrans "env_trans" Nothing (sEnvTrans cSpec)
     sys <- assertExprs SysTrans "sys_trans" Nothing (sSysTrans cSpec)
     return (env `mappend` sys)


-- | Assert all safety constraints from the controller.
assertExprs :: (SrcLoc -> Origin) -> String -> Maybe Expr -> [(SrcLoc,Expr)] -> Sanity LocMap
assertExprs origin pfx mbCxt locs =
  do mbCxt' <- traverse exprToSMT mbCxt
     let addCxt = case mbCxt' of
                    Just cxt -> implies cxt
                    Nothing  -> id
     es     <- traverse (exprToSMT . snd) locs
     ens    <- assertNamed pfx [ addCxt e | e <- es ]
     return $ Map.fromList [ (n, origin loc) | n <- ens | (loc,_) <- locs ]


-- Initial State ---------------------------------------------------------------

-- | Check that the initial state satisfies all safety constraints.
checkInit :: Controller -> LocMap -> Sanity Bool
checkInit cont locs = withScope $
  do locs' <- assertInit cont
     check $ \ ms ->
       let env = locs `mappend` locs'
        in InitConflict [ env Map.! clause | clause <- ms ]

-- | Assert all variable initialization formulas.
assertInit :: Controller -> Sanity LocMap
assertInit Controller { cSpec = Spec { .. } } =
  do si <- assertInitExpr InputVar  "env_init" sSysInit
     ti <- assertInitExpr OutputVar "sys_init" sEnvInit
     return (Map.union si ti)


assertInitExpr :: (SrcLoc -> Origin) -> String -> [(SrcLoc,Expr)] -> Sanity LocMap
assertInitExpr origin pfx inits =
  do let (locs,es) = unzip inits
     ns  <- assertNamed pfx =<< traverse exprToSMT es
     return (Map.fromList (zip ns (map origin locs)))


-- Liveness --------------------------------------------------------------------

-- | Check for system liveness constraints that will eventually violate safety.
checkSysLiveness :: Bool -> Controller -> LocMap -> Sanity ()
checkSysLiveness envWarns cont locs =
  do conflicts <- mapM (checkLiveness locs) (sSysLiveness (cSpec cont))
     forM_ (concat conflicts) $ \ (loc, _, safety) ->
       addMessage (SysLivenessSafety (not envWarns) (SysLiveness loc : safety))

-- | Check for environmental liveness constraints that will eventually violate
-- environmental safety constraints.
checkEnvLiveness :: Controller -> LocMap -> Sanity Bool
checkEnvLiveness cont locs =
  do conflicts <- mapM (checkLiveness locs) (sEnvLiveness (cSpec cont))
     forM_ (concat conflicts) $ \ (loc, _, safety) ->
       addMessage (EnvLivenessSafety (EnvLiveness loc : safety))

     return (not (null conflicts))

-- | Assert liveness constraints one at a time, recording when they violate the
-- safety constraints of the specification.
checkLiveness :: LocMap -> Liveness -> Sanity [(SrcLoc, SExpr, [Origin])]
checkLiveness safety (Liveness es) = withScope $
  do rs <- forM es $ \ (loc,e) -> withScope $
        do e' <- exprToSMT e
           assert e'
           res <- checkSat
           if res /= Sat
              then
                do ms <- getUnsatCore
                   return [ (loc, e', [ safety Map.! m | m <- ms ]) ]
              else return []

     return (concat rs)
