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
    SanityMessage(..), isSanityError, sanityErrors, ppSanityMessage,
    sanityCheck,
  ) where

import           Message
import           PP
import           Panic
import           Scope.Name (Name,nameText,nameUnique)
import           TypeCheck.AST

import           Control.Exception (bracket)
import           Control.Monad (guard,unless)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as L
import           MonadLib (StateT, runStateT, runStateT, get, set, inBase)
import qualified SimpleSMT as SMT


data SanityMessage = SafetyConflict [SrcRange]
                     -- ^ Safety formulas at these locations are in conflict
                     deriving (Show)

ppSanityMessage :: SanityMessage -> Doc
ppSanityMessage (SafetyConflict locs) = ppMessage "[error]" $
  hang (text "Safety constraints are in conflict:")
     2 (bullets (map pp locs))

isSanityError :: SanityMessage -> Bool
isSanityError SafetyConflict{} = True

sanityErrors :: [SanityMessage] -> [SanityMessage]
sanityErrors  = filter isSanityError


-- | Run a sanity checking pass over a specification.
-- NOTE: this requires that the controller has been passed through the expand
-- pass, as it's unable to deal with macros.
sanityCheck :: Bool -> FilePath -> Controller -> IO [SanityMessage]
sanityCheck debug z3 cont =
  do (_,msgs) <- runSMT debug z3 (checkController cont)
     return msgs

-- | Check that all of the safety constraints are satisfiable.
checkController :: Controller -> SMT ()
checkController cont@Controller { .. } = withScope $
  do mapM_ declareEnum cEnums

     mapM_ declareStateVar cInputs
     mapM_ declareStateVar cOutputs

     withScope $
       do interp <- assertSafety cont
          res    <- checkSat
          unless (res == SMT.Sat) $
            do ms <- getUnsatCore
               addMessage (SafetyConflict [ getLoc (interp Map.! clause) | clause <- ms ])



data Origin = SysTrans
            | EnvTrans
              deriving (Eq,Ord,Show)

-- | Assert all safety constraints from the controller.
assertSafety :: Controller -> SMT (Map.Map String (Loc Origin))
assertSafety Controller { .. } =
 do (es,as) <- collectContext (traverse (exprToSMT . thing) (sEnvTrans cSpec))
    (ss,as) <- collectContext (traverse (exprToSMT . thing) (sSysTrans cSpec))
    ens     <- assertNamed "env_trans" es
    sns     <- assertNamed "sys_trans" ss
    return $ Map.fromList
           $ [ (n, SysTrans <$ loc) | n <- sns | loc <- sSysTrans cSpec ]
          ++ [ (n, EnvTrans <$ loc) | n <- ens | loc <- sEnvTrans cSpec ]
                     


-- SMT Monad -------------------------------------------------------------------

type SMT = StateT RW IO

data RW = RW { rwSolver   :: !SMT.Solver
             , rwMessages :: [SanityMessage]
             , rwContext  :: [SMT.SExpr]
             }

addMessage :: SanityMessage -> SMT ()
addMessage msg =
  do RW { .. } <- get
     set $! RW { rwMessages = rwMessages ++ [msg], .. }

runSMT :: Bool -> FilePath -> SMT a -> IO (a,[SanityMessage])
runSMT debug z3 m =
  do mb     <- debugLogger debug
     (a,rw) <- bracket (SMT.newSolver z3 ["-smt2", "-in"] mb) SMT.stop $ \ s ->
                do SMT.setOption s ":produce-models" "true"
                   SMT.setOption s ":produce-unsat-cores" "true"
                   runStateT RW { rwSolver   = s
                                , rwMessages = []
                                , rwContext  = [] } m
     return (a, rwMessages rw)

debugLogger :: Bool -> IO (Maybe SMT.Logger)
debugLogger False = return Nothing
debugLogger True  =
  do l <- SMT.newLogger 0
     return (Just l)

collectContext :: SMT a -> SMT (a,[SMT.SExpr])
collectContext m =
  do rw  <- get
     set $! rw { rwContext = [] }
     a   <- m
     rw' <- get
     set $! rw' { rwContext = rwContext rw }
     return (a, rwContext rw')

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
  go s names i []     = return (reverse names)
  go s names i (x:xs) =
    do let name = p ++ "_" ++ show i
       SMT.assert s (SMT.fun "!" [x,SMT.Atom ":named",SMT.Atom name])
       go s (name:names) (i+1) xs


type Model = Map.Map Name SMT.Value

getModel :: Controller -> SMT (Map.Map Name SMT.Value)
getModel Controller { .. } = withSolver $ \s ->
  do xs <- SMT.getExprs s (map fst (inputs ++ outputs))
     return (Map.fromList (resolve xs inputs ++ resolve xs outputs))
  where
  inputs  = [ (nameToVar svName, svName) | StateVar { .. } <- cInputs  ]
  outputs = [ (nameToVar svName, svName) | StateVar { .. } <- cOutputs ]

  resolve xs vars = [ (name, val) | (key, name) <- vars
                                  , let Just val = lookup key xs ]


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
exprToSMT (EVar t n) = return (nameToVar n)

exprToSMT (ECon _ n) = return (nameToVar n)

exprToSMT (ENum n) = return (SMT.int n)

exprToSMT (ELet n ty e b) =
  do e' <- exprToSMT e
     b  <- exprToSMT b
     return (letE [(n,e')] b)

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

exprToSMT (ENext _ (EVar t n)) =
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
    do withSolver (\s -> SMT.declare s n ty)
       bounds n
