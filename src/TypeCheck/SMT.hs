{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeCheck.SMT (

    -- * SMT Monad
    SMT(), runSMT,
    io,
    addMessage,
    withScope,
    withSolver,
    checkSat,
    getUnsatCore,
    assert,
    assertNamed,

    -- * Translation
    smtName, nameToVar,
    typeToSMT,
    exprToSMT,
    letE,
    declareEnum,
    declareStateVar,

    -- * Re-exported
    SMT.SExpr(..),
    SMT.Solver(),
    SMT.Result(..),
    SMT.implies,
  ) where

import           Panic (HasCallStack,panic)
import           Scope.Name (Name,nameText,nameUnique)
import           TypeCheck.AST


import           Control.Exception (bracket,catch)
import qualified Data.Text as T
import           MonadLib (StateT,runStateT,get,set,BaseM,inBase)
import qualified SimpleSMT as SMT


newtype SMT msg a = SMT (StateT (RW msg) IO a)
                    deriving (Functor,Applicative,Monad)

data RW msg = RW { rwSolver   :: SMT.Solver
                 , rwMessages :: [msg]
                 }


instance BaseM (SMT msg) IO where
  inBase m = SMT (inBase m)
  {-# INLINE inBase #-}

io :: IO a -> SMT msg a
io m = SMT (inBase m)

addMessage :: msg -> SMT msg ()
addMessage msg = SMT $
  do RW { .. } <- get
     set $! RW { rwMessages = rwMessages ++ [msg], .. }

runSMT :: Bool -> FilePath -> SMT msg a -> IO (Maybe (a,[msg]))
runSMT debug z3 (SMT m) =
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

  handler :: IOError -> IO (Maybe (a, RW msg))
  handler _ = return Nothing

debugLogger :: Bool -> IO (Maybe SMT.Logger)
debugLogger False = return Nothing
debugLogger True  =
  do l <- SMT.newLogger 0
     return (Just l)

withScope :: SMT msg a -> SMT msg a
withScope (SMT m) = SMT $
  do rw      <- get
     (a,rw') <- inBase (bracket (SMT.push (rwSolver rw))
                                (\_ -> SMT.pop (rwSolver rw))
                                (\_ -> runStateT rw m))
     set $! rw'
     return a

withSolver :: (SMT.Solver -> IO a) -> SMT msg a
withSolver k = SMT $
  do RW { .. } <- get
     inBase (k rwSolver)

checkSat :: SMT msg SMT.Result
checkSat  = withSolver SMT.check

assert :: SMT.SExpr -> SMT msg ()
assert e = withSolver (`SMT.assert` e)

assertNamed :: String -> [SMT.SExpr] -> SMT msg [String]
assertNamed p es = withSolver (\s -> go s [] 0 es)
  where
  go _ names _ []     = return (reverse names)
  go s names i (x:xs) =
    do let name = p ++ "_" ++ show (i :: Int)
       SMT.assert s (SMT.fun "!" [x,SMT.Atom ":named",SMT.Atom name])
       go s (name:names) (i+1) xs

getUnsatCore :: SMT msg [String]
getUnsatCore  = withSolver $ \ s ->
  do res <- SMT.command s (SMT.List [SMT.Atom "get-unsat-core"])
     case res of
       SMT.List es -> return [ e | SMT.Atom e <- es ]
       _           -> panic ("Unexpected result from (get-unsat-core):\n" ++ show res)

-- SMT Translation -------------------------------------------------------------

-- | Translate a name to a unique name in the SMT embedding.
smtName :: Name -> String
smtName n = T.unpack (nameText n) ++ "_" ++ show (nameUnique n)


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
exprToSMT :: HasCallStack => Expr -> SMT msg SMT.SExpr
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

exprToSMT (EXor x y) =
  do x' <- exprToSMT x
     y' <- exprToSMT y
     return (SMT.xor x' y')

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
declareEnum :: EnumDef -> SMT msg ()
declareEnum EnumDef { .. } = withSolver $ \ s ->
  SMT.ackCommand s $ SMT.fun "declare-datatypes"
    [ SMT.List []
    , SMT.List [SMT.List (nameToVar eName : map nameToVar eCons)] ]


-- | Declare a state variable, including any bounding constraints.
declareStateVar :: StateVar -> SMT msg ()
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
