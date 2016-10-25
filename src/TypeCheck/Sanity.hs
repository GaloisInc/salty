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
module TypeCheck.Sanity (
    SanityMessage(..), isSanityError, sanityErrors, ppSanityMessage,
    sanityCheck,
  ) where

import           PP
import           Panic
import           Scope.Name (Name,nameText,nameUnique)
import           TypeCheck.AST

import           Control.Exception (bracket)
import           Control.Monad (guard,unless)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as L
import           MonadLib (WriterT, runWriterT, Lift, runLift)
import qualified SimpleSMT as SMT


data SanityMessage

ppSanityMessage :: SanityMessage -> Doc
ppSanityMessage  = undefined

isSanityError :: SanityMessage -> Bool
isSanityError _ = False

sanityErrors :: [SanityMessage] -> [SanityMessage]
sanityErrors  = filter isSanityError


-- | Run a sanity checking pass over a specification.
-- NOTE: this requires that the controller has been passed through the expand
-- pass, as it's unable to deal with macros.
sanityCheck :: Bool -> FilePath -> Controller -> IO [SanityMessage]
sanityCheck debug z3 cont = withSolver debug z3 (`checkController` cont)

withSolver :: Bool -> FilePath -> (SMT.Solver -> IO r) -> IO r
withSolver debug z3 k =
  do mb <- debugLogger debug
     bracket (SMT.newSolver z3 ["-smt2", "-in"] mb) SMT.stop $ \s ->
       do SMT.setOption s ":produce-models" "true"
          SMT.setOption s ":produce-unsat-cores" "true"
          k s

debugLogger :: Bool -> IO (Maybe SMT.Logger)
debugLogger False = return Nothing
debugLogger True  =
  do l <- SMT.newLogger 0
     return (Just l)

withScope :: SMT.Solver -> IO a -> IO a
withScope s m = bracket (SMT.push s) (\_ -> SMT.pop s) (\_ -> m)

checkController :: SMT.Solver -> Controller -> IO [SanityMessage]
checkController s cont@Controller { .. } = withScope s $
  do mapM_ (SMT.ackCommand s . declareEnum) cEnums

     mapM_ (declareStateVar s) cInputs
     mapM_ (declareStateVar s) cOutputs

     -- if the 
     let (es,as) = translate (traverse (exprToSMT . thing) (sSysTrans cSpec))
     withScope s $
       do _   <- assertExprs s "sys_trans" es
          res <- SMT.check s
          unless (res == SMT.Sat) $
            do m <- getUnsatCore s
               print m

     return []

assertExprs :: SMT.Solver -> String -> [SMT.SExpr] -> IO [SMT.SExpr]
assertExprs s p = go [] 0
  where
  go names i []     = return (reverse names)
  go names i (x:xs) =
    do let name = SMT.Atom (p ++ "_" ++ show i)
       SMT.assert s (SMT.fun "!" [x,SMT.Atom ":named",name])
       go (name:names) (i+1) xs


type Model = Map.Map Name SMT.Value

getModel :: SMT.Solver -> Controller -> IO (Map.Map Name SMT.Value)
getModel s Controller { .. } =
  do xs <- SMT.getExprs s (map fst (inputs ++ outputs))
     return (Map.fromList (resolve xs inputs ++ resolve xs outputs))
  where
  inputs  = [ (nameToVar svName, svName) | StateVar { .. } <- cInputs  ]
  outputs = [ (nameToVar svName, svName) | StateVar { .. } <- cOutputs ]

  resolve xs vars = [ (name, val) | (key, name) <- vars
                                  , let Just val = lookup key xs ]


getUnsatCore :: SMT.Solver -> IO SMT.SExpr
getUnsatCore s =
  SMT.command s (SMT.List [SMT.Atom "get-unsat-core"])


-- Translation -----------------------------------------------------------------

type Translate = WriterT [SMT.SExpr] Lift

-- | Run a translation expression, and gather up its additional assertions.
translate :: Translate a -> (a,[SMT.SExpr])
translate  = runLift . runWriterT


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
exprToSMT :: HasCallStack => Expr -> Translate SMT.SExpr
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
declareEnum :: EnumDef -> SMT.SExpr
declareEnum EnumDef { .. } =
  SMT.fun "declare-datatypes"
    [ SMT.List []
    , SMT.List [SMT.List (nameToVar eName : map nameToVar eCons)] ]


-- | Declare a state variable, including any bounding constraints.
declareStateVar :: SMT.Solver -> StateVar -> IO ()
declareStateVar s StateVar { .. } =
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
              SMT.assert s (SMT.and (SMT.geq (SMT.Atom n) lo')
                                    (SMT.leq (SMT.Atom n) hi'))

      Nothing -> \ _ -> return ()

  declare n =
    do SMT.declare s n ty
       bounds n
