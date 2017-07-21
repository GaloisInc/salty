{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}

module Slugs.Translate ( mkSpec ) where

import Panic (panic,HasCallStack)
import Scope.Name
import Slugs.Env
import TypeCheck.AST

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Language.Slugs as Slugs
import           MonadLib (StateT,get,sets,sets_,Lift,runM)


-- Translation -----------------------------------------------------------------

mkSpec :: Controller -> (Slugs.Spec,Env)
mkSpec cont = (Slugs.addLimits slugs,env)
  where
  Spec { .. } = cSpec cont

  envInit     = map snd sEnvInit
  envTrans    = map snd sEnvTrans
  sysInit     = map snd sSysInit
  sysTrans    = map snd sSysTrans

  slugs =
    Slugs.Spec { Slugs.specEnv = mkState env envInit envTrans sEnvLiveness
               , Slugs.specSys = mkState env sysInit sysTrans sSysLiveness
               , .. }
  (env,specInput,specOutput) = mkEnv cont

mkState :: Env -> [Expr] -> [Expr] -> [Liveness] -> Slugs.State
mkState env is trans liveness =
  Slugs.State { Slugs.stInit     = cgExprs is
              , Slugs.stTrans    = cgExprs trans
              , Slugs.stLiveness = [ cgExprs (map snd es) | Liveness es <- liveness ] }

  where

  cgExprs es = runCG env $
    do ess <- traverse mkExpr es
       return (conj (concat ess))

-- | Conjunction for slugs.
conj :: [Slugs.Expr] -> Slugs.Expr
conj []  = Slugs.ETrue
conj [e] = e
conj es  = foldl1 Slugs.EAnd es


-- | Translate a boolean-valued expression.
mkExpr :: HasCallStack => Expr -> CG [Slugs.Expr]
mkExpr ETrue =
     return [Slugs.ETrue]

mkExpr EFalse =
     return [Slugs.EFalse]

mkExpr (EEq _ a b) =
  do xs <- slugsBinop (\x y -> Slugs.ENeg (Slugs.EXor x y)) Slugs.ENeg a b
     return [ conj xs ]

mkExpr (ENot a) =
  do as <- mkExpr a
     return [ conj [ Slugs.ENeg e | e <- as ] ]

mkExpr (EAnd a b) =
  do xs <- slugsBinop Slugs.EAnd id a b
     return [ conj xs ]

mkExpr (EOr a b) =
  do xs <- slugsBinop Slugs.EOr id a b
     return [ foldl1 Slugs.EOr xs ]

mkExpr (EXor a b) =
  do xs <- slugsBinop Slugs.EXor id a b
     return [ foldl1 Slugs.EXor xs ]

mkExpr (EVar _ v) =
  do env <- getEnv
     return (mkVar Slugs.UVar (lookupVarExpr v env))

mkExpr (ENext _ (EVar _ v)) =
  do env <- getEnv
     return (mkVar Slugs.UNext (lookupVarExpr v env))

mkExpr (ELet n _ b e) =
  do bs     <- mkExpr b
     (_,is) <- allocNames (length bs)
     bindName n is
     addDefs bs
     mkExpr e

mkExpr (ECon _ n) =
  do env <- getEnv
     let i = lookupConstr n env
     return (Slugs.atomBits (toInteger i))

mkExpr (ENum i) =
     return (Slugs.atomBits (toInteger i))

mkExpr (EPlus a b) =
  do as <- mkExpr a
     bs <- mkExpr b

     -- NOTE: `sumbits` and `sel` both refer to references in the memory buffer
     -- as though they start at 0. They need to be incremented to account for
     -- anything that's already buffered.
     let (sumbits,sel) = Slugs.add' as bs

     -- reserve enough space for sumbits
     (top,_) <- allocNames (length sumbits)
     let upd = Slugs.modifyRefs (+ top)
     addDefs (map upd sumbits)

     return (map upd sel)


mkExpr e@EApp{}  = panic ("Unexpected EApp: " ++ show e)
mkExpr e@ETApp{} = panic ("Unexpected ETApp: " ++ show e)
mkExpr ENext{}   = panic "Unexpected ENext"
mkExpr EPrim{}   = panic "Unexpected EPrim"
mkExpr ESet{}    = panic "Unexpected ESet"


slugsBinop :: (Slugs.Expr -> Slugs.Expr -> Slugs.Expr)
           -> (Slugs.Expr -> Slugs.Expr)
           -> Expr -> Expr -> CG [Slugs.Expr]
slugsBinop op overflow = \ l r ->
  do ls <- mkExpr l
     rs <- mkExpr r
     let (xs,ys) | length ls <= length rs = (ls,rs)
                 | otherwise              = (rs,ls)

         extra = drop (length xs) ys

     return $ [ x `op` y | x <- xs
                         | y <- ys ] ++
              [ overflow y | y <- extra ]


mkVar :: (Slugs.Var -> Slugs.Use) -> Either [Int] Slugs.Var -> [Slugs.Expr]

mkVar _ (Left refs) =
  map Slugs.ERef refs

mkVar mk (Right var@Slugs.VarBool{}) =
  Slugs.atomBits (mk var)

mkVar mk (Right var@Slugs.VarNum{})  =
  Slugs.atomBits (mk var)


-- Translation Monad -----------------------------------------------------------

newtype CG a = CG { unCG :: StateT RW Lift a
                  } deriving (Functor,Applicative,Monad)


data RW = RW { rwNextRef :: !Int
             , rwDefs    :: !(Seq.Seq Slugs.Expr)
             , rwEnv     :: !Env
             }


runCG :: Env -> CG Slugs.Expr -> Slugs.Expr
runCG env m =
  case runM (unCG m) RW { rwNextRef = 0, rwDefs = Seq.empty, rwEnv = env } of
    (final,rw) -> Slugs.EBuf (F.toList (rwDefs rw Seq.|> final))

bindName :: Name -> [Int] -> CG ()
bindName n is = bindNames [(n,is)]

bindNames :: [(Name,[Int])] -> CG ()
bindNames binds = CG $ sets_ $ \ rw ->
  rw { rwEnv = foldr (uncurry addRef) (rwEnv rw) binds }

-- | Allocate a block of names in the resulting memory buffer.
allocNames :: Int -> CG (Int,[Int])
allocNames n = CG $ sets $ \ rw ->
  let next = rwNextRef rw + n
   in ((rwNextRef rw, [ rwNextRef rw .. next - 1 ]), rw { rwNextRef = next })

addDefs :: [Slugs.Expr] -> CG ()
addDefs defs = CG $ sets_ $ \ rw ->
  rw { rwDefs = rwDefs rw Seq.>< Seq.fromList defs }

getEnv :: CG Env
getEnv  = CG (rwEnv `fmap` get)
