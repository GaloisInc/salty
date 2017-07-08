{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Slugs.Translate ( mkSpec ) where

import Panic (panic,HasCallStack)
import Slugs.Env
import TypeCheck.AST

import           Data.List (foldl1')
import qualified Language.Slugs as Slugs


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
    Slugs.Spec { Slugs.specEnv = mkState env (cInputs  cont) envInit envTrans sEnvLiveness
               , Slugs.specSys = mkState env (cOutputs cont) sysInit sysTrans sSysLiveness
               , .. }
  (env,specInput,specOutput) = mkEnv cont

mkState :: Env -> [StateVar] -> [Expr] -> [Expr] -> [Liveness] -> Slugs.State
mkState env vars is trans liveness =
  Slugs.State { Slugs.stInit     = conj inits
              , Slugs.stTrans    = mkExpr env (eAnd trans)
              , Slugs.stLiveness = map mkLiveness liveness }

  where

  mkLiveness (Liveness xs) = mkExpr env (eAnd (map snd xs))

  conj []  = Slugs.ETrue
  conj [e] = e
  conj es  = foldl1 Slugs.EAnd es

  inits    = map (mkExpr env) is



mkInit :: Env -> StateVar -> Expr -> Slugs.Expr
mkInit env StateVar { .. } e = mkExpr env (EEq svType (EVar svType svName) e)

-- | Translate a boolean-valued expression.
mkExpr :: HasCallStack => Env -> Expr -> Slugs.Expr
mkExpr _   ETrue            = Slugs.ETrue
mkExpr _   EFalse           = Slugs.EFalse
mkExpr env (EEq _ l r)      = mkAssign env l r
mkExpr env (ENot a)         = Slugs.ENeg (mkExpr env a)
mkExpr env (EAnd a b)       = Slugs.EAnd (mkExpr env a) (mkExpr env b)
mkExpr env (EOr  a b)       = Slugs.EOr  (mkExpr env a) (mkExpr env b)
mkExpr env (EXor a b)       = Slugs.EXor (mkExpr env a) (mkExpr env b)

mkExpr env (EVar _ v)           = mkVar Slugs.UVar  (lookupVarExpr v env)
mkExpr env (ENext _ (EVar _ v)) = mkVar Slugs.UNext (lookupVarExpr v env)

mkExpr env e@ELet{} =
  let (binds,r)  = destELet e
      (ns,_,bs)  = unzip3 binds
      env'       = foldr (uncurry addRef) env (zip ns [0..])
   in Slugs.EBuf (map (mkExpr env') (bs ++ [r]))

mkExpr _   (ECon _ _)       = panic "Constructor used outside of assignment"
mkExpr _   e@EApp{}         = panic ("Unexpected EApp: " ++ show e)
mkExpr _   e@ETApp{}        = panic ("Unexpected ETApp: " ++ show e)
mkExpr _   (ENum _)         = panic "Unexpected ENum"
mkExpr _   (ENext _ _)      = panic "Unexpected ENext"
mkExpr _   (EPrim _)        = panic "Unexpected EPrim"
mkExpr _   (ESet _ _)       = panic "Unexpected ESet"


slugsVar :: Env -> Expr -> Maybe Slugs.Expr
slugsVar env (EVar _ v)           = Just (mkVar Slugs.UVar  (lookupVarExpr v env))
slugsVar env (ENext _ (EVar _ v)) = Just (mkVar Slugs.UNext (lookupVarExpr v env))
slugsVar _   _                    = Nothing


mkVar :: (Slugs.Var -> Slugs.Use) -> Either Int Slugs.Var -> Slugs.Expr

mkVar _ (Left ref) =
  Slugs.ERef ref

mkVar mk (Right var@Slugs.VarBool{}) =
  Slugs.EVar (mk var)

mkVar mk (Right var@Slugs.VarNum{})  =
  let use = mk var
   in mkEAnd [ Slugs.EBit use i | i <- [0 .. Slugs.varBitSize var - 1] ]


-- | Translate an expression that's expected to be a variable (or a use of Next
-- with a variable)
slugsUse :: HasCallStack => Env -> Expr -> Maybe (Either Int Slugs.Use)
slugsUse env (EVar _ v)           = Just $!
  case lookupVarExpr v env of
    Left ref  -> Left ref
    Right var -> Right (Slugs.UVar var)
slugsUse env (ENext _ (EVar _ v)) = Just (Right (Slugs.UNext (lookupVar v env)))
slugsUse _   _                    = Nothing



-- | Translate a use of equality.
mkAssign :: HasCallStack => Env -> Expr -> Expr -> Slugs.Expr

-- constant enum values
mkAssign env (slugsUse env -> Just use) (ECon _ c) =
  case use of
    Left _ref -> panic "Reference in assign"
    Right var -> Slugs.assignConst var (lookupConstr c env)

mkAssign env (ECon _ c) (slugsUse env -> Just use) =
  case use of
    Left _ref -> panic "Reference in assign"
    Right var -> Slugs.assignConst var (lookupConstr c env)

-- constant booleans
mkAssign env (slugsVar env -> Just var) ETrue  =            var
mkAssign env (slugsVar env -> Just var) EFalse = Slugs.ENeg var
mkAssign env ETrue  (slugsVar env -> Just var) =            var
mkAssign env EFalse (slugsVar env -> Just var) = Slugs.ENeg var

-- constant numbers
mkAssign env (slugsUse env -> Just sa) (ENum a) = mkConstAssign sa a
mkAssign env (ENum a) (slugsUse env -> Just sa) = mkConstAssign sa a

-- negation of variables, just fall back on equality
mkAssign env (ENot (slugsVar env -> Just a)) (slugsVar env -> Just b) =
  mkEq (Slugs.ENeg a) b

mkAssign env (slugsVar env -> Just a) (ENot (slugsVar env -> Just b)) =
  mkEq a (Slugs.ENeg b)

mkAssign env (ENot (slugsVar env -> Just a)) (ENot (slugsVar env -> Just b)) =
  mkEq a b

-- variable assignment
mkAssign env (slugsUse env -> Just sa) (slugsUse env -> Just sb) =
  case (sa,sb) of


    (Right va, Right vb)
      | Slugs.VarNum _ _ hia <- Slugs.toVar va,
        Slugs.VarNum _ _ hib <- Slugs.toVar vb ->
        let numBits = Slugs.numBits (min hia hib)
         in mkEAnd [ mkEq (mkEBit va i) (mkEBit vb i) | i <- [0 .. numBits - 1] ]

    _ ->
      mkEq (toExpr sa) (toExpr sb)

  where
  toExpr (Left ref) = Slugs.ERef ref
  toExpr (Right u)  = Slugs.EVar u

mkAssign _ a b = panic $ unlines [ "Invalid arguments to assign: "
                                 , " * " ++ show a
                                 , " * " ++ show b
                                 ]


mkConstAssign :: HasCallStack => Either Int Slugs.Use -> Integer -> Slugs.Expr
mkConstAssign (Right u) n = Slugs.assignConst u n
mkConstAssign Left{}    _ = panic "Unexpected reference in numeric assignment"

mkEq :: Slugs.Expr -> Slugs.Expr -> Slugs.Expr
mkEq a b = Slugs.EOr (Slugs.EAnd a b) (Slugs.EAnd (Slugs.ENeg a) (Slugs.ENeg b))

mkEBit :: Slugs.HasVar var => var -> Int -> Slugs.Expr
mkEBit var i = Slugs.EBit (Slugs.toUse var) i


mkEAnd :: [Slugs.Expr] -> Slugs.Expr
mkEAnd [] = Slugs.ETrue
mkEAnd es = foldl1' step es
  where
  step acc (Slugs.EAnd a b) = Slugs.EAnd a (Slugs.EAnd b acc)
  step acc e                = Slugs.EAnd e acc
