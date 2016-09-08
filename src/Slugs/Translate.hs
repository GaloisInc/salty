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
  spec = cSpec cont

  slugs =
    Slugs.Spec { Slugs.specEnv = mkState env (cInputs cont)  (sEnvTrans spec) (sEnvLiveness spec)
               , Slugs.specSys = mkState env (cOutputs cont) (sSysTrans spec) (sSysLiveness spec)
               , .. }
  (env,specInput,specOutput) = mkEnv cont

mkState :: Env -> [StateVar] -> Expr -> Expr -> Slugs.State
mkState env vars trans liveness =
  Slugs.State { Slugs.stInit     = conj inits
              , Slugs.stTrans    = mkExpr env trans
              , Slugs.stLiveness = mkExpr env liveness }

  where

  conj []  = Slugs.ETrue
  conj [e] = e
  conj es  = foldl1 Slugs.EAnd es

  inits = [ mkInit env sv e | sv@StateVar { svInit = Just e, .. } <- vars ]

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

mkExpr env (EVar _ v)           = mkVar Slugs.UVar  (lookupVarExpr v env)
mkExpr env (ENext _ (EVar _ v)) = mkVar Slugs.UNext (lookupVarExpr v env)

mkExpr env e@ELet{} =
  let (binds,r)  = destELet e
      (ns,_,bs)  = unzip3 binds
      env'       = foldr (uncurry addRef) env (zip ns [0..])
   in Slugs.EBuf (map (mkExpr env') (bs ++ [r]))

mkExpr _   (ECon _ _)       = panic "Constructor used outside of assignment"
mkExpr _   e@EApp{}         = panic ("Unexpected EApp: " ++ show e)
mkExpr _   (ENum _)         = panic "Unexpected ENum"
mkExpr _   (ENext _ _)      = panic "Unexpected ENext"
mkExpr _   (EPrim _)        = panic "Unexpected EPrim"
mkExpr _   (ESet _ _)         = panic "Unexpected ESet"


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
slugsUse :: Env -> Expr -> Maybe Slugs.Use
slugsUse env (EVar _ v)           = Just (Slugs.UVar  (lookupVar v env))
slugsUse env (ENext _ (EVar _ v)) = Just (Slugs.UNext (lookupVar v env))
slugsUse _   _                    = Nothing



-- | Translate a use of equality.
mkAssign :: HasCallStack => Env -> Expr -> Expr -> Slugs.Expr

-- constant enum values
mkAssign env (slugsUse env -> Just use) (ECon _ c) =
  Slugs.assignConst use (lookupConstr c env)

mkAssign env (ECon _ c) (slugsUse env -> Just use) =
  Slugs.assignConst use (lookupConstr c env)

-- constant booleans
mkAssign env (slugsVar env -> Just var) ETrue  =            var
mkAssign env (slugsVar env -> Just var) EFalse = Slugs.ENeg var
mkAssign env ETrue  (slugsVar env -> Just var) =            var
mkAssign env EFalse (slugsVar env -> Just var) = Slugs.ENeg var

-- constant numbers
mkAssign env (EVar _ n) (ENum a) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)
mkAssign env (ENum a) (EVar _ n) = Slugs.assignConst (lookupVar n env) (a - lowerBound n env)

-- variable assignment
mkAssign env (slugsUse env -> Just sa) (slugsUse env -> Just sb) =
  case (Slugs.toVar sa,Slugs.toVar sb) of

    (Slugs.VarBool{}, Slugs.VarBool{}) ->
      mkEq (Slugs.EVar sa) (Slugs.EVar sb)

    (Slugs.VarNum _ _ hia, Slugs.VarNum _ _ hib) ->
      let numBits = Slugs.numBits (min hia hib)
       in mkEAnd [ mkEq (mkEBit sa i) (mkEBit sb i) | i <- [0 .. numBits - 1] ]

    _ -> panic "Incompatible variables in assignment"


mkAssign _ a b = panic ("Invalid arguments to assign: " ++ show (a,b))


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
