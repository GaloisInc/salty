{-# LANGUAGE RecordWildCards #-}

module Slugs.Translate where

import Scope.Name
import TypeCheck.AST

import           Control.Monad (guard)
import           Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import qualified Language.Slugs as Slugs


-- Translation -----------------------------------------------------------------

data Env = Env { envEnums   :: Map.Map Name EnumDef
               , envConstrs :: Map.Map Name Int
               , envVars    :: Map.Map Name Slugs.Var
               }

panic :: String -> a
panic str = error ("PANIC: " ++ str)

mkEnv :: Controller -> (Env,[Slugs.Var],[Slugs.Var])
mkEnv Controller { .. } = (env, map snd inps, map snd outs)
  where
  env = Env { envEnums   = Map.fromList [ (eName e, e) | e <- cEnums ]
            , envConstrs = Map.fromList constrs
            , envVars    = Map.fromList (inps ++ outs) }

  constrs = concat [ zip eCons [0 .. ] | EnumDef { .. } <- cEnums ]

  inps = [ (svName sv, mkVar env sv) | sv <- cInputs  ]
  outs = [ (svName sv, mkVar env sv) | sv <- cOutputs ]

mkVar :: Env -> StateVar -> Slugs.Var
mkVar env StateVar { .. } =
  case svType of

    TEnum n ->
      let EnumDef { .. } = lookupEnum n env
       in Slugs.VarNum (mangleName svName) 0 (length eCons - 1)

    TInt ->
      case svBounds of
        Just (lo,hi) -> Slugs.VarNum (mangleName svName) lo hi
        Nothing      -> panic "mkDecl: Missing bounds for Int-typed state var"

    TBool ->
      Slugs.VarBool (mangleName svName)

    TFree{} -> panic "mkVar: Free variable"
    TFun{}  -> panic "mkVar: function-typed state variable"


lookupEnum :: Name -> Env -> EnumDef
lookupEnum n Env { .. } = Map.findWithDefault missing n envEnums
  where
  missing = panic ("lookupEnum: Enum missing from environment: " ++ show n)

lookupVar :: Name -> Env -> Slugs.Var
lookupVar n Env { .. } = Map.findWithDefault missing n envVars
  where
  missing = panic ("lookupVar: Var missing from environment: " ++ show n)

lookupConstr :: Name -> Env -> Int
lookupConstr n Env { .. } = Map.findWithDefault missing n envConstrs
  where
  missing = panic ("lookupConstr: Constr missing from environment: " ++ show n)

constrNum :: Name -> EnumDef -> Int
constrNum n EnumDef { .. } = fromMaybe missing (elemIndex n eCons)
  where
  missing = panic ("constrNum: Missing constructor: " ++ show n)


translateController :: Controller -> Slugs.Spec
translateController cont =
  Slugs.Spec { Slugs.specEnv    = mkState env (cInputs cont)  (cEnvTrans cont) (cEnvLiveness cont)
             , Slugs.specSys    = mkState env (cOutputs cont) (cSysTrans cont) (cSysLiveness cont)
             , .. }
  where
  (env,specInput,specOutput) = mkEnv cont

mkState :: Env -> [StateVar] -> Expr -> Expr -> Slugs.State
mkState env vars trans liveness =
  Slugs.State { Slugs.stInit =
                do guard (not (null vars))
                   return (foldl1 Slugs.EAnd inits)

              , Slugs.stTrans =
                do guard (trans /= ETrue)
                   return (mkExpr env trans)

              , Slugs.stLiveness =
                do guard (liveness /= ETrue)
                   return (mkExpr env liveness)
              }

  where
  inits = [ mkInit env sv e | sv@StateVar { svInit = Just e, .. } <- vars ]

mkInit :: Env -> StateVar -> Expr -> Slugs.Expr
mkInit env StateVar { .. } e =
  case (svType, e) of

    (TEnum n, ECon c) ->
      let enum = lookupEnum n env
       in Slugs.assignConst (lookupVar svName env) (constrNum c enum)

    (TInt, ENum n) ->
      case svBounds of
        Nothing      -> panic "mkDecl: Missing bounds for Int-typed state var"
        Just (lo,hi) -> Slugs.assignConst (lookupVar svName env) n

    (TBool, ETrue) ->
      Slugs.EVar (lookupVar svName env)

    (TBool, EFalse) ->
      Slugs.ENeg (Slugs.EVar (lookupVar svName env))

    _ -> panic ("mkDecl: " ++ show e ++ " : " ++ show svType)

mkExpr :: Env -> Expr -> Slugs.Expr

mkExpr _   ETrue      = Slugs.ETrue
mkExpr _   EFalse     = Slugs.EFalse
mkExpr env (EVar v)   = Slugs.EVar (lookupVar v env)
mkExpr env (EEq l r)  = mkAssign env l r
mkExpr env (ENot a)   = Slugs.ENeg (mkExpr env a)
mkExpr env (EAnd a b) = Slugs.EAnd (mkExpr env a) (mkExpr env b)
mkExpr env (EOr  a b) = Slugs.EOr  (mkExpr env a) (mkExpr env b)

mkAssign :: Env -> Expr -> Expr -> Slugs.Expr

mkAssign env (EVar n) (ECon c) =
  Slugs.assignConst (lookupVar n env) (lookupConstr c env)

mkAssign env (ECon c) (EVar n) =
  Slugs.assignConst (lookupVar n env) (lookupConstr c env)

mkAssign env (EVar n) ETrue  = Slugs.EVar (lookupVar n env)
mkAssign env (EVar n) EFalse = Slugs.EVar (lookupVar n env)

mkAssign env ETrue  (EVar n) = Slugs.EVar (lookupVar n env)
mkAssign env EFalse (EVar n) = Slugs.EVar (lookupVar n env)

mangleName :: Name -> String
mangleName n = T.unpack (nameText n) ++ "_" ++ show (nameUnique n)
