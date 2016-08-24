{-# LANGUAGE RecordWildCards #-}

module Slugs.Env (
    Env(), emptyEnv,
    mkEnv,
    lookupVar, lookupVarExpr, lookupVarName, lookupConstr,
    lowerBound, lowerBound',
    hasLowerBound,
    lookupEnum,
    addRef,
  ) where

import Panic (panic,HasCallStack)
import Scope.Name (Name,nameText,nameUnique)
import TypeCheck.AST (Controller(..),StateVar(..),Type(..),EnumDef(..))

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import qualified Language.Slugs as Slugs


data Env = Env { envEnums   :: Map.Map Name EnumDef
               , envConstrs :: Map.Map Name Int
               , envBounds  :: Map.Map Name Integer
               , envVars    :: Map.Map Name Slugs.Var
               , envVars'   :: Map.Map String Name
               , envRefs    :: Map.Map Name Int
               } deriving (Show)

emptyEnv :: Env
emptyEnv  = Env { envEnums   = Map.empty
                , envConstrs = Map.empty
                , envBounds  = Map.empty
                , envVars    = Map.empty
                , envVars'   = Map.empty
                , envRefs    = Map.empty }

-- | Produce an environment, input variables, and output variables, from a
-- controller.
mkEnv :: Controller -> (Env,[Slugs.Var],[Slugs.Var])
mkEnv Controller { .. } = (env, map snd inps, map snd outs)
  where
  env = Env { envEnums   = Map.fromList [ (eName e, e) | e <- cEnums ]
            , envConstrs = Map.fromList constrs
            , envBounds  = Map.fromList bounds
            , envVars    = Map.fromList (inps ++ outs)
            , envVars'   = Map.fromList (map swap (inps ++ outs))
            , envRefs    = Map.empty }

  swap (n,Slugs.VarBool v)    = (v,n)
  swap (n,Slugs.VarNum v _ _) = (v,n)

  constrs = concat [ zip eCons [0 .. ] | EnumDef { .. } <- cEnums ]

  inps = [ (svName, mkVar env svName svType svBounds) | StateVar {..} <- cInputs  ]
  outs = [ (svName, mkVar env svName svType svBounds) | StateVar {..} <- cOutputs ]

  bounds = [ (svName, toInteger lo) | StateVar { svBounds = Just (lo,_), .. } <- cInputs ++ cOutputs ]

mkVar :: HasCallStack => Env -> Name -> Type -> Maybe (Int,Int) -> Slugs.Var
mkVar env name ty mbBounds =
  case ty of

    TEnum n ->
      let EnumDef { .. } = lookupEnum n env
       in Slugs.VarNum (mangleName name) 0 (length eCons - 1)

    -- always adjust down to [0,hi-lo] to avoid extra bits
    TInt ->
      case mbBounds of
        Just (lo,hi) -> Slugs.VarNum (mangleName name) 0 (hi - lo)
        Nothing      -> panic "Missing bounds for Int-typed state var"

    TBool ->
      Slugs.VarBool (mangleName name)

    TFree{} -> panic "free variable"
    TFun{}  -> panic "function-typed state variable"
    TSet{}  -> panic "set-typed state variable"


addRef :: Name -> Int -> Env -> Env
addRef n ix Env {..} = Env { envRefs = Map.insert n ix envRefs, ..}


lookupEnum :: HasCallStack => Name -> Env -> EnumDef
lookupEnum n Env { .. } = Map.findWithDefault missing n envEnums
  where
  missing = panic ("Enum missing from environment: " ++ show n)

lookupVar :: HasCallStack => Name -> Env -> Slugs.Var
lookupVar n Env { .. } = Map.findWithDefault missing n envVars
  where
  missing = panic ("Var missing from environment: " ++ show n ++ "\n" ++ unlines (map show (Map.toList envVars)))

lookupVarExpr :: HasCallStack => Name -> Env -> Either Int Slugs.Var
lookupVarExpr n Env { .. } =
  case Map.lookup n envVars of
    Just var -> Right var
    Nothing  -> Left (Map.findWithDefault missing n envRefs)
  where
  missing = panic ("Var missing from environment: " ++ show n ++ "\n" ++ unlines (map show (Map.toList envVars)))

lowerBound :: Name -> Env -> Integer
lowerBound  = lowerBound' 0
{-# INLINE lowerBound #-}

lowerBound' :: Integer -> Name -> Env -> Integer
lowerBound' d n Env { .. } = Map.findWithDefault d n envBounds

hasLowerBound :: Name -> Env -> Maybe Integer
hasLowerBound n Env { .. } = Map.lookup n envBounds

-- | Take the mangled version of the variable name, and translate it back to the
-- original name.
lookupVarName :: HasCallStack => String -> Env -> Name
lookupVarName n Env { .. } = Map.findWithDefault missing n envVars'
  where
  missing = panic ("Var missing from environment: " ++ n)

lookupConstr :: HasCallStack => Name -> Env -> Int
lookupConstr n Env { .. } = Map.findWithDefault missing n envConstrs
  where
  missing = panic ("Constr missing from environment: " ++ show n)

mangleName :: Name -> String
mangleName n = T.unpack (nameText n) ++ "_" ++ show (nameUnique n)
