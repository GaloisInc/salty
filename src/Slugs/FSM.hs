{-# LANGUAGE RecordWildCards #-}

module Slugs.FSM (
    fromSlugs,
    FSM(..),
    Node(..),
  ) where

import           Scope.Name (Name,nameText)
import           Slugs.Env
import           TypeCheck.AST (Controller(..),StateVar(..),Expr,Type)

import           Data.Either (partitionEithers)
import           Data.List (find,break,group,nub)
import qualified Data.Map.Strict as Map
import qualified Language.Slugs as Slugs

import Debug.Trace

panic :: String -> a
panic msg = error ("PANIC: " ++ msg)

data FSM = FSM { fsmInputs  :: Map.Map Name Type
               , fsmOutputs :: Map.Map Name Type
               , fsmNodes   :: Map.Map Int Node
               } deriving (Show)

data Node = Node { nodeInputs  :: Map.Map Name Expr
                 , nodeOutputs :: Map.Map Name Expr
                 , nodeTrans   :: [Int]
                 } deriving (Show)

-- | Translate from the slugs state machine to one that is easier to generate
-- code for.
fromSlugs :: Env -> Controller -> Slugs.FSM -> FSM
fromSlugs env cont Slugs.FSM { .. } =
  FSM { fsmInputs  = Map.fromList [ (svName, svType) | StateVar { .. } <- inps ]
      , fsmOutputs = Map.fromList [ (svName, svType) | StateVar { .. } <- outs ]
      , fsmNodes   = Map.empty }

  where

  -- translate the state variables from slugs back to names from the original
  -- specification, and partition into inputs/outputs
  (inps,outs) = partitionEithers
              $ map (getStateVar env cont)
              $ nub
              $ map sanitizeVarStr fsmStateDescr


-- | Lookup the state var from the input controller. The result is Left when the
-- state var was an input, and Right when it was an output.
getStateVar :: Env -> Controller -> String -> Either StateVar StateVar
getStateVar env cont str =
  case find svDef (cInputs cont) of
    Just sv -> Left sv
    Nothing ->
      case find svDef (cOutputs cont) of
        Just sv -> Right sv
        Nothing -> panic ("getStateVar: var from slugs missing from spec: " ++ str)

  where

  n = lookupVarName str env

  svDef sv = svName sv == n


-- | Strip out information about bit-vector indexing.
sanitizeVarStr :: String -> String
sanitizeVarStr str = fst (break (== '@') str)
