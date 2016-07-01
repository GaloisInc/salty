{-# LANGUAGE RecordWildCards #-}

module Slugs.FSM (
    fromSlugs,
    FSM(..),
    Node(..),
  ) where

import           Scope.Name (Name)
import           Slugs.Env
import           TypeCheck.AST (Expr,Type)

import qualified Data.Map.Strict as Map
import qualified Language.Slugs as Slugs


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
fromSlugs :: Env -> Slugs.FSM -> FSM
fromSlugs env Slugs.FSM { .. } =
  error (show env)
