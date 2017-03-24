{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeCheck.Groups (
    sccTopDecls
  ) where

import Syntax.AST
import TypeCheck.AST (Group(..))

import           Data.Graph (SCC(..))
import           Data.Graph.SCC (stronglyConnComp)
import qualified Data.Set as Set
import qualified Data.Map as Map


sccTopDecls :: Ord (NameOf a) => [TopDecl a] -> [Group (TopDecl a)]
sccTopDecls ds = map toGroup (stronglyConnComp graph)
  where

  toGroup (AcyclicSCC x) = NonRecursive x
  toGroup (CyclicSCC xs) = Recursive xs

  fvs d = [ keys Map.! k | k <- Set.toList (topDeclFvs d) ]
  keys  = Map.fromList [ (name,key) | (node,key,_) <- graph
                                    , name         <- Set.toList (topDeclDs node) ]

  graph  = [ (node, key, fvs node) | node <- ds | key <- [ 0 :: Int .. ] ]
