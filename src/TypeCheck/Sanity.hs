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
module TypeCheck.Sanity (
    SanityMessage(..), isSanityError, sanityErrors, ppSanityMessage,
    sanityCheck,
  ) where

import           PP
import           TypeCheck.AST

import qualified SimpleSMT as SMT


data SanityMessage

ppSanityMessage :: SanityMessage -> Doc
ppSanityMessage  = undefined

isSanityError :: SanityMessage -> Bool
isSanityError _ = False

sanityErrors :: [SanityMessage] -> [SanityMessage]
sanityErrors  = filter isSanityError


-- | Run a sanity checking pass over a specification.
sanityCheck :: Bool -> FilePath -> Controller -> IO ([SanityMessage])
sanityCheck debug z3 spec =
  do return []
