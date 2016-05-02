{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Infer where

import           Scope.Name (Name)
import qualified Syntax.AST as AST
import           TypeCheck.AST
import           TypeCheck.Groups
import           TypeCheck.Monad

import           Data.List (foldl')
import           Text.Location (thing,getLoc,at)


inferController :: AST.Controller Name -> TC Controller
inferController AST.Controller { AST.cName, AST.cDecls } =
  do updates <- collectErrors (map inferTopGroup (sccTopDecls cDecls))
     return $! foldl' (flip id) (emptyController (thing cName)) updates


inferTopGroup :: Group (AST.TopDecl Name) -> TC (Controller -> Controller)

inferTopGroup (NonRecursive td) =
  simpleTopDecl td

inferTopGroup (Recursive tds)   =
  case traverse mkLocFun tds of
    Just funs -> inferFunGroup funs
    _         -> invalidRecursiveGroup tds


mkLocFun :: AST.TopDecl Name -> Maybe (AST.Loc (AST.Fun Name))
mkLocFun  = go mempty
  where
  go _   (AST.TDLoc loc) = go (getLoc loc) (thing loc)
  go src (AST.TDFun fun) = Just (fun `at` src)
  go _   _               = Nothing



simpleTopDecl :: AST.TopDecl Name -> TC (Controller -> Controller)

simpleTopDecl (AST.TDLoc loc) = withLoc loc (simpleTopDecl (thing loc))

simpleTopDecl (AST.TDFun fun) = undefined



-- | Check a recursive group of functions:
--
--  * Introduce type-variables for all functions in the group
--  * Check each function individually 
--  * Apply the substitution to each function, yielding its type
inferFunGroup :: [AST.Loc (AST.Fun Name)] -> TC (Controller -> Controller)
inferFunGroup funs = undefined
