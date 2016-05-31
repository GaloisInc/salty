module TypeCheck (
    typeCheck,
    TCError(..)
  ) where

import           Scope.Name (Name,Supply)
import qualified Syntax.AST as AST
import qualified TypeCheck.AST as Core
import           TypeCheck.Infer (inferController)
import           TypeCheck.Monad


typeCheck :: Supply -> AST.Controller Name
          -> Either [AST.Loc TCError] (Core.Controller,Supply)
typeCheck sup c = runTC sup (inferController c)
