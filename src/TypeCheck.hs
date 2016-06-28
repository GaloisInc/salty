module TypeCheck (
    typeCheck,
    TCError(..),
    module TypeCheck.PP
  ) where

import           Scope.Name (Name,Supply)
import qualified Syntax.AST as AST
import qualified TypeCheck.AST as Core
import           TypeCheck.Infer (inferController)
import           TypeCheck.Monad
import           TypeCheck.PP


typeCheck :: Supply -> AST.Controller Name
          -> Either [AST.Loc TCError] (Core.Controller,Supply)
typeCheck sup c = runTC sup (inferController c)
