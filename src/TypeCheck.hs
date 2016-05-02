module TypeCheck (
    typeCheck,
    TCError(..)
  ) where

import           Scope.Name (Name)
import qualified Syntax.AST as AST
import qualified TypeCheck.AST as Core
import           TypeCheck.Infer (inferController)
import           TypeCheck.Monad


typeCheck :: AST.Controller Name -> Either [TCError] Core.Controller
typeCheck c = runTC (inferController c)
