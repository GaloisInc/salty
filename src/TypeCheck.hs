module TypeCheck (
    typeCheck,
    TCError(..),
    expand,

    module TypeCheck.Sanity,
  ) where

import           Scope.Check (Renamed)
import           Scope.Name (Supply)
import qualified Syntax.AST as AST
import qualified TypeCheck.AST as Core
import           TypeCheck.Expand
import           TypeCheck.Infer (inferController)
import           TypeCheck.Monad
import           TypeCheck.Sanity


typeCheck :: Supply -> AST.Controller Renamed
          -> Either [TCError] (Core.Controller,Supply)
typeCheck sup c = runTC sup (inferController c)
