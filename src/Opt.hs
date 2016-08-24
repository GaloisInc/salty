module Opt where

import Opt.HashCons (hashCons)
import Opt.Simpl (simp)
import Scope.Name (Supply)
import TypeCheck.AST (Controller)


opt :: Supply -> Controller -> (Controller,Supply)
opt s c = hashCons s (simp c)
