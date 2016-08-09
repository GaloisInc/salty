module Panic (
    panic,
    HasCallStack
  ) where

import GHC.Stack (HasCallStack,withFrozenCallStack,callStack,getCallStack)

panic :: HasCallStack => String -> a
panic str =
  let stack  = getCallStack callStack

      origin | length stack > 1 = "(" ++ fst (stack !! 1) ++ ")"
             | otherwise        = ""

      msg   = [ "PANIC", origin, ": ", str ]

   in withFrozenCallStack (error (concat msg))
