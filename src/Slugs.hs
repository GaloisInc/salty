module Slugs (
    runSlugs,
    FSM(..),
    Node(..),
  ) where

import Slugs.FSM (fromSlugs,FSM(..),Node(..))
import Slugs.Translate (mkSpec)
import Opt.Simpl (simp)
import TypeCheck.AST (Controller)
import TypeCheck.Expand (expand)

import           Control.Monad (when)
import qualified Language.Slugs as S


runSlugs :: Bool -> FilePath -> Controller -> IO (Maybe FSM)
runSlugs dbg slugsProg c =
  do let cont = simp (expand c)
     let (spec,env) = mkSpec cont
     when dbg (print (S.ppSpec spec))
     res <- S.runSlugs slugsProg spec
     case res of
       S.Unrealizable    -> return Nothing
       S.StateMachine sm -> return (Just (fromSlugs env cont sm))
