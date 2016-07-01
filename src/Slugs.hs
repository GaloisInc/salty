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

import qualified Language.Slugs.Run as S


runSlugs :: FilePath -> Controller -> IO (Maybe FSM)
runSlugs slugsProg c =
  do let (spec,env) = mkSpec (simp (expand c))
     res <- S.runSlugs slugsProg spec
     case res of
       S.Unrealizable    -> return Nothing
       S.StateMachine sm -> return (Just (fromSlugs env sm))
