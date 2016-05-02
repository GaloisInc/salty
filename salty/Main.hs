
import Scope.Check
import Syntax.Parser
import TypeCheck

import qualified Data.Text.Lazy.IO as L
import           System.Exit (exitFailure)

main :: IO ()
main  =
  do bytes <- L.readFile "test.salt"

     pCont <-
       case parseController "test.salt" bytes of
         Right p  -> return p
         Left err -> do print err
                        exitFailure

     scCont <-
       case scopeCheck pCont of
         Right sc  -> return sc
         Left errs -> do mapM_ print errs
                         exitFailure

     print scCont

     tcCont <-
       case typeCheck scCont of
         Right tc  -> return tc
         Left errs -> do mapM_ print errs
                         exitFailure

     print tcCont
