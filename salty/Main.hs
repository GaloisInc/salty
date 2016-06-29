
import Opt.Simpl
import Scope.Check
import Scope.Name (emptySupply)
import Slugs.Translate (translateController)
import Syntax.Parser
import TypeCheck

import qualified Data.Text.Lazy.IO as L
import           Language.Slugs.PP (ppSpec)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

main :: IO ()
main  =
  do [file] <- getArgs

     bytes <- L.readFile file

     pCont <-
       case parseController file bytes of
         Right p  -> return p
         Left err -> do print err
                        exitFailure

     (scCont,scSup) <-
       case scopeCheck emptySupply pCont of
         Right sc  -> return sc
         Left errs -> do mapM_ print errs
                         exitFailure

     (tcCont,tcSup) <-
       case typeCheck scSup scCont of
         Right tc  -> return tc
         Left errs -> do mapM_ (print . ppTCError) errs
                         exitFailure

     print (pp tcCont)
     putStrLn "----"
     print (pp (simp (expand tcCont)))

     print (ppSpec (translateController tcCont))
