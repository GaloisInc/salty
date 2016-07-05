
import CodeGen.Java (javaFSM)
import Scope.Check
import Scope.Name (emptySupply)
import Slugs (runSlugs)
import Syntax.Parser
import TypeCheck

import           Control.Exception (catch,IOException)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy.IO as L
import           System.Environment (getArgs,lookupEnv)
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

     (tcCont,_) <-
       case typeCheck scSup scCont of
         Right tc  -> return tc
         Left errs -> do mapM_ (print . ppTCError) errs
                         exitFailure

     mbSlugs <- lookupEnv "SLUGS"
     let slugs = fromMaybe "../slugs/src/slugs" mbSlugs

     mbFSM <- runSlugs slugs tcCont `catch` \ e ->
                 do let _ = e :: IOException
                    putStrLn "Failed to run slugs. Is SLUGS set?"
                    exitFailure

     fsm <- case mbFSM of
              Just fsm -> return fsm
              Nothing  -> do putStrLn "Unrealizable"
                             exitFailure

     print (javaFSM fsm)
