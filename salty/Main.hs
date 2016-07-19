{-# LANGUAGE RecordWildCards #-}

import Options

import CodeGen.Java (Package,javaFSM)
import Scope.Check
import Scope.Name (emptySupply)
import Slugs (runSlugs)
import Syntax.Parser
import TypeCheck

import           Control.Exception (catch,IOException)
import           Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as L
import           System.Directory (createDirectoryIfMissing)
import           System.Exit (exitFailure)
import           System.FilePath (takeDirectory,(</>))
import           Text.Show.Pretty (ppShow)

main :: IO ()
main  =
  do opts <- parseOptions

     bytes <- L.readFile (optInput opts)

     pCont <-
       case parseController (optInput opts) bytes of
         Right p  -> return p
         Left err -> do print err
                        exitFailure

     when (optDumpParsed opts) (putStrLn (ppShow pCont))

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

     mbFSM <- runSlugs (optDumpSpec opts) (optSlugs opts) tcCont `catch` \ e ->
                 do let _ = e :: IOException
                    putStrLn "Failed to run slugs. Is SLUGS set?"
                    exitFailure

     fsm <- case mbFSM of
              Just fsm -> return fsm
              Nothing  -> do putStrLn "Unrealizable"
                             exitFailure

     writePackage opts (javaFSM (optPackage opts) fsm)


writePackage :: Options -> Package -> IO ()
writePackage opts pkg = mapM_ writeClass (Map.toList pkg)
  where
  prefix = case optOutDir opts of
             Just dir -> dir
             Nothing  -> ""

  writeClass (file,doc) =
    do let outFile = prefix </> file
       createDirectoryIfMissing True (takeDirectory outFile)

       putStrLn ("Writing " ++ outFile)
       writeFile outFile (show doc)
