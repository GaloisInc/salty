module Options where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs,getProgName)
import System.Exit (exitSuccess,exitFailure)


data Options = Options { optHelp    :: Bool
                       , optJava    :: Maybe String
                       , optPython  :: Bool
                       , optInput   :: FilePath
                       , optOutDir  :: Maybe FilePath
                       , optSlugs   :: FilePath
                       , optDumpParsed :: Bool
                       , optDumpSpec :: Bool
                       } deriving (Show)

defaultOptions :: Options
defaultOptions  =
  Options { optHelp    = False
          , optJava    = Nothing
          , optPython  = False
          , optInput   = "controller.salt"
          , optOutDir  = Nothing
          , optSlugs   = "slugs"
          , optDumpParsed = False
          , optDumpSpec = False
          }


data Parser = OK (Options -> Options)
            | Error [String]

instance Monoid Parser where
  mempty = OK id

  mappend (OK f)    (OK g)    = OK (g . f)
  mappend (Error a) (Error b) = Error (a ++ b)
  mappend a@Error{} _         = a
  mappend _         b         = b


options :: [OptDescr Parser]
options  =
  [ Option "h" ["help"] (NoArg setHelp)
    "Display this message"

  , Option "j" ["java"] (OptArg setJava "PACKAGE_STRING")
    "Output a java implementation of the controller, as this package"

  , Option "p" ["python"] (NoArg setPython)
    "Output a python implementation of the controller"

  , Option "o" ["output"] (ReqArg setOutDir "PATH")
    "Optional output directory for artifacts"

  , Option "s" ["slugs"] (ReqArg setSlugs "SLUGS_PATH")
    "The path to the slugs executable"

  , Option "" ["ddump-parse"] (NoArg setDumpParsed)
    "Dump the parse tree for the controller"

  , Option "" ["ddump-spec"] (NoArg setDumpSpec)
    "Dump the slugs spec before running slugs"
  ]

setHelp :: Parser
setHelp  = OK (\opts -> opts { optHelp = True })

setJava :: Maybe String -> Parser
setJava mb = OK (\opts -> opts { optJava = Just (fromMaybe "" mb) })

setInput :: String -> Parser
setInput str = OK (\opts -> opts { optInput = str })

setOutDir :: String -> Parser
setOutDir str = OK (\opts -> opts { optOutDir = Just str })

setSlugs :: String -> Parser
setSlugs str = OK (\opts -> opts { optSlugs = str })

setDumpParsed :: Parser
setDumpParsed  = OK (\opts -> opts { optDumpParsed = True })

setDumpSpec :: Parser
setDumpSpec  = OK (\opts -> opts { optDumpSpec = True })

setPython :: Parser
setPython  = OK (\opts -> opts { optPython = True })


parseOptions :: IO Options
parseOptions  =
  do args <- getArgs
     case getOpt (ReturnInOrder setInput) options args of
       (ps,[],[]) ->
         case mconcat ps of

           OK mk ->
             do let opts = mk defaultOptions
                when (optHelp opts) $
                  do showUsage []
                     exitSuccess
                return opts

           Error errs ->
             do showUsage errs
                exitFailure

       (_,_,errs) ->
         do showUsage errs
            exitFailure

showUsage :: [String] -> IO ()
showUsage errs =
  do prog <- getProgName
     let banner = "Usage: " ++ prog ++ " [OPTIONS] <controller.salt>"
     putStrLn (usageInfo (unlines (errs ++ ["", banner])) options)
