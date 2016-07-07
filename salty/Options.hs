module Options where

import Control.Monad (when)
import System.Console.GetOpt
import System.Environment (getArgs,getProgName)
import System.Exit (exitSuccess,exitFailure)


data Options = Options { optHelp    :: Bool
                       , optPackage :: String
                       , optInput   :: FilePath
                       , optSlugs   :: FilePath
                       } deriving (Show)

defaultOptions :: Options
defaultOptions  =
  Options { optHelp    = False
          , optPackage = "salty"
          , optInput   = "controller.salt"
          , optSlugs   = "slugs"
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

  , Option "p" ["package"] (ReqArg setPackage "PACKAGE_STRING")
    "The base package name to use for java code generation"

  , Option "s" ["slugs"] (ReqArg setSlugs "SLUGS_PATH")
    "The path to the slugs executable"
  ]

setHelp :: Parser
setHelp  = OK (\opts -> opts { optHelp = True })

setPackage :: String -> Parser
setPackage str = OK (\opts -> opts { optPackage = str })

setInput :: String -> Parser
setInput str = OK (\opts -> opts { optInput = str })

setSlugs :: String -> Parser
setSlugs str = OK (\opts -> opts { optSlugs = str })


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
