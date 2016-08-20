module Options where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs,getProgName)
import System.Exit (exitSuccess,exitFailure)
import System.FilePath (takeExtension)


data Options = Options { optHelp    :: Bool
                       , optJava    :: Maybe String
                       , optPython  :: Bool
                       , optDot     :: Bool
                       , optInput   :: Maybe Input
                       , optInputLen:: Maybe Int
                       , optOutDir  :: Maybe FilePath
                       , optSlugs   :: FilePath
                       , optDumpParsed :: Bool
                       , optDumpCore :: Bool
                       , optDumpSpec :: Bool
                       } deriving (Show)

data Input = InpSpec FilePath
             -- ^ A salty specification

           | InpJSON FilePath
             -- ^ Used when just generating code from the json output of slugs.

           | InpSlugsOut FilePath
             -- ^ Used when just generating code from the output of slugs.
             deriving (Show)

defaultOptions :: Options
defaultOptions  =
  Options { optHelp       = False
          , optJava       = Nothing
          , optPython     = False
          , optDot        = False
          , optInput      = Nothing
          , optInputLen   = Nothing
          , optOutDir     = Nothing
          , optSlugs      = "slugs"
          , optDumpParsed = False
          , optDumpCore   = False
          , optDumpSpec   = False
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

  , Option "d" ["dot"] (NoArg setDot)
    "Output a graphviz representation of the controller"

  , Option "o" ["output"] (ReqArg setOutDir "PATH")
    "Optional output directory for artifacts"

  , Option "s" ["slugs"] (ReqArg setSlugs "SLUGS_PATH")
    "The path to the slugs executable"

  , Option "l" ["length"] (ReqArg setInputLen "NUMBER")
    "When just using the code generator, this is the number of input variables in the slugs output"

  , Option "" ["ddump-parse"] (NoArg setDumpParsed)
    "Dump the parse tree for the controller"

  , Option "" ["ddump-core"] (NoArg setDumpCore)
    "Dump the core representation of the type-checked controller"

  , Option "" ["ddump-spec"] (NoArg setDumpSpec)
    "Dump the slugs spec before running slugs"
  ]

setHelp :: Parser
setHelp  = OK (\opts -> opts { optHelp = True })

setJava :: Maybe String -> Parser
setJava mb = OK (\opts -> opts { optJava = Just (fromMaybe "" mb) })

setInput :: String -> Parser
setInput str =
  case map toLower (takeExtension str) of
    ".json"     -> OK (\opts -> opts { optInput = Just (InpJSON     str) })
    ".salt"     -> OK (\opts -> opts { optInput = Just (InpSpec     str) })
    ".slugsout" -> OK (\opts -> opts { optInput = Just (InpSlugsOut str) })
    _           -> Error ["Unknown input filetype for `" ++ str ++ "`"]

setInputLen :: String -> Parser
setInputLen str =
  case reads str of
    [(x,"")] -> OK (\opts -> opts { optInputLen = Just x })
    _        -> Error ["Unable to parse input length"]

setOutDir :: String -> Parser
setOutDir str = OK (\opts -> opts { optOutDir = Just str })

setSlugs :: String -> Parser
setSlugs str = OK (\opts -> opts { optSlugs = str })

setDumpParsed :: Parser
setDumpParsed  = OK (\opts -> opts { optDumpParsed = True })

setDumpCore :: Parser
setDumpCore  = OK (\opts -> opts { optDumpCore = True })

setDumpSpec :: Parser
setDumpSpec  = OK (\opts -> opts { optDumpSpec = True })

setPython :: Parser
setPython  = OK (\opts -> opts { optPython = True })

setDot :: Parser
setDot  = OK (\opts -> opts { optDot = True })


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
