module Options where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs,getProgName)
import System.Exit (exitSuccess,exitFailure)
import System.FilePath (takeExtension)


data Options = Options { optHelp         :: !Bool
                       , optOptLevel     :: !Int
                       , optSanity       :: !Bool
                       , optAnnotations  :: !Bool
                       , optJava         :: !(Maybe String)
                       , optCpp          :: !(Maybe [String])
                       , optPython       :: !Bool
                       , optDot          :: !Bool
                       , optSPARK        :: !Bool
                       , optInput        :: !(Maybe Input)
                       , optInputLen     :: !(Maybe Int)
                       , optOutDir       :: !(Maybe FilePath)
                       , optSlugs        :: !FilePath
                       , optZ3           :: !FilePath
                       , optDumpParsed   :: !Bool
                       , optDumpCore     :: !Bool
                       , optDumpExpanded :: !Bool
                       , optDumpSimp     :: !Bool
                       , optDumpOpt      :: !Bool
                       , optDumpSpec     :: !Bool
                       , optDumpSanity   :: !Bool
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
  Options { optHelp         = False
          , optOptLevel     = 1
          , optSanity       = True
          , optAnnotations  = False
          , optJava         = Nothing
          , optCpp          = Nothing
          , optPython       = False
          , optDot          = False
          , optSPARK        = False
          , optInput        = Nothing
          , optInputLen     = Nothing
          , optOutDir       = Nothing
          , optSlugs        = "slugs"
          , optZ3           = "z3"
          , optDumpParsed   = False
          , optDumpCore     = False
          , optDumpExpanded = False
          , optDumpSimp     = False
          , optDumpOpt      = False
          , optDumpSpec     = False
          , optDumpSanity   = False
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

  , Option "a" ["annotations"] (NoArg setAnnotations)
    "Output information about annotations"

  , Option "j" ["java"] (OptArg setJava "PACKAGE_STRING")
    "Output a java implementation of the controller, as this package"

  , Option "p" ["python"] (NoArg setPython)
    "Output a python implementation of the controller"

  , Option "" ["cpp"] (OptArg setCpp "CPP_NAMESPACE")
    "Output a c++ implementation of the controller"

  , Option "d" ["dot"] (NoArg setDot)
    "Output a graphviz representation of the controller"

  , Option "k" ["spark"] (NoArg setSPARK)
    "Output a SPARK Ada implementation of the controller"

  , Option "o" ["output"] (ReqArg setOutDir "PATH")
    "Optional output directory for artifacts"

  , Option "s" ["slugs"] (ReqArg setSlugs "SLUGS_PATH")
    "The path to the slugs executable"

  , Option "z3" ["z3"] (ReqArg setZ3 "Z3_PATH")
    "The path to the z3 executable"

  , Option "l" ["length"] (ReqArg setInputLen "NUMBER")
    "When just using the code generator, this is the number of input variables in the slugs output"

  , Option "O" [] (ReqArg setOptLevel "NUMBER")
    "Enable/disable optimizations by passing 0/1"

  , Option "" ["disable-sanity"] (NoArg disableSanity)
    "Disable the sanity checker"

  , Option "" ["ddump-parse"] (NoArg setDumpParsed)
    "Dump the parse tree for the controller"

  , Option "" ["ddump-core"] (NoArg setDumpCore)
    "Dump the core representation of the type-checked controller"

  , Option "" ["ddump-expanded"] (NoArg setDumpExpanded)
    "Dump the expanded form of the controller"

  , Option "" ["ddump-simpl"] (NoArg setDumpSimp)
    "Dump the simplified expanded form of the controller"

  , Option "" ["ddump-opt"] (NoArg setDumpOpt)
    "Dump the optimized core representation of the type-checked controller"

  , Option "" ["ddump-spec"] (NoArg setDumpSpec)
    "Dump the input to slugs and its output"

  , Option "" ["ddump-sanity"] (NoArg setDumpSanity)
    "Dump intermediate output during sanity checking"
  ]

setHelp :: Parser
setHelp  = OK (\opts -> opts { optHelp = True })

setJava :: Maybe String -> Parser
setJava mb = OK (\opts -> opts { optJava = Just (fromMaybe "" mb) })

setCpp :: Maybe String -> Parser
setCpp mb =
  case mb of
    Just str ->
      case parseCppNs str of
        Just ns -> OK (\opts -> opts { optCpp = Just ns })
        Nothing -> Error ["Failed to parse C++ namespace: " ++ str]

    Nothing -> OK (\opts -> opts { optCpp = Just [] })

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

setAnnotations :: Parser
setAnnotations  = OK (\opts -> opts { optAnnotations = True })

setSlugs :: String -> Parser
setSlugs str = OK (\opts -> opts { optSlugs = str })

setZ3 :: String -> Parser
setZ3 str = OK (\opts -> opts { optZ3 = str })

setOptLevel :: String -> Parser
setOptLevel str =
  case reads str of
    [(x,"")] -> OK (\opts -> opts { optOptLevel = x })
    _        -> Error ["Failed to parse a number for optimization level"]

disableSanity :: Parser
disableSanity  = OK (\opts -> opts { optSanity = False })

setDumpParsed :: Parser
setDumpParsed  = OK (\opts -> opts { optDumpParsed = True })

setDumpCore :: Parser
setDumpCore  = OK (\opts -> opts { optDumpCore = True })

setDumpExpanded :: Parser
setDumpExpanded  = OK (\opts -> opts { optDumpExpanded = True })

setDumpSimp :: Parser
setDumpSimp  = OK (\opts -> opts { optDumpSimp = True })

setDumpOpt :: Parser
setDumpOpt  = OK (\opts -> opts { optDumpOpt = True })

setDumpSpec :: Parser
setDumpSpec  = OK (\opts -> opts { optDumpSpec = True })

setDumpSanity :: Parser
setDumpSanity  = OK (\opts -> opts { optDumpSanity = True })

setPython :: Parser
setPython  = OK (\opts -> opts { optPython = True })

setDot :: Parser
setDot  = OK (\opts -> opts { optDot = True })

setSPARK :: Parser
setSPARK  = OK (\opts -> opts { optSPARK = True })

parseCppNs :: String -> Maybe [String]
parseCppNs  = go []
  where
  go acc str =
    case break (== ':') str of
      (as, ':':':':bs) -> go (as:acc) bs
      (as, [])         -> Just (reverse (as:acc))
      _                -> Nothing

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
