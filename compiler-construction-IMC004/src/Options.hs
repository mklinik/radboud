module Options where

import System.IO
import System.Console.GetOpt
import Text.Printf (printf)
import Control.Monad
import System.Exit

programName :: String
programName = "spl"

data Mode = Help | Prettyprint | CheckParser
  deriving (Show)

data Options = Options
  { input :: Handle
  , inputFilename :: String
  , output :: Handle
  , outputFilename :: String
  , mode :: Mode
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { input = stdin
  , inputFilename = "--"
  , output = stdout
  , outputFilename = "--"
  , mode = Prettyprint
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option []     ["help"]    (NoArg  (\  opts -> opts { mode = Help }))               "display this help and exit"
  , Option ['i']  ["input"]   (ReqArg (\s opts -> opts { inputFilename = s }) "FILE")  "file to read input from;\nuse \"--\" to read from stdin;\nwhen unspecified, defaults to stdin"
  , Option ['o']  ["output"]  (ReqArg (\s opts -> opts { outputFilename = s }) "FILE") "file to write output to;\nuse \"--\" to write to stdout;\nwhen unspecified, defaults to stdout"
  , Option []  ["check"]      (NoArg  (\  opts -> opts { mode = CheckParser }))              "parse, prettyprint, parse and compare ASTs"
  ]

get :: [String] -> IO Options
get args = do
  let (opts_, files, errors) = getOpt Permute options args
  let opts__ = foldl (flip id) defaultOptions opts_
  opts <- optOpenOutputFile opts__ >>= optOpenInputFile

  when ((not . null) errors)
    (tryHelp $ head errors)

  when ((not . null) files)
    (tryHelp $ printf "unrecognized option `%s'\n" $ head files)

  return opts

  where
    printAndExit :: String -> IO a
    printAndExit s = putStr s >> exitFailure

    tryHelp message = printAndExit $ programName ++ ": " ++ message
      ++ "Try `" ++ programName ++ " --help' for more information.\n"

optOpenInputFile :: Options -> IO Options
optOpenInputFile opts = do
  case (inputFilename opts) of
    "--" -> return opts { input = stdin }
    _    -> do
      handle <- openFile (inputFilename opts) ReadMode
      return opts { input = handle }

optOpenOutputFile :: Options -> IO Options
optOpenOutputFile opts = do
  case (outputFilename opts) of
    "--" -> return opts { output = stdout }
    _    -> do
      handle <- openFile (outputFilename opts) WriteMode
      return opts { output = handle }

printHelp :: IO ()
printHelp = putStr $ usageInfo "Usage: spl [OPTION]...\n" options
