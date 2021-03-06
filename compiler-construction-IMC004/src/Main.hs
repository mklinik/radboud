module Main where

import System.Environment (getArgs)
import System.IO
import System.Console.GetOpt
import Text.Printf (printf)
import Control.Monad

import Parser
import Prettyprinter
import Interpreter
import Utils
import Ast
import qualified Typechecker as TC
import Repl
import System.Exit
import Compile
import CompileError

main :: IO ()
main = do
  opts <- getArgs >>= get
  run opts >>= exitWith

parseAnd :: (AstProgram -> String) -> Options -> IO ()
parseAnd action opts = parseAnd_ (return . action) opts

parseAnd_ :: (AstProgram -> IO String) -> Options -> IO ()
parseAnd_ action opts = do
  input <- hGetContents (inFile opts)
  case runParser_ (inputFilename opts) pProgram input of
    Right ast -> do
      action ast >>= hPutStrLn (outFile opts)
    Left err  -> print err

parseAndE :: Show a => (AstProgram -> Either a String) -> Options -> IO ExitCode
parseAndE action opts = do
  input <- hGetContents (inFile opts)
  case runParser_ (inputFilename opts) pProgram input of
    Right ast -> do
      case action ast of
        Right s -> do
          hPutStrLn (outFile opts) s
          return ExitSuccess
        Left err -> do
          hPutStrLn (outFile opts) $ show err
          return $ ExitFailure 1
    Left err -> do
      hPutStrLn (outFile opts) $ show err
      return $ ExitFailure 1


run :: Options -> IO ExitCode
run opts = do
  returnCode <- case mode opts of
    ModePrettyprint -> do
      parseAnd (prettyprint) opts
      return ExitSuccess

    ModeShow -> do
      parseAnd show opts
      return ExitSuccess

    ModeHelp -> do
      printHelp
      return ExitSuccess

    ModeCheckParser -> do
      input <- hGetContents (inFile opts)
      print $
        do ast1 <- runParser_ (inputFilename opts) pProgram input
           ast2 <- runParser_ (inputFilename opts) pProgram $ prettyprint ast1
           Right (ast1 == ast2)
      return ExitSuccess

    ModeInterpret -> do
      parseAnd_ runProgram opts
      return ExitSuccess

    ModeTypecheck -> do
      parseAndE typecheck opts

    ModeInteractive -> do
      putStrLn ("Welcome to " ++ programName ++ " interactive mode.")
      putStrLn "Type Ctrl-d to exit."
      readEvalPrintLoop
      return ExitSuccess

    ModeCompile -> do
      input <- hGetContents (inFile opts)
      case compileSsm (inputFilename opts) input of
        Left err -> do
          hPutStrLn stderr $ show err
          return $ ExitFailure 1
        Right asm -> do
          mapM_ (prettyprintAsm (outFile opts)) asm
          return ExitSuccess

    ModeCompileNoTypecheck -> do
      input <- hGetContents (inFile opts)
      case compileSsmNoTypecheck (inputFilename opts) input of
        Left err -> do
          hPutStrLn stderr $ show err
          return $ ExitFailure 1
        Right asm -> do
          mapM_ (prettyprintAsm (outFile opts)) asm
          return ExitSuccess

  cleanUp opts
  return returnCode

prettyprintAsm :: Handle -> String -> IO ()
prettyprintAsm file line = do
  unless (':' `elem` line) (hPutStr file "    ") -- indent all but labelled lines
  hPutStrLn file line

typecheck :: AstProgram -> Either CompileError String
typecheck ast =
  let result = TC.runTypecheck $ TC.typecheck ast
  in
    case result of
    Left err -> Left err
    Right (_, ast2) -> Right $ prettyprint ast2

cleanUp :: Options -> IO ()
cleanUp opts = do
  hClose (inFile opts)
  hClose (outFile opts)

programName :: String
programName = "spl"

data Mode
  = ModeHelp
  | ModePrettyprint
  | ModeCheckParser
  | ModeShow
  | ModeInterpret
  | ModeTypecheck
  | ModeInteractive
  | ModeCompile
  | ModeCompileNoTypecheck
  deriving (Show)

data Options = Options
  { inFile :: Handle
  , inputFilename :: String
  , outFile :: Handle
  , outputFilename :: String
  , mode :: Mode
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { inFile = stdin
  , inputFilename = "--"
  , outFile = stdout
  , outputFilename = "--"
  , mode = ModePrettyprint
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option []     ["help"]    (NoArg  (\  opts -> opts { mode = ModeHelp }))               "display this help and exit"
  , Option ['i']  ["input"]   (ReqArg (\s opts -> opts { inputFilename = s }) "FILE")  "file to read input from;\nuse \"--\" to read from stdin;\nwhen unspecified, defaults to stdin"
  , Option ['o']  ["output"]  (ReqArg (\s opts -> opts { outputFilename = s }) "FILE") "file to write output to;\nuse \"--\" to write to stdout;\nwhen unspecified, defaults to stdout"
  , Option []  ["check"]      (NoArg  (\  opts -> opts { mode = ModeCheckParser }))        "parse, prettyprint, parse and compare ASTs"
  , Option []  ["show"]       (NoArg  (\  opts -> opts { mode = ModeShow }))               "parse, then show"
  , Option []  ["interpret"]  (NoArg  (\  opts -> opts { mode = ModeInterpret }))          "parse, then interpret"
  , Option []  ["typecheck"]  (NoArg  (\  opts -> opts { mode = ModeTypecheck }))          "parse, then typecheck"
  , Option []  ["interactive"](NoArg  (\  opts -> opts { mode = ModeInteractive }))        "read-eval-print loop"
  , Option ['c']  ["compile"] (NoArg  (\  opts -> opts { mode = ModeCompile }))            "generate assembly code for the Simple Stack Machine"
  , Option []  ["compile-unsafe"] (NoArg  (\  opts -> opts { mode = ModeCompileNoTypecheck })) "generate assembly code for the Simple Stack Machine, but don't perform typechecking"
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
    "--" -> return opts { inFile = stdin }
    _    -> do
      handle <- openFile (inputFilename opts) ReadMode
      return opts { inFile = handle }

optOpenOutputFile :: Options -> IO Options
optOpenOutputFile opts = do
  case (outputFilename opts) of
    "--" -> return opts { outFile = stdout }
    _    -> do
      handle <- openFile (outputFilename opts) WriteMode
      return opts { outFile = handle }

printHelp :: IO ()
printHelp = putStr $ usageInfo "Usage: spl [OPTION]...\n" options
