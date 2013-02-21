module Main where

-- import           Text.ParserCombinators.UU.Utils hiding (runParser)
import           System.Environment (getArgs)
import           System.IO

import qualified Options
import           Options (Options)
import           Parser
import           Prettyprint

main :: IO ()
main = do
  opts <- getArgs >>= Options.get
  run opts

run :: Options -> IO ()
run opts = do
  case Options.mode opts of
    Options.Prettyprint -> do
      input <- hGetContents (Options.input opts)
      let output = prettyprint $ runParser (Options.inputFilename opts) pProgram input
      hPutStrLn (Options.output opts) output

    Options.Help -> Options.printHelp

  cleanUp opts

cleanUp :: Options -> IO ()
cleanUp opts = do
  hClose (Options.input opts)
  hClose (Options.output opts)
