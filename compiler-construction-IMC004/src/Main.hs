module Main where

import           Text.ParserCombinators.UU.Utils
import           System.Environment (getArgs)
import           System.IO

import qualified Options
import           Options (Options)
import           Parser

main :: IO ()
main = do
  opts <- getArgs >>= Options.get
  run opts

run :: Options -> IO ()
run opts = do
  case Options.mode opts of
    Options.Prettyprint -> hGetContents (Options.input opts) >>= return . show . runParser "" pProg >>= hPutStrLn (Options.output opts)
    Options.Help -> Options.printHelp
  cleanUp opts

cleanUp :: Options -> IO ()
cleanUp opts = do
  hClose (Options.input opts)
  hClose (Options.output opts)
