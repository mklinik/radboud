module Main where

import           Text.ParserCombinators.UU.Utils
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.IO

import Options
import Parser

main :: IO ()
main = do
  args <- getArgs
  interact $ show . runParser "stdio" pProg

run :: Options -> IO ()
run options = hGetContents (input options) >>= return . show . runParser "" pProg >>= hPutStrLn (output options)
