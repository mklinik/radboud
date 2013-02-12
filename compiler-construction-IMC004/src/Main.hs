module Main where

import Lexer
import Parser

main :: IO ()
main = do
  sequence_ $ map putStrLn keywords
