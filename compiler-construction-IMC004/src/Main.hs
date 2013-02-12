module Main where

import Lexer

main :: IO ()
main = do
  sequence_ $ map putStrLn keywords
