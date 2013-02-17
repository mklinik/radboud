module Main where

import Text.ParserCombinators.UU.Utils

import Parser

-- main = do
  -- sequence_ $ map putStrLn keywords

main :: IO ()
main = do
  -- print $ runParser "test" pProg "(Int,Int) foo=100;"
  interact $ show . runParser "stdio" pProg
