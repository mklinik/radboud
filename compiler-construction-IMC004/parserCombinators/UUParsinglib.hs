{-# LANGUAGE  FlexibleContexts #-}

module UUParsinglib where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances

pa = pSym 'a'

parseA = parse ((,) <$> pa <*> pEnd) (createStr (LineColPos 0 0 0) "zaabb")

main = do
  let (a, errors) = parseA
  putStrLn ("-- Result: " ++ show a)
  if null errors
    then return ()
    else do putStrLn ("-- Errors: ")
            (sequence_ . (map (putStrLn . show))) errors
