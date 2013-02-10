{-# LANGUAGE  FlexibleContexts #-}

module UUParsinglib where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

pa = pSym 'a'
parseA = run pa "aabb"

parens :: Parser Int
parens =
  (max . (+1))
    <$ (pSym '(') <*> parens <* pSym ')' <*> parens
  <|> pure 0
parseParens = run parens "#(())((()))"
-- parseParens = run parensLeftRec "#"

parensLeftRec :: Parser Int
parensLeftRec =
  (\q _ p _ -> max (p+1) q)
    <$> parens <*> (pSym '(') <*> parens <*> pSym ')'
  <|> pure 0

run p input = parse ((,) <$> p <*> pEnd) (createStr (LineCol 0 0) input)

pKwElse :: Parser String
pKwElse = pToken "else"

parseKwElse = run pKwElse "else nothing"

pDigit :: Parser Char
pDigit = pRange ('0', '9')

pDigitAsInt :: Parser Int
pDigitAsInt = (\c -> fromEnum c - fromEnum '0') <$> pDigit

pInt :: Parser Int
-- pInt = foldl (\num digit -> num * 10 + digit) 0 <$> pMany pDigitAsInt
pInt = pChainl (pure $ \num digit -> num * 10 + digit) pDigitAsInt

pSignedInt :: Parser Int
pSignedInt = (   (* (-1)) <$ pSym '-'
             <|> id <$ pSym '+'
             <|> pure id
             ) <*> pInt


main = do
  let (a, errors) = parseKwElse
  putStrLn ("-- Result: " ++ show a)
  if null errors
    then return ()
    else do putStrLn ("-- Errors: ")
            (sequence_ . (map (putStrLn . show))) errors
