{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction #-}

module UUParsinglib where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils hiding (pDigit)

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

pDigit :: Parser Char
pDigit = pRange ('0', '9')

pDigitAsInt :: Parser Int
pDigitAsInt = (\c -> fromEnum c - fromEnum '0') <$> pDigit

pInt :: Parser Int
-- pInt = foldl (\num digit -> num * 10 + digit) 0 <$> pMany pDigitAsInt
pInt = pChainl (pure $ \num digit -> num * 10 + digit) pDigitAsInt

pSignedInt :: Parser Int
pSignedInt = lexeme $
  (   lexeme ((* (-1)) <$ pSym '-')
  <|> lexeme (id <$ pSym '+')
  <|> pure id
  ) <*> pInt

pPlus :: Parser Int
pPlus = (+) <$> pSignedInt <* lexeme (pSym '+') <*> pSignedInt

pPlusMinus :: Parser Int
pPlusMinus = pChainl addops pSignedInt

pOp (sem, symbol) = sem <$ lexeme (pSym symbol)

pChoice = foldr (<|>) pFail

anyOp = pChoice . map pOp

addops = anyOp [((+), '+'), ((-), '-')]
mulops = anyOp [((*), '*')]

pTimes = pChainl mulops pSignedInt
pPlusMinusTimes = foldr pChainl pSignedInt [addops, mulops]

pPack l p r = lexeme (pSym l) *> p <* lexeme (pSym r)

pExpr = pSpaces *> (foldr pChainl pFactor [addops, mulops] <|> pIfThenElse)
pFactor = pSignedInt <|> pPack '(' pExpr ')'

pIfThenElse = ifThenElse <$ lexeme (pToken "if") <*> pBoolExpr <* lexeme (pToken "then") <*> pExpr <* lexeme (pToken "else") <*> pExpr

ifThenElse i t e = if i then t else e

pBoolExpr :: Parser Bool
pBoolExpr =
      lexeme (pure True  <* pToken "True")
  <|> lexeme (pure False <* pToken "False")

main = do
  interact $ show . runParser "signed integer" (pSpaces *> pSignedInt)
  putStrLn ""
