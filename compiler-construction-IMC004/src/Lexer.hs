{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction #-}
module Lexer where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils

keywords =
  [ "Void"
  , "Int"
  , "Bool"
  , "if"
  , "else"
  , "while"
  , "return"
  , "False"
  , "True"
  ]

punctuation =
  [ '('
  , ')'
  ]

operators =
  [ '+'
  , '-'
  , '*'
  , '/'
  , '='
  ]

data Token
  = Keyword String
  | Punctuation Char
  | Identifier String
  | Number Integer
  | Operator Char
  deriving (Show)

lexer :: Parser [Token]
lexer = many (lexeme pTok)

pTok :: Parser Token
pTok =
      (foldl1 (<|>) $ map (Punctuation <$>) $ map pSym punctuation)
  <|> (foldl1 (<|>) $ map (Operator <$>) $ map pSym operators)
  <|> (\i -> if i `elem` keywords then Keyword i else Identifier i) <$> pIdentifier
  <|> Number <$> pInteger

pIdentifier :: Parser String
pIdentifier = ((:) <$> (pLetter <|> pSym '_') <*> pList1 (pDigit <|> pLetter <|> pSym '_'))
