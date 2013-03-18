{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module Parser where

import Text.ParserCombinators.UU
import qualified Text.ParserCombinators.UU.BasicInstances as PC
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils hiding (pNatural, lexeme, pSymbol)
import Data.Char (ord)

import Ast

-- basically the same as Parser from uu-parsinglib, but not as general. We fix String and LineColPos
type SplParser a = P (PC.Str Char String PC.LineColPos) a

pProgram :: SplParser AstProgram
pProgram = AstProgram <$ lexeme (pure ()) <*> some pDeclaration

pDeclaration :: SplParser AstDeclaration
pDeclaration = pVarDeclaration <|> pFunDeclaration

pVarDeclaration :: SplParser AstDeclaration
pVarDeclaration = AstVarDeclaration <$> ((\s -> AstMeta { sourceLocation = PC.pos s }) <$> pState) <*> pType defaultBaseTypes <*> pIdentifier <* pSymbol "=" <*> pExpr <* pSymbol ";"

defaultBaseTypes :: [String]
defaultBaseTypes = ["Int", "Bool"]

pFunDeclaration :: SplParser AstDeclaration
pFunDeclaration =
  AstFunDeclaration
    <$> pReturnType <*> pIdentifier
    <* pSymbol "(" <*> opt pFunctionArguments [] <* pSymbol ")"
    <* pSymbol "{" <*> many pVarDeclaration <*> some pStatement <* pSymbol "}"

pReturnType :: Parser AstType
pReturnType = pType ("Void" : defaultBaseTypes)

pType :: [String] -> Parser AstType
pType baseTypes =
      mkBaseTypeOrIdentifier baseTypes <$> pIdentifier
  <|> TupleType <$ pSymbol "(" <*> pType baseTypes <* pSymbol "," <*> pType baseTypes <* pSymbol ")"
  <|> ListType <$ pSymbol "[" <*> pType baseTypes <* pSymbol "]"

mkBaseTypeOrIdentifier :: [String] -> String -> AstType
mkBaseTypeOrIdentifier baseTypes s =
  if s `elem` baseTypes
    then BaseType s
    else PolymorphicType s

pFunctionArguments :: Parser [AstFunctionArgument]
pFunctionArguments = (:) <$> pFunctionArgument <*> opt (pSymbol "," *> pFunctionArguments) []

pFunctionArgument :: Parser AstFunctionArgument
pFunctionArgument = AstFunctionArgument <$> pType defaultBaseTypes <*> pIdentifier

pStatement :: Parser AstStatement
pStatement =
      AstBlock <$ pSymbol "{" <*> many pStatement <* pSymbol "}"
  <|> AstIfThenElse <$ pSymbol "if" <* pSymbol "(" <*> pExpr <* pSymbol ")" <*> pStatement <*>
        (pSymbol "else" *> pStatement  <<|> pure (AstBlock []))
  <|> AstWhile <$ pSymbol "while" <* pSymbol "(" <*> pExpr <* pSymbol ")" <*> pStatement
  <|> AstAssignment <$> pIdentifier <* pSymbol "=" <*> pExpr <* pSymbol ";"
  <|> (     AstReturn <$ pSymbol "return" <*> opt (Just <$> pExpr) Nothing <* pSymbol ";"
       <<|> AstFunctionCallStmt <$> pFunctionCall <* pSymbol ";"
      )

pExpr :: Parser AstExpr
pExpr = pChainr (mkOp ":") $ foldr pChainl pBaseExpr pBinOps
  where
    pBinOps = map (foldr (<|>) pFail) $ map (map mkOp) binOps

mkOp :: String -> Parser (AstExpr -> AstExpr -> AstExpr)
mkOp op = AstBinOp <$> pSymbol op

-- Precedence levels are the same as in C (except for `cons`, of course)
binOps :: [[String]]
binOps =
  [ [ "||" ]
  , [ "&&" ]
  , [ "==" , "!=" ]
  , [ "<" , ">" , "<=" , ">=" ]
  , [ "+" , "-" ]
  , [ "*" , "/", "%" ]
  ]

pBaseExpr :: Parser AstExpr
pBaseExpr =
      mkBoolOrIdentifier <$> pIdentifier
  <|> AstUnaryOp <$> pSymbol "!" <*> pExpr
  <|> AstInteger <$> pNatural
  <|> pSymbol "(" *> pExpr <* pSymbol ")"
  <|> AstFunctionCallExpr <$> pFunctionCall
  <|> AstEmptyList <$ pSymbol "[" <* pSymbol "]"
  <|> AstTuple <$ pSymbol "(" <*> pExpr <* pSymbol "," <*> pExpr <* pSymbol ")"
  <|> pSymbol "-" *> pNegatedExpression
  <?> "Expression"

pIdentifier :: Parser String
pIdentifier = lexeme ((:) <$> pLetter <*> many (pLetter <|> pDigit <|> PC.pSym '_')) <?> "Identifier"

pFunctionCall :: Parser AstFunctionCall
pFunctionCall = AstFunctionCall <$> pIdentifier <* pSymbol "(" <*> opt pActualParameters [] <* pSymbol ")"

pNegatedExpression :: Parser AstExpr
pNegatedExpression =
       AstInteger . negate <$> pNatural
  <<|> AstUnaryOp "-" <$> pExpr

pActualParameters :: Parser [AstExpr]
pActualParameters = (:) <$> pExpr <*> opt (pSymbol "," *> pActualParameters) []

booleanConstants :: [String]
booleanConstants = ["True", "False"]

mkBoolOrIdentifier :: String -> AstExpr
mkBoolOrIdentifier s =
  if s `elem` booleanConstants
    then AstBoolean (s == "True")
    else AstIdentifier s

pNatural :: Parser Integer
pNatural = lexeme $
  pChainl (pure $ \num digit -> num * 10 + digit) ((\c -> toInteger (ord c - ord '0')) <$> pDigit)

pSymbol :: String -> Parser String
pSymbol = lexeme . PC.pToken

lexeme :: PC.ParserTrafo a a
lexeme p = p <* many (pSpace <<|> pComment)

pSpace :: Parser ()
pSpace = () <$ pAnySym " \r\n\t" <?> "Whitespace"

pComment :: Parser ()
pComment = pLineComment <|> pBlockComment <?> "Comment"

pLineComment :: Parser ()
pLineComment = () <$ PC.pToken "//" <* PC.pMunch (/= '\n') <* PC.pSym '\n'

pBlockComment :: Parser ()
pBlockComment = () <$ PC.pToken "/*" <* pCrapUntilBlockCommentEnd

pCrapUntilBlockCommentEnd :: Parser ()
pCrapUntilBlockCommentEnd =
  () <$ PC.pMunch (/= '*') <* -- eat everything that's not a star
    (() <$ PC.pToken "*/" <<|> -- if we get a comment-end delimiter, we're done
     () <$ PC.pSym '*' <* pCrapUntilBlockCommentEnd -- otherwise, eat the star and continue
    )
