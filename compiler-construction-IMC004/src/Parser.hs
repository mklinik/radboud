{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module Parser where

import Text.ParserCombinators.UU
import qualified Text.ParserCombinators.UU.BasicInstances as UU
import Text.ParserCombinators.UU.Utils hiding (pNatural, lexeme, pSymbol)
import Data.Char (ord)

import Ast

-- basically the same as Parser from uu-parsinglib, but not as general. We fix String and LineColPos
type SplParser a = P (UU.Str Char String UU.LineColPos) a
type SplParserTrafo a b = SplParser a -> SplParser b

pProgram :: SplParser AstProgram
pProgram = AstProgram <$ lexeme (pure ()) <*> some pDeclaration

pDeclaration :: SplParser [AstDeclaration]
pDeclaration =  ((:[]) <$> pSingleDecl) <|> pFunDeclBlock

pSingleDecl :: SplParser AstDeclaration
pSingleDecl = pVarDeclaration <|> pFunDeclaration

pFunDeclBlock :: SplParser [AstDeclaration]
pFunDeclBlock = pSymbol "{" *> many pFunDeclaration <* pSymbol "}"

pSourceLocation :: SplParser AstMeta
pSourceLocation = (\s -> AstMeta { sourceLocation = UU.pos s, inferredType = Nothing }) <$> pState

pVarDeclaration :: SplParser AstDeclaration
pVarDeclaration = AstVarDeclaration <$> pSourceLocation <*> pType defaultBaseTypes <* pSpace <*> pIdentifier <* pSymbol "=" <*> pExpr <* pSymbol ";"

defaultBaseTypes :: [String]
defaultBaseTypes = ["Int", "Bool"]

pFunDeclaration :: SplParser AstDeclaration
pFunDeclaration =
  AstFunDeclaration
    <$> pSourceLocation <*> pReturnType <*> pIdentifier
    <* pSymbol "(" <*> opt pFunctionArguments [] <* pSymbol ")"
    <* pSymbol "{" <*> many pVarDeclaration <*> some pStatement <* pSymbol "}"

pReturnType :: SplParser AstType
pReturnType = pType ("Void" : defaultBaseTypes)

pType :: [String] -> SplParser AstType
pType baseTypes =
      mkBaseTypeOrIdentifier baseTypes <$> pSourceLocation <*> pIdentifier
  <|> TupleType <$> pSourceLocation <* pSymbol "(" <*> pType baseTypes <* pSymbol "," <*> pType baseTypes <* pSymbol ")"
  <|> ListType <$> pSourceLocation <* pSymbol "[" <*> pType baseTypes <* pSymbol "]"
  <|> RecordType <$> pSourceLocation <* pSymbol "{" <*> pListSep (pSymbol ",") pRecordFieldType <* pSymbol "}"

pRecordFieldType :: SplParser AstRecordFieldType
pRecordFieldType = AstRecordFieldType <$> pSourceLocation <*> pType defaultBaseTypes <*> pIdentifier

mkBaseTypeOrIdentifier :: [String] -> AstMeta -> String -> AstType
mkBaseTypeOrIdentifier baseTypes m s =
  if s `elem` baseTypes
    then BaseType m s
    else PolymorphicType m s

pFunctionArguments :: SplParser [AstFunctionArgument]
pFunctionArguments = (:) <$> pFunctionArgument <*> opt (pSymbol "," *> pFunctionArguments) []

pFunctionArgument :: SplParser AstFunctionArgument
pFunctionArgument = AstFunctionArgument <$> pSourceLocation <*> pType defaultBaseTypes <*> pIdentifier

pStatement :: SplParser AstStatement
pStatement =
      AstBlock <$ pSymbol "{" <*> many pStatement <* pSymbol "}"
  <|> AstIfThenElse <$> pSourceLocation <* pSymbol "if" <* pSymbol "(" <*> pExpr <* pSymbol ")" <*> pStatement <*>
        (pSymbol "else" *> pStatement  <<|> pure (AstBlock []))
  <|> AstWhile <$> pSourceLocation <* pSymbol "while" <* pSymbol "(" <*> pExpr <* pSymbol ")" <*> pStatement
  <|> AstAssignment <$> pSourceLocation <*> pIdentifier <* pSymbol "=" <*> pExpr <* pSymbol ";"
  <|> (     AstReturn <$> pSourceLocation <* pSymbol "return" <*> opt (Just <$> pExpr) Nothing <* pSymbol ";"
       <<|> AstFunctionCallStmt <$> pFunctionCall <* pSymbol ";"
      )

pExpr :: SplParser AstExpr
pExpr = pChainr (mkOp ":") $ foldr pChainl pBaseExpr pBinOps
  where
    pBinOps = map (foldr (<|>) pFail) $ map (map mkOp) binOps

mkOp :: String -> SplParser (AstExpr -> AstExpr -> AstExpr)
mkOp op = AstBinOp <$> pSourceLocation <*> pSymbol op

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

pBaseExpr :: SplParser AstExpr
pBaseExpr =
      mkBoolOrIdentifier <$> pSourceLocation <*> pIdentifier
  <|> AstUnaryOp <$> pSourceLocation <*> pSymbol "!" <*> pBaseExpr
  <|> AstInteger <$> pSourceLocation <*> pNatural
  <|> AstFunctionCallExpr <$> pFunctionCall
  <|> AstEmptyList <$> pSourceLocation <* pSymbol "[" <* pSymbol "]"
  <|> addLength 1
      ( do
        loc <- pSourceLocation
        _ <- pSymbol "("
        e <- pExpr
        pParenthesizedExpression loc e
      )
  <|> addLength 1
      ( do
        loc <- pSourceLocation
        _ <- pSymbol "-"
        pNegatedExpression loc
      )
  <|> AstRecord <$> pSourceLocation <* pSymbol "{" <*> pListSep (pSymbol ",") pRecordField <* pSymbol "}"
  <?> "Expression"

pRecordField :: SplParser AstRecordField
pRecordField = AstRecordField <$> pSourceLocation <*> pType defaultBaseTypes <*> pIdentifier <* pSymbol "=" <*> pExpr

pParenthesizedExpression :: AstMeta -> AstExpr -> SplParser AstExpr
pParenthesizedExpression loc e =
      pSymbol ")" *> pure e
  <|> pSymbol "," *> (AstTuple loc e <$> pExpr) <* pSymbol ")"

pIdentifier :: SplParser String
pIdentifier = lexeme ((:) <$> pLetter <*> many (pLetter <|> pDigit <|> UU.pSym '_')) <?> "Identifier"

pFunctionCall :: SplParser AstFunctionCall
pFunctionCall = AstFunctionCall <$> pSourceLocation <*> pIdentifier <* pSymbol "(" <*> opt pActualParameters [] <* pSymbol ")"

pNegatedExpression :: AstMeta -> SplParser AstExpr
pNegatedExpression loc =
       (AstInteger loc) . negate <$> pNatural
  <<|> (AstUnaryOp loc "-") <$> pBaseExpr

pActualParameters :: SplParser [AstExpr]
pActualParameters = (:) <$> pExpr <*> opt (pSymbol "," *> pActualParameters) []

booleanConstants :: [String]
booleanConstants = ["True", "False"]

mkBoolOrIdentifier :: AstMeta -> String -> AstExpr
mkBoolOrIdentifier m s =
  if s `elem` booleanConstants
    then AstBoolean m (s == "True")
    else AstIdentifier m s

pNatural :: SplParser Integer
pNatural = lexeme $
  pChainl (pure $ \num digit -> num * 10 + digit) ((\c -> toInteger (ord c - ord '0')) <$> pDigit)

pSymbol :: String -> SplParser String
pSymbol = lexeme . UU.pToken

lexeme :: SplParserTrafo a a
lexeme p = p <* many (pSpace <<|> pComment)

pSpace :: SplParser ()
pSpace = () <$ pAnySym " \r\n\t" <?> "Whitespace"

pComment :: SplParser ()
pComment = pLineComment <|> pBlockComment <?> "Comment"

pLineComment :: SplParser ()
pLineComment = () <$ UU.pToken "//" <* UU.pMunch (/= '\n') <* UU.pSym '\n'

pBlockComment :: SplParser ()
pBlockComment = () <$ UU.pToken "/*" <* pCrapUntilBlockCommentEnd

pCrapUntilBlockCommentEnd :: SplParser ()
pCrapUntilBlockCommentEnd =
  () <$ UU.pMunch (/= '*') <* -- eat everything that's not a star
    (() <$ UU.pToken "*/" <<|> -- if we get a comment-end delimiter, we're done
     () <$ UU.pSym '*' <* pCrapUntilBlockCommentEnd -- otherwise, eat the star and continue
    )
