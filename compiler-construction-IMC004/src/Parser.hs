{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

-- The parser transforms the input text into an AST
module Parser where

import Text.ParserCombinators.UU
import qualified Text.ParserCombinators.UU.BasicInstances as PC
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils hiding (runParser, pNatural, lexeme, pSymbol)
import Text.Printf (printf)
import Data.Char (ord)

import Ast

pProgram :: Parser AstProgram
pProgram = AstProgram <$ lexeme (pure ()) <*> some pDeclaration

pDeclaration :: Parser AstDeclaration
pDeclaration = pVarDeclaration <|> pFunDeclaration

pVarDeclaration :: Parser AstDeclaration
pVarDeclaration = AstVarDeclaration <$> pType defaultBaseTypes <*> pIdentifier <* pSymbol "=" <*> pExpr <* pSymbol ";"

defaultBaseTypes :: [String]
defaultBaseTypes = ["Int", "Bool"]

pFunDeclaration :: Parser AstDeclaration
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

pIdentifier :: Parser String
pIdentifier = lexeme ((:) <$> pLetter <*> many (pLetter <|> pDigit <|> PC.pSym '_'))

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
lexeme p = p <* many (pSpace <<|> pLineComment <|> pBlockComment)

pSpace :: Parser ()
pSpace = () <$ pAnySym " \r\n\t"

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

runParser :: String -> PC.Parser a -> String -> a
runParser inputName parser input | (a,b) <- execParser parser input =
    if null b
    then a
    else error (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError input b))
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
        pruneError :: String -> [PC.Error PC.LineColPos] -> String
        pruneError _ [] = ""
        pruneError _ (PC.DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
        pruneError s (PC.Inserted _ position expr : _) = prettyError s expr position
        pruneError s (PC.Deleted  _ position expr : _) = prettyError s expr position
        pruneError _ (PC.Replaced _ _ _ _ : _) = error "pruneError: unhandled case `Replaced`" -- don't know what to do, don't care (for now)
        prettyError :: String -> [String] -> PC.LineColPos -> String
        prettyError s expr loc@(PC.LineColPos _ _ pos) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (PC.show_expecting loc expr)
                                                           (show loc)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - pos) ' ' ++ (take 71 $ drop (pos - 30) s')
