{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

-- The parser transforms the input text into an AST
module Parser where

import Text.ParserCombinators.UU
import qualified Text.ParserCombinators.UU.BasicInstances as PC
import           Text.ParserCombinators.UU.BasicInstances (Parser)
-- import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils hiding (runParser, pInteger, lexeme, pSymbol)
import Text.Printf (printf)
import Data.Char (ord)

import Ast

pProgram :: Parser AstProgram
pProgram = AstProgram <$ lexeme (pure ()) <*> some pDeclaration

pDeclaration :: Parser AstDeclaration
pDeclaration = pVarDeclaration -- <|> pFunDeclaration

pVarDeclaration :: Parser AstDeclaration
pVarDeclaration = AstVarDeclaration <$> pType <*> pIdentifier <* pSymbol "=" <*> pExpr <* pSymbol ";"

-- pFunDeclaration = undefined

pType :: Parser AstType
pType =
      mkBaseTypeOrIdentifier <$> pIdentifier
  <|> TupleType <$ pSymbol "(" <*> pType <* pSymbol "," <*> pType <* pSymbol ")"
  <|> ListType <$ pSymbol "[" <*> pType <* pSymbol "]"

baseTypes :: [String]
baseTypes = ["Int", "Bool"]

mkBaseTypeOrIdentifier :: String -> AstType
mkBaseTypeOrIdentifier s =
  if s `elem` baseTypes
    then BaseType s
    else PolymorphicType s

pIdentifier :: Parser String
pIdentifier = lexeme ((:) <$> pLetter <*> many (pLetter <|> pDigit <|> PC.pSym '_'))

pExpr :: Parser AstExpr
pExpr =
      AstInteger <$> pInteger
  <|> mkBoolOrIdentifier <$> pIdentifier
  <|> pSymbol "(" *> pExpr <* pSymbol ")"
  <|> AstTuple <$ pSymbol "(" <*> pExpr <* pSymbol "," <*> pExpr <* pSymbol ")"
  <|> AstEmptyList <$ pSymbol "[" <* pSymbol "]"

booleanConstants :: [String]
booleanConstants = ["True", "False"]

mkBoolOrIdentifier :: String -> AstExpr
mkBoolOrIdentifier s =
  if s `elem` booleanConstants
    then AstBoolean (s == "True")
    else AstIdentifier s

pInteger :: Parser Integer
pInteger = lexeme $ opt (negate <$ pSymbol "-") id <*>
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
pCrapUntilBlockCommentEnd = () <$
  PC.pMunch (/= '*') <* -- eat everything that's not a star
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
