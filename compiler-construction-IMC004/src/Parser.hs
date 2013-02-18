{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

-- The parser transforms the input text into an AST
module Parser where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils hiding (runParser)
import Text.Printf (printf)

import Ast

pProg :: Parser AstProg
pProg = AstProg <$> some pDecl

pDecl = pVarDecl -- <|> pFunDecl

pVarDecl = AstVarDecl <$> pType <*> pIdentifier <* pSymbol "=" <* pExpr <* pSymbol ";"

-- pFunDecl = undefined

pType = lexeme $
      BaseType <$> pToken "Int"
  <|> BaseType <$> pToken "Bool"
  <|> TupleType <$ pSymbol "(" <*> pType <* pSymbol "," <*> pType <* pSymbol ")"
  <|> ListType <$ pSymbol "[" <*> pType <* pSymbol "]"

pIdentifier = lexeme $ many pLetter

pExpr = lexeme $ many pDigit


runParser :: String -> Parser a -> String -> a
runParser inputName p s | (a,b) <- execParser p s =
    if null b
    then a
    else error (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError s b))
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
        pruneError :: String -> [Error LineColPos] -> String
        pruneError _ [] = ""
        pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
        pruneError s (Inserted _ pos exp : _) = prettyError s exp pos
        pruneError s (Deleted  _ pos exp : _) = prettyError s exp pos
        prettyError :: String -> [String] -> LineColPos -> String
        prettyError s exp p@(LineColPos line c abs) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (show_expecting p exp)
                                                           (show p)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - c) ' ' ++ (take 71 $ drop (c - 30) s')
