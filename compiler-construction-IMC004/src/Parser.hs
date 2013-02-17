{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}
module Parser where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils

data AstProg = AstProg [AstDecl]
  deriving (Show)
data AstDecl = AstVarDecl AstType String | AstFunDecl
  deriving (Show)
data AstType = BaseType String | TupleType AstType AstType | ListType AstType | PolymorphicType String
  deriving (Show)

pProg :: Parser AstProg
pProg = AstProg <$> some pDecl <* pEnd

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
