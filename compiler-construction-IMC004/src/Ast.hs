
-- Data structures which form the Abstract Syntax Tree
module Ast where

import Data.List (intersperse)

data AstProgram = AstProgram [AstDeclaration]
  deriving (Eq)

data AstDeclaration
  = AstVarDeclaration AstType String AstExpr
  | AstFunDeclaration AstType String [AstFunctionArgument] [AstDeclaration] [AstStatement]
  deriving (Eq)

data AstType = BaseType String | TupleType AstType AstType | ListType AstType | PolymorphicType String
  deriving (Eq)

data AstExpr
  = AstIdentifier String
  | AstInteger Integer
  | AstBoolean Bool
  | AstTuple AstExpr AstExpr
  | AstEmptyList
  deriving (Eq)

data AstStatement
  = AstReturn (Maybe AstExpr)
  | AstBlock [AstStatement]
  | AstAssignment String AstExpr
  | AstWhile AstExpr AstStatement
  | AstIfThenElse AstExpr AstStatement AstStatement
  deriving (Eq)

data AstFunctionArgument = AstFunctionArgument AstType String
  deriving (Eq)

instance Show AstProgram where
  show (AstProgram decls) = concat $ intersperse "\n" $ map show decls

instance Show AstDeclaration where
  show (AstFunDeclaration typ ident args vars stmts) = concat $ intersperse " "
    [show typ, "#" ++ ident, "("]
      ++ arguments
      ++ [")", "{"]
      ++ variables
      ++ ["\n  "]
      ++ statements
      ++ ["}"]
    where
      arguments = intersperse ", " $ map show args
      variables = intersperse " " $ map show vars
      statements = intersperse "\n  " $ map show stmts
  show (AstVarDeclaration typ ident expr) = concat $ intersperse " " [show typ, "#" ++ ident, "=", show expr, ";"]

instance Show AstType where
  show (BaseType t) = t
  show (TupleType a b) = concat ["(", show a, ", ", show b, ")"]
  show (ListType t) = concat ["[", show t, "]"]
  show (PolymorphicType t) = "$" ++ t

instance Show AstExpr where
  show (AstIdentifier ident) = "#" ++ ident
  show (AstInteger i) = show i
  show (AstBoolean b) = "@" ++ show b
  show (AstTuple a b) = concat ["(", show a, ", ", show b, ")"]
  show (AstEmptyList) = "[]"

instance Show AstStatement where
  show (AstReturn mExpr) = "return " ++ (maybe "" show mExpr) ++ ";"
  show (AstBlock stmts) = "{ " ++ statements ++ "}"
    where
      statements = concat $ intersperse "\n  " $ map show stmts
  show (AstAssignment ident val) = concat $ intersperse " " [ident, "=", show val, ";"]
  show (AstWhile condition body) = concat $ intersperse " " ["while (", show condition, ")", show body]
  show (AstIfThenElse condition thenStmt elseStmt) = concat $ intersperse " " ["if (", show condition, ")", show thenStmt, "else", show elseStmt]

instance Show AstFunctionArgument where
  show (AstFunctionArgument typ ident) = concat $ intersperse " " [show typ, ident]
