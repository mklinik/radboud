
-- Data structures which form the Abstract Syntax Tree
module Ast where

data AstProgram = AstProgram [AstDeclaration]
  deriving (Show, Eq)

data AstDeclaration
  = AstVarDeclaration AstType String AstExpr
  | AstFunDeclaration AstType String [AstFunctionArgument] [AstDeclaration] [AstStatement]
  deriving (Show, Eq)

data AstType = BaseType String | TupleType AstType AstType | ListType AstType | PolymorphicType String
  deriving (Show, Eq)

data AstExpr
  = AstIdentifier String
  | AstInteger Integer
  | AstBoolean Bool
  | AstTuple AstExpr AstExpr
  | AstEmptyList
  | AstBinOp String AstExpr AstExpr
  | AstUnaryOp String AstExpr
  deriving (Show, Eq)

data AstStatement
  = AstReturn (Maybe AstExpr)
  | AstBlock [AstStatement]
  | AstAssignment String AstExpr
  | AstWhile AstExpr AstStatement
  | AstIfThenElse AstExpr AstStatement AstStatement
  deriving (Show, Eq)

data AstFunctionArgument = AstFunctionArgument AstType String
  deriving (Show, Eq)
