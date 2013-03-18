-- Data structures which form the Abstract Syntax Tree
module Ast where

import Text.ParserCombinators.UU.BasicInstances as PC

-- Meta information for ast nodes
data AstMeta = AstMeta
  { sourceLocation :: PC.LineColPos
  }
  deriving (Show)

instance Eq AstMeta where
  (==) _ _ = True

emptyMeta :: AstMeta
emptyMeta = AstMeta { sourceLocation = PC.LineColPos 0 0 0 }

data AstProgram = AstProgram [AstDeclaration]
  deriving (Show, Eq)

data AstDeclaration
  = AstVarDeclaration AstMeta AstType String AstExpr
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
  | AstFunctionCallExpr AstFunctionCall
  deriving (Show, Eq)

data AstStatement
  = AstReturn (Maybe AstExpr)
  | AstBlock [AstStatement]
  | AstAssignment String AstExpr
  | AstWhile AstExpr AstStatement
  | AstIfThenElse AstExpr AstStatement AstStatement
  | AstFunctionCallStmt AstFunctionCall
  deriving (Show, Eq)

data AstFunctionCall = AstFunctionCall String [AstExpr]
  deriving (Show, Eq)

data AstFunctionArgument = AstFunctionArgument AstType String
  deriving (Show, Eq)
