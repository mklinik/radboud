-- Data structures which form the Abstract Syntax Tree
module Ast where

import Text.ParserCombinators.UU.BasicInstances as UU

-- Meta information for ast nodes
data AstMeta = AstMeta
  { sourceLocation :: UU.LineColPos
  }
  deriving (Show)

instance Eq AstMeta where
  (==) _ _ = True

emptyMeta :: AstMeta
emptyMeta = AstMeta { sourceLocation = UU.LineColPos 0 0 0 }

data AstProgram = AstProgram [AstDeclaration]
  deriving (Show, Eq)

data AstDeclaration
  = AstVarDeclaration AstMeta AstType String AstExpr
  | AstFunDeclaration AstMeta AstType String [AstFunctionArgument] [AstDeclaration] [AstStatement]
  deriving (Show, Eq)

data AstType = BaseType String | TupleType AstType AstType | ListType AstType | PolymorphicType String
  deriving (Show, Eq)

data AstExpr
  = AstIdentifier AstMeta String
  | AstInteger AstMeta Integer
  | AstBoolean AstMeta Bool
  | AstTuple AstMeta AstExpr AstExpr
  | AstEmptyList AstMeta
  | AstBinOp AstMeta String AstExpr AstExpr
  | AstUnaryOp AstMeta String AstExpr
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

data AstFunctionCall = AstFunctionCall AstMeta String [AstExpr]
  deriving (Show, Eq)

data AstFunctionArgument = AstFunctionArgument AstType String
  deriving (Show, Eq)
