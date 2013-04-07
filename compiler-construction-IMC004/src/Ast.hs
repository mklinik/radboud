-- Data structures which form the Abstract Syntax Tree
module Ast where

import Text.ParserCombinators.UU.BasicInstances as UU
import SplType

-- Meta information for ast nodes
data AstMeta = AstMeta
  { sourceLocation :: UU.LineColPos
  , inferredType :: Maybe SplType
  }
  deriving (Show)

instance Eq AstMeta where
  (==) _ _ = True

emptyMeta :: AstMeta
emptyMeta = AstMeta
  { sourceLocation = UU.LineColPos 0 0 0
  , inferredType = Nothing
  }

data AstProgram = AstProgram [[AstDeclaration]]
  deriving (Show, Eq)

data AstDeclaration
  = AstVarDeclaration AstMeta AstType String AstExpr
  | AstFunDeclaration AstMeta AstType String [AstFunctionArgument] [AstDeclaration] [AstStatement]
  deriving (Show, Eq)

data AstType
  = BaseType AstMeta String
  | TupleType AstMeta AstType AstType
  | ListType AstMeta AstType
  | PolymorphicType AstMeta String
  | FunctionType [AstType] AstType -- there is no parser for this yet, but it is useful in the typechecker
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
  = AstReturn AstMeta (Maybe AstExpr)
  | AstBlock [AstStatement]
  | AstAssignment AstMeta String AstExpr
  | AstWhile AstMeta AstExpr AstStatement
  | AstIfThenElse AstMeta AstExpr AstStatement AstStatement
  | AstFunctionCallStmt AstFunctionCall
  deriving (Show, Eq)

data AstFunctionCall = AstFunctionCall AstMeta String [AstExpr]
  deriving (Show, Eq)

data AstFunctionArgument = AstFunctionArgument AstMeta AstType String
  deriving (Show, Eq)
