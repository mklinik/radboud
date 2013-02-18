
-- Data structures which form the Abstract Syntax Tree
module Ast where

import Data.List (intersperse)

data AstProg = AstProg [AstDecl]

data AstDecl = AstVarDecl AstType String AstExpr | AstFunDecl

data AstType = BaseType String | TupleType AstType AstType | ListType AstType | PolymorphicType String

data AstExpr
  = AstIdentifier String
  | AstInteger Integer
  | AstBoolean Bool

instance Show AstProg where
  show (AstProg decls) = concat $ intersperse "\n" $ map show decls

instance Show AstDecl where
  show AstFunDecl = "AstFunDecl"
  show (AstVarDecl typ ident expr) = concat $ intersperse " " [show typ, "#" ++ ident, "=", show expr, ";"]

instance Show AstType where
  show (BaseType t) = t
  show (TupleType a b) = concat ["(", show a, ", ", show b, ")"]
  show (ListType t) = concat ["[", show t, "]"]
  show (PolymorphicType t) = t

instance Show AstExpr where
  show (AstIdentifier ident) = "#" ++ ident
  show (AstInteger i) = show i
  show (AstBoolean b) = "@" ++ show b
