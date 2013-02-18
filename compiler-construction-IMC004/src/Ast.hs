
-- Data structures which form the Abstract Syntax Tree
module Ast where

import Data.List (intersperse)

data AstProg = AstProg [AstDecl]

data AstDecl = AstVarDecl AstType String | AstFunDecl

data AstType = BaseType String | TupleType AstType AstType | ListType AstType | PolymorphicType String

instance Show AstProg where
  show (AstProg decls) = concat $ intersperse "\n" $ map show decls

instance Show AstDecl where
  show AstFunDecl = "AstFunDecl"
  show (AstVarDecl typ id) = concat $ intersperse " " $ [show typ, id, "=", "...", ";"]

instance Show AstType where
  show (BaseType t) = t
  show (TupleType a b) = concat ["(", show a, ", ", show b, ")"]
  show (ListType t) = concat ["[", show t, "]"]
  show (PolymorphicType t) = t
