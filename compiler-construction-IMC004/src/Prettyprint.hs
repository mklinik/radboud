module Prettyprint where

import Data.List (intersperse)

import Ast

prettyprint :: Prettyprint a => a -> String
prettyprint a = pp 0 a "<<EOF>>"

indent :: Prettyprint a => Int -> a -> (String -> String)
indent level a = pp (level + 2) a

unindent :: Prettyprint a => Int -> a -> (String -> String)
unindent level a = pp (level - 2) a

class Prettyprint a where
  pp :: Int -> a -> (String -> String)

instance Prettyprint AstProgram where
  pp level (AstProgram []) = id
  pp level (AstProgram (d:decls)) = \s -> pp level d "\n" ++ pp level (AstProgram decls) s

instance Prettyprint AstDeclaration where
  pp level (AstVarDeclaration typ ident expr) = \s -> replicate level ' ' ++ pp level typ " " ++ ident ++ " = " ++ pp level expr ";\n" ++ s
  pp level (AstFunDeclaration typ ident args decls stmts) = \s ->
      pp level typ " " ++ ident ++ "(" ++ arguments ++ ")" ++ "\n{\n" ++ declarations ++ "}\n" ++ s
    where
      arguments = (foldl (.) id $ intersperse (", " ++) $ map (pp level) args) ""
      declarations = (foldl (.) id $ map (indent level) decls) ""


instance Prettyprint AstType where
  pp _ (BaseType t) = (t ++)
  pp _ (PolymorphicType t) = (t ++)
  pp level (TupleType a b) = \s -> "(" ++ pp level a ", " ++ pp level b ")" ++ s
  pp level (ListType t) = \s -> "[" ++ pp level t "]" ++ s

instance Prettyprint AstFunctionArgument where
  pp level (AstFunctionArgument typ ident) = \s -> pp level typ " " ++ ident ++ s

instance Prettyprint AstExpr where
  pp _ (AstIdentifier ident) = (ident ++)
  pp _ (AstInteger i) = (show i ++)
  pp _ (AstBoolean b) = (show b ++)
  pp level (AstTuple a b) = \s -> "(" ++ pp level a ", " ++ pp level b ")" ++ s
  pp level (AstEmptyList) = ("[]" ++)
