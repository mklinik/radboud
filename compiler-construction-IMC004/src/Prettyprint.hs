module Prettyprint where

import Data.List (intersperse)

import Ast

prettyprint :: Prettyprint a => a -> String
prettyprint a = pp 0 a ""

indent :: Prettyprint a => Int -> a -> (String -> String)
indent level a = pp (level + 2) a

unindent :: Prettyprint a => Int -> a -> (String -> String)
unindent level a = pp (level - 2) a

class Prettyprint a where
  pp :: Int -> a -> (String -> String)

instance Prettyprint AstProgram where
  pp _ (AstProgram []) = id
  pp level (AstProgram (d:decls)) = \s -> pp level d "\n" ++ pp level (AstProgram decls) s

instance Prettyprint AstDeclaration where
  pp level (AstVarDeclaration typ ident expr) = \s -> replicate level ' ' ++ pp level typ " " ++ ident ++ " = " ++ pp level expr ";\n" ++ s
  pp level (AstFunDeclaration typ ident args decls stmts) = \s ->
      pp level typ " " ++ ident ++ "(" ++ arguments ++ ")" ++ "\n{\n" ++ declarations ++ statements ++ "}\n" ++ s
    where
      arguments = (foldl (.) id $ intersperse (", " ++) $ map (pp level) args) ""
      declarations = (foldl (.) id $ map (indent level) decls) ""
      statements = (foldl (.) id $ map (indent level) stmts) ""


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
  pp _ (AstEmptyList) = ("[]" ++)

instance Prettyprint AstStatement where
  pp level (AstReturn mExpr) = \s -> replicate level ' ' ++ "return" ++ maybe ";\n" (\e -> " " ++ pp level e ";\n") mExpr ++ s
  pp level (AstBlock stmts) = \s -> replicate level ' ' ++ "{\n" ++ statements ++ replicate level ' ' ++ "}\n" ++ s
    where
      statements = (foldl (.) id $ map (indent level) stmts) ""
  pp level (AstAssignment ident expr) = \s -> replicate level ' ' ++ ident ++ " = " ++ pp level expr ";\n" ++ s
  pp level (AstWhile condition body) = \s -> replicate level ' ' ++ "while( " ++ pp level condition " )\n" ++ body_ ++ s
    where
      body_ = case body of
        AstBlock _ -> pp level body ""     -- Blocks don't need to be indented ...
        _          -> indent level body "" -- ... but single statements do.
  pp level (AstIfThenElse condition thenStmt elseStmt) = \s -> replicate level ' ' ++ "if( " ++ pp level condition " )\n" ++
      indent level thenStmt "" ++ replicate level ' ' ++ "else\n" ++ indent level elseStmt "" ++ s
