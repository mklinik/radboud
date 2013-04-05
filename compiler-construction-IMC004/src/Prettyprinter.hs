module Prettyprinter where

import Data.List (intersperse)

import Ast

prettyprint :: Prettyprint a => a -> String
prettyprint a = pp 0 a ""

indent :: Prettyprint a => Int -> a -> (String -> String)
indent level a = pp (level + 2) a

class Prettyprint a where
  pp :: Int -> a -> (String -> String)

instance Prettyprint AstProgram where
  pp _ (AstProgram []) = id
  pp level (AstProgram (d:decls)) = undefined -- \s -> pp level d "\n" ++ pp level (AstProgram decls) s

instance Prettyprint AstDeclaration where
  pp level (AstVarDeclaration _ typ ident expr) = \s -> replicate level ' ' ++ pp level typ " " ++ ident ++ " = " ++ pp level expr ";\n" ++ s
  pp level (AstFunDeclaration _ typ ident args decls stmts) = \s ->
      pp level typ " " ++ ident ++ "(" ++ arguments ++ ")" ++ "\n{\n" ++ declarations ++ statements ++ "}\n" ++ s
    where
      arguments = (foldl (.) id $ intersperse (", " ++) $ map (pp level) args) ""
      declarations = (foldl (.) id $ map (indent level) decls) ""
      statements = (foldl (.) id $ map (indent level) stmts) ""


instance Prettyprint AstType where
  pp _ (BaseType _ t) = (t ++)
  pp _ (PolymorphicType _ t) = (t ++)
  pp level (TupleType _ a b) = \s -> "(" ++ pp level a ", " ++ pp level b ")" ++ s
  pp level (ListType _ t) = \s -> "[" ++ pp level t "]" ++ s

instance Prettyprint AstFunctionArgument where
  pp level (AstFunctionArgument _ typ ident) = \s -> pp level typ " " ++ ident ++ s

instance Prettyprint AstExpr where
  pp _ (AstIdentifier _ ident) = (ident ++)
  pp _ (AstInteger _ i) = (show i ++)
  pp _ (AstBoolean _ b) = (show b ++)
  pp level (AstTuple _ a b) = \s -> "(" ++ pp level a ", " ++ pp level b ")" ++ s
  pp _ (AstEmptyList _) = ("[]" ++)
  pp level (AstBinOp _ op l r) = \s -> "(" ++ pp level l (" " ++ op ++ " ") ++ pp level r ")" ++ s
  pp level (AstUnaryOp _ op e) = \s -> op ++ pp level e s
  pp level (AstFunctionCallExpr fun) = pp level fun

instance Prettyprint AstFunctionCall where
  pp level (AstFunctionCall _ ident args) = \s -> ident ++ "(" ++ foldr ($) ")" (intersperse (", "++) (map (pp level) args)) ++ s

instance Prettyprint AstStatement where
  pp level (AstReturn _ mExpr) = \s -> replicate level ' ' ++ "return" ++ maybe ";\n" (\e -> " " ++ pp level e ";\n") mExpr ++ s
  pp level (AstBlock stmts) = \s -> replicate level ' ' ++ "{\n" ++ statements ++ replicate level ' ' ++ "}\n" ++ s
    where
      statements = (foldl (.) id $ map (indent level) stmts) ""
  pp level (AstAssignment _ ident expr) = \s -> replicate level ' ' ++ ident ++ " = " ++ pp level expr ";\n" ++ s
  pp level (AstWhile _ condition body) = \s -> replicate level ' ' ++ "while( " ++ pp level condition " )\n" ++ indent_ level body "" ++ s
  pp level (AstIfThenElse _ condition thenStmt elseStmt) = \s -> replicate level ' ' ++ "if( " ++ pp level condition " )\n" ++
      indent_ level thenStmt "" ++ replicate level ' ' ++ "else\n" ++ indent_ level elseStmt "" ++ s
  pp level (AstFunctionCallStmt fun) = \s -> replicate level ' ' ++ pp level fun ";\n" ++ s

indent_ :: Int -> AstStatement -> (String -> String)
indent_ level a@(AstBlock _) = pp level a -- Blocks don't need to be indented ...
indent_ level a = indent level a          -- ... but everything else does.
