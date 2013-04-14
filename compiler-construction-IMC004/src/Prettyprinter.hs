{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Prettyprinter where

import Data.List (intersperse)

import Ast
import SplType
import Typechecker

prettyprint :: Prettyprint a => a -> String
prettyprint a = pp 0 a ""

indent :: Prettyprint a => Int -> a -> (String -> String)
indent level a = pp (level + 2) a

class Prettyprint a where
  pp :: Int -> a -> (String -> String)

instance Prettyprint AstProgram where
  pp _ (AstProgram []) = id
  pp level (AstProgram (d:decls)) = \s -> ds ++ pp level (AstProgram decls) s
    where
      ds = startToken ++ (foldl (.) id $ intersperse (middleToken ++) $ map (pp level) d) endToken
      (startToken,middleToken,endToken) = if length d > 1 then ("{", "", "}\n") else ("", "\n", "\n")

ppMaybeTyp :: Int -> AstDeclaration -> (String -> String)
ppMaybeTyp level (AstVarDeclaration meta typ _ _) =
  case fmap makeNiceAutoTypeVariables (inferredType meta) of
    Nothing -> pp level typ
    Just t  -> pp level t

ppMaybeTyp level (AstFunDeclaration meta typ name args _ _) = \s ->
  case fmap makeNiceAutoTypeVariables (inferredType meta) of
    Just (SplFunctionType argTypes returnType) ->
      "\n// " ++ name ++ " : " ++ pp level (inferredType meta) "\n" ++
        pp level returnType " " ++ name ++ "(" ++ argumentsNice argTypes ++ ")" ++ s
    _ -> pp level typ " " ++ name ++ "(" ++ arguments ++ ")" ++ s
  where
    arguments = (foldl (.) id $ intersperse (", " ++) $ map (pp level) args) ""
    argumentsNice argTypes = (foldl (.) id $ intersperse (", " ++) $ map (ppOneArg level) (zip args argTypes)) ""
    ppOneArg :: Int -> (AstFunctionArgument, SplType) -> (String -> String)
    ppOneArg lev (AstFunctionArgument _ _ argName, argType) = \s -> pp lev argType " " ++ argName ++ s

instance Prettyprint (Maybe SplType) where
  pp level Nothing = id
  pp level (Just t) = \s -> prettyprintType t ++ s

instance Prettyprint AstDeclaration where
  pp level ast@(AstVarDeclaration _ _ ident expr) = \s -> replicate level ' ' ++ ppMaybeTyp level ast " " ++ ident ++ " = " ++ pp level expr ";\n" ++ s
  pp level ast@(AstFunDeclaration _ _ _ _ decls stmts) = \s ->
    ppMaybeTyp level ast "\n{\n" ++ declarations ++ statements ++ "}\n" ++ s
    where
      declarations = (foldl (.) id $ map (indent level) decls) ""
      statements = (foldl (.) id $ map (indent level) stmts) ""


instance Prettyprint AstType where
  pp _ (BaseType _ t) = (t ++)
  pp _ (PolymorphicType _ t) = (t ++)
  pp level (TupleType _ a b) = \s -> "(" ++ pp level a ", " ++ pp level b ")" ++ s
  pp level (ListType _ t) = \s -> "[" ++ pp level t "]" ++ s

ppMaybeTypArg :: Int -> AstFunctionArgument -> (String -> String)
ppMaybeTypArg level (AstFunctionArgument meta typ _) =
  case inferredType meta of
    Nothing -> pp level typ
    Just  t -> pp level t

instance Prettyprint AstFunctionArgument where
  pp level ast@(AstFunctionArgument _ _ ident) = \s -> ppMaybeTypArg level ast " " ++ ident ++ s

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

instance Prettyprint SplType where
  pp _ t = \s -> prettyprintType t ++ s
