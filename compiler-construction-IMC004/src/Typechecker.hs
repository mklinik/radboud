module Typechecker where

import Data.List (intersperse)

import Ast

data SplBaseType
 = BaseTypeInt
 | BaseTypeBool
 | BaseTypeVoid
 deriving (Eq)

data SplType
 = SplBaseType SplBaseType
 | SplTypeVariable String
 | SplTupleType SplType SplType
 | SplListType SplType
 | SplFunctionType [SplType] SplType
 deriving (Eq)


instance Show SplType where
  show t = prettyprintType t

prettyprintType :: SplType -> String
prettyprintType (SplBaseType BaseTypeInt) = "Int"
prettyprintType (SplBaseType BaseTypeBool) = "Bool"
prettyprintType (SplBaseType BaseTypeVoid) = "Void"
prettyprintType (SplTypeVariable v) = v
prettyprintType (SplTupleType x y) = "(" ++ prettyprintType x ++ ", " ++ prettyprintType y ++ ")"
prettyprintType (SplListType x) = "[" ++ prettyprintType x ++ ")"
prettyprintType (SplFunctionType argTypes returnType) = "(" ++ concat (intersperse " " (map prettyprintType argTypes)) ++ " -> " ++ prettyprintType returnType ++ ")"

type Unifier = String -> SplType
type Environment = String -> SplType
data CompileError
  = TypeError SplType SplType AstMeta

instance Show CompileError where
  show (TypeError expected got meta) =
    "Couldn't match expected type `" ++ show expected
    ++ "' with actual type `" ++ show got
    ++ "' at position " ++ show (sourceLocation meta)
  

emptyEnvironment :: Environment
emptyEnvironment = undefined

emptyUnifier :: Unifier
emptyUnifier = undefined

typeVars :: SplType -> [String]
typeVars (SplBaseType _) = []
typeVars (SplTypeVariable v) = [v]
typeVars (SplTupleType x y) = typeVars x ++ typeVars y
typeVars (SplListType x) = typeVars x
typeVars (SplFunctionType argTypes returnType) = foldl (\accum argType -> typeVars argType ++ accum) (typeVars returnType) argTypes

substitute :: Unifier -> SplType -> SplType
substitute u (SplTypeVariable v) = u v
substitute _ t@(SplBaseType _) = t
substitute u (SplTupleType x y) = SplTupleType (substitute u x) (substitute u y)
substitute u (SplListType x) = SplListType (substitute u x)
substitute u (SplFunctionType argTypes returnType) = SplFunctionType (map (substitute u) argTypes) (substitute u returnType)

type Typecheck a = Either CompileError a

unify :: SplType -> SplType -> AstMeta -> Unifier -> Typecheck Unifier
unify (SplTypeVariable v) t@(SplBaseType _) _ u = Right $ \x -> if x == v then t else u x
-- The catch-all clause. All other combinations are not unifiable
unify expected got meta _ = Left $ TypeError expected got meta

inferType :: Environment -> AstExpr -> SplType -> Typecheck SplType
inferType _ (AstInteger meta _) sigma = do
  u <- unify sigma (SplBaseType BaseTypeInt) meta emptyUnifier
  Right $ substitute u sigma

inferType _ (AstBoolean meta _) sigma = do
  u <- unify sigma (SplBaseType BaseTypeBool) meta emptyUnifier
  Right $ substitute u sigma

