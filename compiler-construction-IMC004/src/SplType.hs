module SplType where

import Data.List (intersperse)

data SplBaseType
 = BaseTypeInt
 | BaseTypeBool
 | BaseTypeVoid
 | BaseTypeDummy String
 deriving (Eq)

data SplType
 = SplBaseType SplBaseType
 | SplTypeVariable String
 | SplTupleType SplType SplType
 | SplListType SplType
 | SplFunctionType [SplType] SplType
 | SplForall [String] SplType
 deriving (Eq)

-- for convenience
splTypeVoid, splTypeInt, splTypeBool :: SplType
splTypeVoid = SplBaseType BaseTypeVoid
splTypeInt = SplBaseType BaseTypeInt
splTypeBool = SplBaseType BaseTypeBool

instance Show SplType where
  show t = prettyprintType t

prettyprintType :: SplType -> String
prettyprintType (SplBaseType BaseTypeInt) = "Int"
prettyprintType (SplBaseType BaseTypeBool) = "Bool"
prettyprintType (SplBaseType BaseTypeVoid) = "Void"
prettyprintType (SplBaseType (BaseTypeDummy s)) = "?" ++ s
prettyprintType (SplTypeVariable v) = v
prettyprintType (SplTupleType x y) = "(" ++ prettyprintType x ++ ", " ++ prettyprintType y ++ ")"
prettyprintType (SplListType x) = "[" ++ prettyprintType x ++ "]"
prettyprintType (SplFunctionType argTypes returnType) = "(" ++ concat (intersperse " " (map prettyprintType argTypes)) ++ " -> " ++ prettyprintType returnType ++ ")"
prettyprintType (SplForall vars t) = "forall " ++ (concat $ intersperse " " vars) ++ ", " ++ prettyprintType t
