module SplType where

import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map

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
 | SplForall [String] SplType
 | SplRecordType Row
 deriving (Eq)

data Row
 = SplFixedRow (Map String SplType)
 | SplVariableRow String (Map String SplType)
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
prettyprintType (SplTypeVariable v) = v
prettyprintType (SplTupleType x y) = "(" ++ prettyprintType x ++ ", " ++ prettyprintType y ++ ")"
prettyprintType (SplListType x) = "[" ++ prettyprintType x ++ "]"
prettyprintType (SplFunctionType argTypes returnType) = "(" ++ concat (intersperse " " (map prettyprintType argTypes)) ++ " -> " ++ prettyprintType returnType ++ ")"
prettyprintType (SplForall vars t) = "forall " ++ (concat $ intersperse " " vars) ++ ", " ++ prettyprintType t
prettyprintType (SplRecordType row) =
  "{" ++ prettyprintRow row ++ "}"

prettyprintRow :: Row -> String
prettyprintRow (SplFixedRow fields) = prettyprintFields fields
prettyprintRow (SplVariableRow var fields) = var ++ " | " ++ prettyprintFields fields

prettyprintFields :: (Map String SplType) -> String
prettyprintFields fields =
  (concat $ intersperse ", " $ map (\(label, typ) -> prettyprintType typ ++ " " ++ label) (Map.assocs fields))
