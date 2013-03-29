module Typechecker where

import Data.List (intersperse)
import qualified Data.Map as Map
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM)

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
prettyprintType (SplListType x) = "[" ++ prettyprintType x ++ "]"
prettyprintType (SplFunctionType argTypes returnType) = "(" ++ concat (intersperse " " (map prettyprintType argTypes)) ++ " -> " ++ prettyprintType returnType ++ ")"

type TypecheckState = (Integer, Environment)
type Typecheck a = EitherT CompileError (State TypecheckState) a

fresh :: Typecheck SplType
fresh = do
  (i, env) <- lift get
  lift $ put (i+1, env)
  return $ SplTypeVariable ("{" ++ show i ++ "}")

type Unifier = String -> SplType

type Environment = (Map.Map String SplType, [Map.Map String SplType])

data CompileError
  = TypeError SplType SplType AstMeta
  | UnknownIdentifier String AstMeta
  | InternalError String

instance Show CompileError where
  show (TypeError expected got meta) =
    "Couldn't match expected type `" ++ show expected
    ++ "' with actual type `" ++ show got ++ "' "
    ++ position meta
  show (UnknownIdentifier ident meta) =
    "Unknown identifier `" ++ ident ++ "' " ++ position meta
  show (InternalError x) = "Internal error `" ++ x ++ "'" -- should never happen, but you know...

position :: AstMeta -> String
position meta = "at position " ++ show (sourceLocation meta)

emptyEnvironment :: Environment
emptyEnvironment = (Map.empty, [Map.empty])

envLookup :: String -> AstMeta -> Typecheck SplType
envLookup ident meta = do
  (_, (globals, locals:_)) <- lift get
  case Map.lookup ident locals of
    (Just t) -> right t
    Nothing  -> case Map.lookup ident globals of
      (Just t) -> right t
      Nothing  -> left $ UnknownIdentifier ident meta

envAddGlobal :: String -> SplType -> Typecheck ()
envAddGlobal ident t = do
  (i, (globals, locals)) <- lift get
  lift $ put (i, (Map.insert ident t globals, locals))

-- envAdd :: String -> SplType -> Environment -> Typecheck Environment
-- envAdd name value (globals, l:locals) = right (globals, (Map.insert name value l):locals)
-- envAdd _ _ _ = left $ InternalError "envAdd"

-- envAddDeclaration :: (String -> SplType -> Typecheck Environment) -> AstDeclaration -> Typecheck Environment
-- envAddDeclaration doAdd (AstVarDeclaration _ typ ident expression) =


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

type Constraints = [(SplType, SplType)]

noConstraints :: Constraints
noConstraints = []

astType2splType :: AstType -> Typecheck SplType
astType2splType (BaseType _ "Bool") = return (SplBaseType BaseTypeBool)
astType2splType (BaseType _ "Int") = return (SplBaseType BaseTypeInt)
astType2splType (BaseType _ "Void") = return (SplBaseType BaseTypeVoid)
astType2splType (BaseType _ _) = left $ InternalError "astType2splType: non-base types are always type variables"
astType2splType (TupleType _ a b) = do
  a_ <- astType2splType a
  b_ <- astType2splType b
  return $ SplTupleType a_ b_
astType2splType (ListType _ a) = astType2splType a >>= return . SplListType
astType2splType (PolymorphicType _ a) = return $ SplTypeVariable a

class InferType a where
  inferType :: a -> Typecheck (SplType, Constraints)

instance InferType AstProgram where
  inferType (AstProgram decls) =
    foldM (\(_, c1) d -> do
      (t, c2) <- inferType d
      return (t, c1 ++ c2)
      ) (SplBaseType BaseTypeVoid, noConstraints) decls

instance InferType AstDeclaration where
  inferType (AstVarDeclaration _ astType name expr) = do
    (exprType, exprConstraints) <- inferType expr
    a <- case astType of
      -- yes, we are discarding the type variable here!
      -- TODO: the same type variables occuring in type expressions must get the same fresh type variables
      (PolymorphicType _ _) -> fresh
      _ -> astType2splType astType
    envAddGlobal name a
    return (a, (a,exprType):exprConstraints)

instance InferType AstExpr where
  inferType (AstIdentifier meta x) = do
    t <- envLookup x meta
    return (t, noConstraints)

  inferType (AstBoolean meta _) = return (SplBaseType BaseTypeBool, noConstraints)
  inferType (AstInteger meta _) = return (SplBaseType BaseTypeInt, noConstraints)

  inferType (AstFunctionCallExpr (AstFunctionCall meta f (actualArg:_))) = do
    ft <- envLookup f meta
    (actualArgType, c2) <- inferType actualArg
    x <- fresh
    return (x, (ft, SplFunctionType [actualArgType] x):c2)


initializeEnvironment :: Typecheck ()
initializeEnvironment = sequence_
  [ do
    a <- fresh
    envAddGlobal "print" (SplFunctionType [a] (SplBaseType BaseTypeVoid))

  , do
    a <- fresh
    b <- fresh
    envAddGlobal "fst" (SplFunctionType [SplTupleType a b] a)

  , do
    a <- fresh
    b <- fresh
    envAddGlobal "snd" (SplFunctionType [SplTupleType a b] b)

  , do
    a <- fresh
    envAddGlobal "head" (SplFunctionType [SplListType a] a)

  , do
    a <- fresh
    envAddGlobal "tail" (SplFunctionType [SplListType a] a)

  , do
    a <- fresh
    envAddGlobal "isEmpty" (SplFunctionType [SplListType a] (SplBaseType BaseTypeBool))
  ]



runTypecheck :: (Typecheck a) -> (Either CompileError a, TypecheckState)
runTypecheck t = runState (runEitherT (initializeEnvironment >> t)) (0, emptyEnvironment)
