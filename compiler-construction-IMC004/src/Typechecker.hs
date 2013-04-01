module Typechecker where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM)

import Ast
import SplType
import CompileError

type Environment = (Map.Map String (SplType, Constraints), Map.Map String (SplType, Constraints))
type TypecheckState = (Integer, Environment)
type Typecheck a = EitherT CompileError (State TypecheckState) a

type Constraints = [(SplType, SplType)]

noConstraints :: Constraints
noConstraints = []

fresh :: Typecheck SplType
fresh = do
  (i, env) <- lift get
  lift $ put (i+1, env)
  return $ SplTypeVariable ("{" ++ show i ++ "}")

type Unifier = String -> SplType

emptyUnifier :: Unifier
emptyUnifier = undefined

emptyEnvironment :: Environment
emptyEnvironment = (Map.empty, Map.empty)

envLookup :: String -> AstMeta -> Typecheck (SplType, Constraints)
envLookup ident meta = do
  (_, (globals, locals)) <- lift get
  case Map.lookup ident locals of
    (Just t) -> right t
    Nothing  -> case Map.lookup ident globals of
      (Just t) -> right t
      Nothing  -> left $ UnknownIdentifier ident meta

envAddGlobal :: String -> SplType -> Constraints -> Typecheck ()
envAddGlobal ident t constraints = do
  (i, (globals, locals)) <- lift get
  lift $ put (i, (Map.insert ident (t, constraints) globals, locals))

-- envAdd :: String -> SplType -> Environment -> Typecheck Environment
-- envAdd name value (globals, l:locals) = right (globals, (Map.insert name value l):locals)
-- envAdd _ _ _ = left $ InternalError "envAdd"

-- envAddDeclaration :: (String -> SplType -> Typecheck Environment) -> AstDeclaration -> Typecheck Environment
-- envAddDeclaration doAdd (AstVarDeclaration _ typ ident expression) =

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


-- Makes fresh type variables for each type variable occuring in the given AstType.
-- The same type variables in the AstType get the same fresh type variables.
astFreshTypeVariables :: AstType -> Typecheck (Map.Map String SplType)
astFreshTypeVariables astType = do
  oldFreshs <- mapM oldFresh $ Set.toList $ astTypeVariables
  return $ foldl (flip $ uncurry Map.insert) Map.empty oldFreshs
  where
    -- Pairs the given type variable with a fresh one.
    oldFresh :: String -> Typecheck (String, SplType)
    oldFresh s = do
      a <- fresh
      return (s, a)
    -- Gets all type variables in an AstType.
    astTypeVariables :: Set.Set String
    astTypeVariables = astTypeVariables_ astType Set.empty

    astTypeVariables_ :: AstType -> Set.Set String -> Set.Set String
    astTypeVariables_ (BaseType _ _) s = s
    astTypeVariables_ (TupleType _ a b) s = astTypeVariables_ b $ astTypeVariables_ a s
    astTypeVariables_ (ListType _ a) s = astTypeVariables_ a s
    astTypeVariables_ (PolymorphicType _ v) s = Set.insert v s


-- Turns an AstType into an SplType such that all type variables are replaced
-- with fresh type variables, but occurences of the same type variable get the
-- same fresh type variable.
-- For example (a -> b -> c) becomes (<1> -> <2> -> <3>)
--         and (a -> b -> a) becomes (<1> -> <2> -> <1>)
-- where a, b, c are given type variables and <1>, <2>, <3> are fresh type
-- variables.
astType2splType :: AstType -> Typecheck SplType
astType2splType t = do
  tvars <- astFreshTypeVariables t
  astType2splType_ t tvars
  where
  astType2splType_ (BaseType _ "Bool") _ = right (SplBaseType BaseTypeBool)
  astType2splType_ (BaseType _ "Int") _ = right (SplBaseType BaseTypeInt)
  astType2splType_ (BaseType _ "Void") _ = right (SplBaseType BaseTypeVoid)
  astType2splType_ (BaseType _ _) _ = left $ InternalError "astType2splType: non-base types are always type variables"
  astType2splType_ (TupleType _ a b) tvars = do
    a_ <- astType2splType_ a tvars
    b_ <- astType2splType_ b tvars
    right $ SplTupleType a_ b_
  astType2splType_ (ListType _ a) tvars =  astType2splType_ a tvars >>= right . SplListType
  astType2splType_ (PolymorphicType _ a) tvars =
    case Map.lookup a tvars of
      Just v -> right v
      Nothing -> left $ InternalError "astType2splType: type variable not found"


class InferType a where
  inferType :: a -> Typecheck (SplType, Constraints)

instance InferType AstProgram where
  inferType (AstProgram decls) =
    -- mapM inferType decls >> return (SplBaseType BaseTypeVoid, noConstraints)
    -- collect all constraints while inferring. This is only needed for
    -- debugging; constraints are not needed in branches of the AST, only the
    -- environment needs to be passed around
    foldM (\(_, c1) d -> do
      (t, c2) <- inferType d
      return (t, c1 ++ c2)
      ) (SplBaseType BaseTypeVoid, noConstraints) decls

instance InferType AstDeclaration where
  inferType (AstVarDeclaration _ astType name expr) = do
    (exprType, exprConstraints) <- inferType expr
    a <- astType2splType astType
    envAddGlobal name a ((a,exprType):exprConstraints)
    return (a, (a,exprType):exprConstraints)



instance InferType AstExpr where
  inferType (AstIdentifier meta x) = envLookup x meta

  inferType (AstBoolean _ _) = return (SplBaseType BaseTypeBool, noConstraints)
  inferType (AstInteger _ _) = return (SplBaseType BaseTypeInt, noConstraints)
  inferType (AstEmptyList _) = do
    a <- fresh
    return (SplListType a, noConstraints)

  -- inferType (AstFunctionCallExpr (AstFunctionCall meta f (actualArg:_))) = do
    -- ft <- envLookup f meta
    -- (actualArgType, c2) <- inferType actualArg
    -- x <- fresh
    -- return (x, (ft, SplFunctionType [actualArgType] x):c2)


initializeEnvironment :: Typecheck ()
initializeEnvironment = sequence_
  [ do
    a <- fresh
    envAddGlobal "print" (SplFunctionType [a] (SplBaseType BaseTypeVoid)) noConstraints

  , do
    a <- fresh
    b <- fresh
    envAddGlobal "fst" (SplFunctionType [SplTupleType a b] a) noConstraints

  , do
    a <- fresh
    b <- fresh
    envAddGlobal "snd" (SplFunctionType [SplTupleType a b] b) noConstraints

  , do
    a <- fresh
    envAddGlobal "head" (SplFunctionType [SplListType a] a) noConstraints

  , do
    a <- fresh
    envAddGlobal "tail" (SplFunctionType [SplListType a] a) noConstraints

  , do
    a <- fresh
    envAddGlobal "isEmpty" (SplFunctionType [SplListType a] (SplBaseType BaseTypeBool)) noConstraints
  ]



runTypecheck :: (Typecheck a) -> (Either CompileError a, TypecheckState)
runTypecheck t = runState (runEitherT (initializeEnvironment >> t)) (0, emptyEnvironment)
