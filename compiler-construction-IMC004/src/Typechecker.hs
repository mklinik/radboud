module Typechecker where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM)

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
  return $ SplTypeVariable ("<" ++ show i ++ ">")

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

-- If the identifier already exists, it is replaced.
-- TODO: explicitly distinguish between updating and adding global identifiers, to generate error messages for updates
envAddGlobal :: String -> SplType -> Constraints -> Typecheck ()
envAddGlobal ident t constraints = do
  (i, (globals, locals)) <- lift get
  lift $ put (i, (Map.insert ident (t, constraints) globals, locals))

envAddLocal :: String -> (SplType, Constraints) -> Typecheck ()
envAddLocal name value = do
  (i, (globals, locals)) <- lift get
  lift $ put (i, (globals, Map.insert name value locals))

envClearLocals :: Typecheck ()
envClearLocals = do
  (i, (globals, _)) <- lift get
  lift $ put (i, (globals, Map.empty))

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
    astTypeVariables_ (FunctionType argTypes returnType) s =
      astTypeVariables_ returnType $ foldl (flip astTypeVariables_) s argTypes


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
  astType2splType_ (FunctionType argTypes returnType) tvars = do
    returnType_ <- astType2splType_ returnType tvars
    argTypes_ <- mapM (\typ -> astType2splType_ typ tvars) argTypes
    right $ SplFunctionType argTypes_ returnType_


class InferType a where
  inferType :: a -> Typecheck (SplType, Constraints)


initDeclaration :: AstDeclaration -> Typecheck ()
initDeclaration (AstVarDeclaration _ astType name _) = do
  a <- astType2splType astType
  envAddGlobal name a noConstraints
initDeclaration (AstFunDeclaration _ astReturnType name formalArgs _ _) = do
  -- we're building a fake Ast node here: the function type
  a <- astType2splType (FunctionType (map (\(AstFunctionArgument _ ty _) -> ty) formalArgs) astReturnType)
  envAddGlobal name a noConstraints

instance InferType AstProgram where
  inferType (AstProgram decls) = do
    -- two passes:
    -- first pass: put all global identifiers to environment
    mapM_ initDeclaration decls

    -- second pass: infer type of global identifiers
    mapM_ inferType decls

    return (SplBaseType BaseTypeVoid, noConstraints) -- don't care


instance InferType AstDeclaration where

  inferType (AstVarDeclaration meta _ name expr) = do
    (splType, constraints) <- envLookup name meta
    (exprType, exprConstraints) <- inferType expr
    envAddGlobal name splType (constraints ++ ((splType,exprType):exprConstraints))
    return (SplBaseType BaseTypeVoid, noConstraints) -- don't care

  inferType (AstFunDeclaration meta returnType name formalArgs _ body) = do
    (functionType, functionConstraints) <- envLookup name meta

    freshArgTypes <- mapM makeSplArgType formalArgs
    mapM_ (uncurry envAddLocal) freshArgTypes
    freshReturnType <- astType2splType returnType
    envAddLocal "#return" (freshReturnType, noConstraints)
    blaat <- mapM inferType body
    let bodyConstraints = concatMap snd blaat
    envClearLocals

    let inferredType = SplFunctionType (map (fst . snd) freshArgTypes) freshReturnType
    envAddGlobal name functionType (functionConstraints ++ (functionType,inferredType):bodyConstraints)

    return (SplBaseType BaseTypeVoid, noConstraints) -- don't care
    where
      makeSplArgType :: AstFunctionArgument -> Typecheck (String, (SplType, Constraints))
      makeSplArgType (AstFunctionArgument _ typ nam) = do
        typ_ <- astType2splType typ
        return (nam, (typ_, noConstraints))

dontCare :: (SplType, Constraints)
dontCare = (SplBaseType BaseTypeVoid, noConstraints)

instance InferType AstStatement where
  inferType (AstReturn _ Nothing) = do
    (returnType, returnConstraints) <- envLookup "#return" emptyMeta
    return (returnType, (splTypeVoid, returnType):returnConstraints)
  inferType (AstReturn _ (Just expr)) = do
    (returnType, returnConstraints) <- envLookup "#return" emptyMeta
    (exprType, exprConstraints) <- inferType expr
    return (returnType, (exprType, returnType):returnConstraints ++ exprConstraints)

  inferType (AstIfThenElse _ astCondition thenStmt elseStmt) = do
    (conditionType, conditionConstraints) <- inferType astCondition
    (_, thenConstraints) <- inferType thenStmt
    (_, elseConstraints) <- inferType elseStmt
    return (splTypeVoid, (conditionType, splTypeBool):conditionConstraints ++ thenConstraints ++ elseConstraints)

  inferType (AstBlock stmts) = do
    blaat <- mapM inferType stmts
    return (splTypeVoid, concatMap snd blaat)

  inferType (AstWhile _ condition body) = do
    (conditionType, conditionConstraints) <- inferType condition
    (_, bodyConstraints) <- inferType body
    return (splTypeVoid, (conditionType, splTypeBool):conditionConstraints ++ bodyConstraints)

  inferType (AstAssignment meta name expr) = do
    (exprType, exprConstraints) <- inferType expr
    (nameType, nameConstraints) <- envLookup name meta
    return (splTypeVoid, (exprType, nameType):exprConstraints ++ nameConstraints)

instance InferType AstExpr where
  inferType (AstIdentifier meta x) = envLookup x meta
  inferType (AstBoolean _ _) = return (SplBaseType BaseTypeBool, noConstraints)
  inferType (AstInteger _ _) = return (SplBaseType BaseTypeInt, noConstraints)
  inferType (AstEmptyList _) = do
    a <- fresh
    return (SplListType a, noConstraints)
  inferType (AstFunctionCallExpr f) = inferType f
  inferType (AstBinOp meta name lhs rhs) = inferType (AstFunctionCall meta name [lhs, rhs])
  inferType (AstUnaryOp meta name arg) = inferType (AstFunctionCall meta ("unary " ++ name) [arg])
  inferType (AstTuple _ a b) = do
    (aType, aConstraints) <- inferType a
    (bType, bConstraints) <- inferType b
    return (SplTupleType aType bType, aConstraints ++ bConstraints)

instance InferType AstFunctionCall where
  inferType (AstFunctionCall meta f actualArgs) = do
    (functionType, functionConstraints) <- envLookup f meta
    (actualArgTypes, actualArgsConstraints) <- liftM unzip $ mapM inferType actualArgs
    returnType <- fresh
    return (returnType, (functionType, SplFunctionType actualArgTypes returnType):functionConstraints ++ concat actualArgsConstraints)


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

  -- unary and binary operators are put here with invalid identifiers
  , do
    envAddGlobal "unary !" (SplFunctionType [SplBaseType BaseTypeBool] (SplBaseType BaseTypeBool)) noConstraints

  , do
    envAddGlobal "unary -" (SplFunctionType [SplBaseType BaseTypeInt] (SplBaseType BaseTypeInt)) noConstraints

  , do
    mapM_ (\o -> envAddGlobal o (SplFunctionType [SplBaseType BaseTypeInt, SplBaseType BaseTypeInt] (SplBaseType BaseTypeInt)) noConstraints)
      ["+", "-", "*", "/", "%"]
  ]



runTypecheck :: (Typecheck a) -> (Either CompileError a, TypecheckState)
runTypecheck t = runState (runEitherT (initializeEnvironment >> t)) (0, emptyEnvironment)

prettyprintGlobals :: Environment -> String
prettyprintGlobals (globals, _) =
    concatMap (\(name, (typ, constr)) -> name ++ " : " ++ show typ ++ " | " ++ show constr ++ "\n") blaat
  where blaat = Map.toList globals
