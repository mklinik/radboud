module Typechecker where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM, liftM)
import Data.Char (ord, chr)

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

type Unifier = SplType -> SplType

emptyUnifier :: Unifier
emptyUnifier = id

emptyEnvironment :: Environment
emptyEnvironment = (Map.empty, Map.empty)

envLookup :: String -> AstMeta -> Typecheck (SplType, Constraints)
envLookup ident meta = do
  (_, (globals, locals)) <- lift get
  case Map.lookup ident locals of
    (Just t) -> right t
    Nothing  -> case Map.lookup ident globals of
      (Just t) -> makeFreshTypeVariables t
      Nothing  -> left $ UnknownIdentifier ident meta

makeFreshTypeVariables :: (SplType, Constraints) -> Typecheck (SplType, Constraints)
makeFreshTypeVariables (t, constraints) = do
  let tvars = Set.toList $ Set.fromList $ typeVarsInConstraints constraints ++ typeVars t
  u <- foldM foobar emptyUnifier tvars
  return $ (substitute u t, substitute u constraints)
  where
    foobar :: Unifier -> String -> Typecheck Unifier
    foobar u var = do
      a <- fresh
      return (u . mkSubstitution var a)

typeVarsInConstraints :: Constraints -> [String]
typeVarsInConstraints [] = []
typeVarsInConstraints ((t1, t2):cs) = typeVars t1 ++ typeVars t2 ++ typeVarsInConstraints cs

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

class Substitute a where
  substitute :: Unifier -> a -> a

instance Substitute SplType where
  substitute u t@(SplTypeVariable _) = u t
  substitute _ t@(SplBaseType _) = t
  substitute u (SplTupleType x y) = SplTupleType (substitute u x) (substitute u y)
  substitute u (SplListType x) = SplListType (substitute u x)
  substitute u (SplFunctionType argTypes returnType) = SplFunctionType (map (substitute u) argTypes) (substitute u returnType)

instance (Substitute a, Substitute b) => Substitute (a, b) where
  substitute u (a, b) = (substitute u a, substitute u b)

instance Substitute a => Substitute [a] where
  substitute u l = map (substitute u) l

unify :: (SplType, SplType) -> Typecheck Unifier
unify (SplBaseType BaseTypeInt, SplBaseType BaseTypeInt) = return id
unify (SplBaseType BaseTypeBool, SplBaseType BaseTypeBool) = return id
unify (SplBaseType BaseTypeVoid, SplBaseType BaseTypeVoid) = return id
unify (SplTypeVariable v1, SplTypeVariable v2) | v1 == v2 = return id
unify (SplListType t1, SplListType t2) = unify (t1, t2)
unify (SplTupleType a1 b1, SplTupleType a2 b2) = unifyAll [(a1, a2), (b1, b2)]
unify (t1@(SplFunctionType args1 ret1), t2@(SplFunctionType args2 ret2)) =
  if length args1 == length args2
    then unifyAll $ zip (ret1:args1) (ret2:args2)
    else left $ TypeError t1 t2 emptyMeta
unify (SplTypeVariable v, t) | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify (t, SplTypeVariable v) | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify (t1, t2) = left $ TypeError t1 t2 emptyMeta

mkSubstitution :: String -> SplType -> SplType -> SplType
mkSubstitution v1 t (SplTypeVariable v2) | v1 == v2 = t
mkSubstitution _ _ t = t

unifyAll :: Constraints -> Typecheck Unifier
unifyAll cs = foldM unifyBlaat emptyUnifier cs

unifyBlaat :: Unifier -> (SplType, SplType) -> Typecheck Unifier
unifyBlaat u (a, b) = do
 u2 <- unify (substitute u a, substitute u b)
 return (u2 . u)

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
  astType2splType_ (BaseType _ "Bool") _ = right splTypeBool
  astType2splType_ (BaseType _ "Int") _ = right splTypeInt
  astType2splType_ (BaseType _ "Void") _ = right splTypeVoid
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

    return dontCare


instance InferType AstDeclaration where

  inferType (AstVarDeclaration meta _ name expr) = do
    (splType, constraints) <- envLookup name meta
    (exprType, exprConstraints) <- inferType expr
    envAddGlobal name splType (constraints ++ ((splType,exprType):exprConstraints))
    return dontCare

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

    return dontCare
    where
      makeSplArgType :: AstFunctionArgument -> Typecheck (String, (SplType, Constraints))
      makeSplArgType (AstFunctionArgument _ typ nam) = do
        typ_ <- astType2splType typ
        return (nam, (typ_, noConstraints))

dontCare :: (SplType, Constraints)
dontCare = (splTypeVoid, noConstraints)

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

  inferType (AstFunctionCallStmt f) = inferType f

instance InferType AstExpr where
  inferType (AstIdentifier meta x) = envLookup x meta
  inferType (AstBoolean _ _) = return (splTypeBool, noConstraints)
  inferType (AstInteger _ _) = return (splTypeInt, noConstraints)
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
    envAddGlobal "print" (SplFunctionType [a] splTypeVoid) noConstraints

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
    envAddGlobal "isEmpty" (SplFunctionType [SplListType a] splTypeBool) noConstraints

  -- unary and binary operators are put here with invalid identifiers
  , do
    envAddGlobal "unary !" (SplFunctionType [splTypeBool] splTypeBool) noConstraints

  , do
    envAddGlobal "unary -" (SplFunctionType [splTypeInt] (splTypeInt)) noConstraints

  , do
    mapM_ (\o -> envAddGlobal o (SplFunctionType [splTypeInt, splTypeInt] (splTypeInt)) noConstraints)
      ["+", "-", "*", "/", "%"]

  , do
    mapM_ (\o -> envAddGlobal o (SplFunctionType [splTypeInt, splTypeInt] (splTypeBool)) noConstraints)
      ["<", ">", "<=", ">=", "==", "!="]

  , do
    mapM_ (\o -> envAddGlobal o (SplFunctionType [splTypeBool, splTypeBool] (splTypeBool)) noConstraints)
      ["&&", "||"]

  , do
    a <- fresh
    envAddGlobal ":" (SplFunctionType [a, SplListType a] (SplListType a)) noConstraints
  ]

typecheck :: AstProgram -> Typecheck ()
typecheck prog = do
  -- third pass: run unifier on all global identifiers
  _ <- inferType prog -- these constraints are empty, the juicy stuff is in the environment
  (i, (globals, locals)) <- lift get
  unifiedGlobals <- mapM unifyOneGlobal $ Map.toList globals
  lift $ put (i, (Map.fromList unifiedGlobals, locals))
  return () -- success!

unifyOneGlobal :: (String, (SplType, Constraints)) -> Typecheck (String, (SplType, Constraints))
unifyOneGlobal (name, (typ, constraints)) = do
  u <- unifyAll constraints
  return $ (name, (substitute u typ, constraints))

runTypecheck :: (Typecheck a) -> (Either CompileError a, TypecheckState)
runTypecheck t = runState (runEitherT (initializeEnvironment >> t)) (0, emptyEnvironment)

prettyprintGlobals :: Environment -> String
prettyprintGlobals (globals, _) =
    concatMap (\(name, (typ, constr)) -> name ++ " : " ++ show typ ++ " | " ++ show constr ++ "\n") blaat
  where blaat = Map.toList globals

-- Replaces auto-type variables with letters from a-z
makeNiceAutoTypeVariables :: SplType -> SplType
makeNiceAutoTypeVariables t =
  let tvars = Set.fromList $ typeVars t
      (_, u) = Set.foldl foobar (0, emptyUnifier) tvars
  in
    substitute u t
  where
    foobar :: (Int, Unifier) -> String -> (Int, Unifier)
    foobar (i, u) var = (i+1, u . mkSubstitution var (SplTypeVariable ((chr $ ord 'a' + i):[])))
