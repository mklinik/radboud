module Typechecker where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM, liftM)
import Data.Char (ord, chr)
import Text.ParserCombinators.UU.BasicInstances (LineColPos)
import Data.List (intersperse)

import Ast
import SplType
import CompileError

data Environment = Environment
  { globals :: Map.Map String (SplType, Constraints)
  , locals :: Map.Map String (SplType, Constraints)
  , nextAutoVar :: Integer
  , currentDeclaration :: Maybe String
  }
type TypecheckState = Environment
type Typecheck a = EitherT CompileError (State TypecheckState) a

type Constraint = (LineColPos, (SplType, SplType))
type Constraints = [Constraint]

instance Show CompileError where
  show (TypeError expected got p) =
    "Couldn't match expected type `" ++ (show $ makeNiceAutoTypeVariables expected)
    ++ "' with actual type `" ++ (show $ makeNiceAutoTypeVariables got) ++ "' "
    ++ position p
  show (UnknownIdentifier ident meta) =
    "Unknown identifier `" ++ ident ++ "' " ++ (position $ sourceLocation meta)
  show (ParseError message) = message
  show (InternalError x) = "Internal error `" ++ x ++ "'" -- should never happen, but you know...

noConstraints :: Constraints
noConstraints = []

fresh :: Typecheck SplType
fresh = do
  env <- lift get
  let i = nextAutoVar env
  lift $ put $ env { nextAutoVar = i+1 }
  return $ SplTypeVariable ("<" ++ show i ++ ">")

setCurrentDeclaration :: String -> Typecheck ()
setCurrentDeclaration name = do
  env <- lift get
  lift $ put $ env { currentDeclaration = Just name }

clearCurrentDeclaration :: Typecheck ()
clearCurrentDeclaration = do
  env <- lift get
  lift $ put $ env { currentDeclaration = Nothing }

type Unifier = SplType -> SplType

emptyUnifier :: Unifier
emptyUnifier = id

emptyEnvironment :: Environment
emptyEnvironment = Environment
  { globals = Map.empty
  , locals = Map.empty
  , nextAutoVar = 0
  , currentDeclaration = Nothing
  }

envLookup :: String -> AstMeta -> Typecheck (SplType, Constraints)
envLookup ident meta = do
  env <- lift get
  case Map.lookup ident (locals env) of
    (Just t) -> right t
    Nothing  -> case Map.lookup ident (globals env) of
      (Just t) -> if currentDeclaration env == (Just ident) then right t else makeFreshTypeVariables t
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
typeVarsInConstraints ((_, (t1, t2)):cs) = typeVars t1 ++ typeVars t2 ++ typeVarsInConstraints cs

-- If the identifier already exists, it is replaced.
-- TODO: explicitly distinguish between updating and adding global identifiers, to generate error messages for updates
envAddGlobal :: String -> (SplType, Constraints) -> Typecheck ()
envAddGlobal ident t = do
  env <- lift get
  lift $ put env { globals = Map.insert ident t (globals env) }

envAddLocal :: String -> (SplType, Constraints) -> Typecheck ()
envAddLocal name value = do
  env <- lift get
  lift $ put env { locals =  Map.insert name value (locals env) }

envClearLocals :: Typecheck ()
envClearLocals = do
  env <- lift get
  lift $ put env { locals = Map.empty }

envAddConstraints :: String -> Constraints -> AstMeta -> Typecheck ()
envAddConstraints ident newCs meta = do
  env <- lift get
  case Map.lookup ident (locals env) of
    Just (t, oldCs) -> lift $ put $ env { locals = Map.insert ident (t, oldCs ++ newCs) (locals env) }
    Nothing  -> case Map.lookup ident (globals env) of
      Just (t, oldCs) -> lift $ put $ env { globals = Map.insert ident (t, oldCs ++ newCs) (globals env) }
      Nothing  -> left $ UnknownIdentifier ident meta

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

instance Substitute LineColPos where
  substitute _ p = p

unify :: Constraint -> Typecheck Unifier
unify (_, (SplBaseType BaseTypeInt, SplBaseType BaseTypeInt)) = return id
unify (_, (SplBaseType BaseTypeBool, SplBaseType BaseTypeBool)) = return id
unify (_, (SplBaseType BaseTypeVoid, SplBaseType BaseTypeVoid)) = return id
unify (_, (SplTypeVariable v1, SplTypeVariable v2)) | v1 == v2 = return id
unify (p, (SplListType t1, SplListType t2)) = unify (p, (t1, t2))
unify (p, (SplTupleType a1 b1, SplTupleType a2 b2)) = unifyAll [(p, (a1, a2)), (p, (b1, b2))]
unify (p, (t1@(SplFunctionType args1 ret1), t2@(SplFunctionType args2 ret2))) =
  if length args1 == length args2
    then unifyAll $ zip (repeat p) $ zip (ret1:args1) (ret2:args2)
    else left $ TypeError t1 t2 p
unify (_, (SplTypeVariable v, t)) | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify (_, (t, SplTypeVariable v)) | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify (p, (t1, t2)) = left $ TypeError t1 t2 p

mkSubstitution :: String -> SplType -> SplType -> SplType
mkSubstitution v1 t (SplTypeVariable v2) | v1 == v2 = t
mkSubstitution _ _ t = t

unifyAll :: Constraints -> Typecheck Unifier
unifyAll cs = foldM unifyBlaat emptyUnifier cs

unifyBlaat :: Unifier -> Constraint -> Typecheck Unifier
unifyBlaat u (p, (a, b)) = do
 u2 <- unify (p, (substitute u a, substitute u b))
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

-- initDeclaration :: AstDeclaration -> Typecheck ()
initDeclaration doAdd (AstVarDeclaration _ astType name _) = do
  a <- astType2splType astType
  doAdd name (a, noConstraints)
initDeclaration doAdd (AstFunDeclaration _ astReturnType name formalArgs _ _) = do
  -- we're building a fake Ast node here: the function type
  a <- astType2splType (FunctionType (map (\(AstFunctionArgument _ ty _) -> ty) formalArgs) astReturnType)
  doAdd name (a, noConstraints)

instance InferType AstProgram where
  inferType (AstProgram decls) = do
    -- two passes:
    -- first pass: put all global identifiers to environment
    mapM_ (initDeclaration envAddGlobal) decls

    -- second pass: infer type of global identifiers
    mapM_ inferType decls

    return dontCare

returnSymbol :: String
returnSymbol = "#return"

instance InferType AstDeclaration where

  inferType (AstVarDeclaration meta _ name expr) = do
    setCurrentDeclaration name
    (splType, _) <- envLookup name meta
    (exprType, exprConstraints) <- inferType expr
    envAddConstraints name ((sourceLocation meta, (splType,exprType)):exprConstraints) meta
    clearCurrentDeclaration
    return dontCare

  inferType (AstFunDeclaration meta returnType name formalArgs decls body) = do
    setCurrentDeclaration name
    (functionType, functionConstraints) <- envLookup name meta

    freshArgTypes <- mapM makeSplArgType formalArgs
    mapM_ (uncurry envAddLocal) freshArgTypes
    mapM_ (initDeclaration envAddLocal) decls
    freshReturnType <- astType2splType returnType
    envAddLocal returnSymbol (freshReturnType, noConstraints)
    blaat2 <- mapM inferType decls
    blaat <- mapM inferType body
    let bodyConstraints = concatMap snd blaat
    let declConstraints = concatMap snd blaat2
    envClearLocals

    let inferredType = SplFunctionType (map (fst . snd) freshArgTypes) freshReturnType
    envAddGlobal name (functionType, (functionConstraints ++ (sourceLocation meta, (functionType,inferredType)):bodyConstraints ++ declConstraints))

    clearCurrentDeclaration
    return dontCare
    where
      makeSplArgType :: AstFunctionArgument -> Typecheck (String, (SplType, Constraints))
      makeSplArgType (AstFunctionArgument _ typ nam) = do
        typ_ <- astType2splType typ
        return (nam, (typ_, noConstraints))

dontCare :: (SplType, Constraints)
dontCare = (splTypeVoid, noConstraints)

instance InferType AstStatement where
  inferType (AstReturn meta Nothing) = do
    (returnType, returnConstraints) <- envLookup returnSymbol emptyMeta
    return (returnType, (sourceLocation meta, (splTypeVoid, returnType)):returnConstraints)
  inferType (AstReturn meta (Just expr)) = do
    (returnType, returnConstraints) <- envLookup returnSymbol emptyMeta
    (exprType, exprConstraints) <- inferType expr
    return (returnType, (sourceLocation meta, (exprType, returnType)):returnConstraints ++ exprConstraints)

  inferType (AstIfThenElse meta astCondition thenStmt elseStmt) = do
    (conditionType, conditionConstraints) <- inferType astCondition
    (_, thenConstraints) <- inferType thenStmt
    (_, elseConstraints) <- inferType elseStmt
    return (splTypeVoid, (sourceLocation meta, (conditionType, splTypeBool)):conditionConstraints ++ thenConstraints ++ elseConstraints)

  inferType (AstBlock stmts) = do
    blaat <- mapM inferType stmts
    return (splTypeVoid, concatMap snd blaat)

  inferType (AstWhile meta condition body) = do
    (conditionType, conditionConstraints) <- inferType condition
    (_, bodyConstraints) <- inferType body
    return (splTypeVoid, (sourceLocation meta, (conditionType, splTypeBool)):conditionConstraints ++ bodyConstraints)

  inferType (AstAssignment meta name expr) = do
    (exprType, exprConstraints) <- inferType expr
    (nameType, nameConstraints) <- envLookup name meta
    return (splTypeVoid, (sourceLocation meta, (exprType, nameType)):exprConstraints ++ nameConstraints)

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
    return (returnType, (sourceLocation meta, (functionType, SplFunctionType actualArgTypes returnType)):functionConstraints ++ concat actualArgsConstraints)


initializeEnvironment :: Typecheck ()
initializeEnvironment = sequence_
  [ do
    a <- fresh
    envAddGlobal "print" ((SplFunctionType [a] splTypeVoid), noConstraints)

  , do
    a <- fresh
    b <- fresh
    envAddGlobal "fst" ((SplFunctionType [SplTupleType a b] a), noConstraints)

  , do
    a <- fresh
    b <- fresh
    envAddGlobal "snd" ((SplFunctionType [SplTupleType a b] b), noConstraints)

  , do
    a <- fresh
    envAddGlobal "head" ((SplFunctionType [SplListType a] a), noConstraints)

  , do
    a <- fresh
    envAddGlobal "tail" ((SplFunctionType [SplListType a] (SplListType a)), noConstraints)

  , do
    a <- fresh
    envAddGlobal "isEmpty" ((SplFunctionType [SplListType a] splTypeBool), noConstraints)

  -- unary and binary operators are put here with invalid identifiers
  , do
    envAddGlobal "unary !" ((SplFunctionType [splTypeBool] splTypeBool), noConstraints)

  , do
    envAddGlobal "unary -" ((SplFunctionType [splTypeInt] (splTypeInt)), noConstraints)

  , do
    mapM_ (\o -> envAddGlobal o ((SplFunctionType [splTypeInt, splTypeInt] (splTypeInt)), noConstraints))
      ["+", "-", "*", "/", "%"]

  , do
    mapM_ (\o -> envAddGlobal o ((SplFunctionType [splTypeInt, splTypeInt] (splTypeBool)), noConstraints))
      ["<", ">", "<=", ">=", "==", "!="]

  , do
    mapM_ (\o -> envAddGlobal o ((SplFunctionType [splTypeBool, splTypeBool] (splTypeBool)), noConstraints))
      ["&&", "||"]

  , do
    a <- fresh
    envAddGlobal ":" ((SplFunctionType [a, SplListType a] (SplListType a)), noConstraints)
  ]

typecheck :: AstProgram -> Typecheck ()
typecheck prog = do
  -- third pass: run unifier on all global identifiers
  _ <- inferType prog -- these constraints are empty, the juicy stuff is in the environment
  env <- lift get
  unifiedGlobals <- mapM unifyOneGlobal $ Map.toList $ globals env
  lift $ put $ env { globals = Map.fromList unifiedGlobals }
  return () -- success!

unifyOneGlobal :: (String, (SplType, Constraints)) -> Typecheck (String, (SplType, Constraints))
unifyOneGlobal (name, (typ, constraints)) = do
  u <- unifyAll constraints
  return $ (name, (substitute u typ, constraints))

runTypecheck :: (Typecheck a) -> (Either CompileError a, TypecheckState)
runTypecheck t = runState (runEitherT (initializeEnvironment >> t)) emptyEnvironment

prettyprintGlobals :: Environment -> String
prettyprintGlobals env =
    concatMap (\(name, (typ, constr)) -> name ++ " : " ++ show typ ++ " | " ++ prettyprintConstraints constr ++ "\n") blaat
  where blaat = Map.toList $ globals env

prettyprintConstraints :: Constraints -> String
prettyprintConstraints cs = concat $ intersperse ", " $ map prettyprintConstraint cs

prettyprintConstraint :: Constraint -> String
prettyprintConstraint (_, (t1, t2)) = show t1 ++ " = " ++ show t2

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
