{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Typechecker where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM, liftM)
import Data.Char (ord, chr)
import Text.ParserCombinators.UU.BasicInstances (LineColPos)
import Data.List (intersperse, (\\))
import Debug.Trace

import Ast
import SplType
import CompileError

data TypecheckState = TypecheckState
  { nextAutoVar :: Integer
  }
type Typecheck a = EitherT CompileError (State TypecheckState) a
type Constraint = (SplType, SplType)
type Constraints = [Constraint]
type Environment = [(String, SplType)]

emptyEnvironment :: Environment
emptyEnvironment = []

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

type Unifier = SplType -> SplType

emptyUnifier :: Unifier
emptyUnifier = id

emptyTypecheckState :: TypecheckState
emptyTypecheckState = TypecheckState
  { nextAutoVar = 0
  }

envLookup :: String -> AstMeta -> Environment -> Typecheck SplType
envLookup ident meta [] = left $ UnknownIdentifier ident meta
envLookup ident meta ((name,typ):env)
  | ident == name = right typ
  | otherwise     = envLookup ident meta env

makeFreshTypeVariables :: SplType -> Typecheck Unifier
makeFreshTypeVariables t = do
  let tvars = Set.toList $ Set.fromList $ typeVars t
  foldM foobar emptyUnifier tvars
  where
    foobar :: Unifier -> String -> Typecheck Unifier
    foobar u var = do
      a <- fresh
      return (u . mkSubstitution var a)

envAdd :: String -> SplType -> Environment -> Environment
envAdd ident typ env = (ident,typ):env

envFreeTypeVars :: Environment -> [String]
envFreeTypeVars [] = []
envFreeTypeVars ((_, t):env) = typeVars t ++ envFreeTypeVars env

typeVars :: SplType -> [String]
typeVars (SplBaseType _) = []
typeVars (SplTypeVariable v) = [v]
typeVars (SplTupleType x y) = typeVars x ++ typeVars y
typeVars (SplListType x) = typeVars x
typeVars (SplFunctionType argTypes returnType) = foldl (\accum argType -> typeVars argType ++ accum) (typeVars returnType) argTypes
typeVars (SplForall vars t) = typeVars t \\ vars

class Substitute a where
  substitute :: Unifier -> a -> a

instance Substitute SplType where
  substitute u t@(SplTypeVariable _) = u t
  substitute _ t@(SplBaseType _) = t
  substitute u (SplTupleType x y) = SplTupleType (substitute u x) (substitute u y)
  substitute u (SplListType x) = SplListType (substitute u x)
  substitute u (SplFunctionType argTypes returnType) = SplFunctionType (map (substitute u) argTypes) (substitute u returnType)

instance Substitute Environment where
  substitute u [] = []
  substitute u ((name, typ):env) = (name, substitute u typ) : substitute u env

-- TODO: just use Either instead of Typecheck
unify :: AstMeta -> SplType -> SplType -> Typecheck Unifier
unify _ (SplBaseType BaseTypeInt) (SplBaseType BaseTypeInt) = return id
unify _ (SplBaseType BaseTypeBool) (SplBaseType BaseTypeBool) = return id
unify _ (SplBaseType BaseTypeVoid) (SplBaseType BaseTypeVoid) = return id
unify _ (SplTypeVariable v1) (SplTypeVariable v2) | v1 == v2 = return id
unify p (SplListType t1) (SplListType t2) = unify p t1 t2
unify p (SplTupleType a1 b1) (SplTupleType a2 b2) = unifyAll p [(a1, a2), (b1, b2)]
unify p t1@(SplFunctionType args1 ret1) t2@(SplFunctionType args2 ret2) =
  if length args1 == length args2
    then case runTypecheck (unifyAll p $ zip (ret1:args1) (ret2:args2)) of
      (Left _, _) -> left $ TypeError t1 t2 $ sourceLocation p
      (Right u, _) -> right u
    else left $ TypeError t1 t2 $ sourceLocation p
unify _ (SplTypeVariable v) t | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify _ t (SplTypeVariable v) | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify p t1 t2 = left $ TypeError t1 t2 $ sourceLocation p

mkSubstitution :: String -> SplType -> SplType -> SplType
mkSubstitution v1 t (SplTypeVariable v2) | v1 == v2 = t
mkSubstitution _ _ t = t

unifyAll :: AstMeta -> Constraints -> Typecheck Unifier
unifyAll meta cs = foldM (unifyBlaat meta) emptyUnifier cs
  where
  unifyBlaat :: AstMeta -> Unifier -> Constraint -> Typecheck Unifier
  unifyBlaat meta u (a, b) = do
    u2 <- unify meta (substitute u a) (substitute u b)
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
  inferType :: Environment -> a -> SplType -> Typecheck (Unifier, Environment)

dontCare :: (SplType, Constraints)
dontCare = (splTypeVoid, noConstraints)

returnSymbol :: String
returnSymbol = "#return"

quantify vars t = if null vars then t else (SplForall vars t)

instance InferType AstProgram where
  inferType env (AstProgram []) _ = return (emptyUnifier, env)

  inferType env (AstProgram ((AstVarDeclaration meta astType name expr):decls)) s = do
    a <- astType2splType astType
    (u,_) <- inferType (envAdd name a env) expr a
    let env2 = substitute u env
    let a2 = substitute u a
    let bs = typeVars a2 \\ envFreeTypeVars (substitute u env2)
    inferType (envAdd name (quantify bs a2) env2) (AstProgram decls) (substitute u s)

  inferType env (AstProgram ((AstFunDeclaration meta returnType name formalArgs localDecls (body:_)):decls)) s = do
    splArgs <- mapM (\((AstFunctionArgument _ typ name)) -> astType2splType typ >>= return . (,) name) formalArgs
    splReturnType <- astType2splType returnType
    let env2 = foldl (flip $ uncurry envAdd) env splArgs :: Environment
    let splFunctionType = SplFunctionType (map snd splArgs) splReturnType
    let env3 = envAdd name splFunctionType env2
    (u,_) <- inferType env2 body splReturnType

    let splFunctionType2 = substitute u splFunctionType
    let env21 = substitute u env
    let bs = typeVars splFunctionType2 \\ envFreeTypeVars (substitute u env21)
    inferType (envAdd name (quantify bs splFunctionType2) env21) (AstProgram decls) (substitute u s)

{-
instance InferType AstDeclaration where

  inferType (AstFunDeclaration meta returnType name formalArgs decls body) = do
    setCurrentDeclaration name
    (functionType, functionConstraints) <- envLookup name meta

    freshArgTypes <- mapM makeSplArgType formalArgs
    mapM_ (uncurry envAddFormalArg) freshArgTypes
    mapM_ (initDeclaration envAddLocal) decls
    freshReturnType <- astType2splType returnType
    envAddFormalArg returnSymbol (freshReturnType, noConstraints)
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
        -}

instance InferType AstStatement where
  inferType _ (AstReturn meta Nothing) _ = undefined
    {-
    (returnType, returnConstraints) <- envLookup returnSymbol emptyMeta
    return (returnType, (sourceLocation meta, (returnType, splTypeVoid)):returnConstraints)
  inferType (AstReturn meta (Just expr)) = do
    (returnType, returnConstraints) <- envLookup returnSymbol emptyMeta
    (exprType, exprConstraints) <- inferType expr
    return (returnType, (sourceLocation meta, (returnType, exprType)):returnConstraints ++ exprConstraints)

  inferType (AstIfThenElse meta astCondition thenStmt elseStmt) = do
    (conditionType, conditionConstraints) <- inferType astCondition
    (_, thenConstraints) <- inferType thenStmt
    (_, elseConstraints) <- inferType elseStmt
    return (splTypeVoid, (sourceLocation meta, (splTypeBool, conditionType)):conditionConstraints ++ thenConstraints ++ elseConstraints)

  inferType (AstBlock stmts) = do
    blaat <- mapM inferType stmts
    return (splTypeVoid, concatMap snd blaat)

  inferType (AstWhile meta condition body) = do
    (conditionType, conditionConstraints) <- inferType condition
    (_, bodyConstraints) <- inferType body
    return (splTypeVoid, (sourceLocation meta, (splTypeBool, conditionType)):conditionConstraints ++ bodyConstraints)

  inferType (AstAssignment meta name expr) = do
    (exprType, exprConstraints) <- inferType expr
    (nameType, nameConstraints) <- envLookup_ False name meta
    envAddConstraints name ((sourceLocation meta, (nameType, exprType)):exprConstraints) meta
    return (splTypeVoid, (sourceLocation meta, (exprType, nameType)):exprConstraints ++ nameConstraints)

  inferType (AstFunctionCallStmt f) = inferType f
  -}

instance InferType AstExpr where
  {-
  inferType (AstIdentifier meta x) = envLookup x meta
  inferType (AstBoolean _ _) = return (splTypeBool, noConstraints)
  -}
  inferType env (AstInteger meta _) s = do
    u <- unify meta s splTypeInt
    return (u, env)
  {-
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

    -}
typecheck :: AstProgram -> Typecheck Environment
typecheck prog = do
  let env = emptyEnvironment
  (_, env) <- inferType env prog splTypeVoid
  return env

runTypecheck :: (Typecheck a) -> (Either CompileError a, TypecheckState)
runTypecheck t = runState (runEitherT t) emptyTypecheckState

prettyprintGlobals :: Environment -> String
prettyprintGlobals env = concatMap (\(name, typ) -> name ++ " : " ++ show typ ++ "\n") env

-- prettyprintConstraints :: Constraints -> String
-- prettyprintConstraints cs = concat $ intersperse ", " $ map prettyprintConstraint cs

-- prettyprintConstraint :: Constraint -> String
-- prettyprintConstraint (_, (t1, t2)) = show t1 ++ " = " ++ show t2

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
