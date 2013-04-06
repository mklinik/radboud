{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Typechecker where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM)
import Data.Char (ord, chr)
import Data.List ((\\), nub)
-- import Debug.Trace

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

envLookup :: AstMeta -> String -> Environment -> Typecheck SplType
envLookup meta ident [] = left $ UnknownIdentifier ident meta
envLookup meta ident ((name,typ):env)
  | ident == name =
      case typ of
        (SplForall vars t1) -> do
          freshVars <- mapM (\var -> fresh >>= return . (,) var) vars
          let u2 = foldl (\u (v, a) -> u . mkSubstitution v a) id freshVars
          right $ substitute u2 t1
        _ -> right typ
  | otherwise     = envLookup meta ident env

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
  substitute u (SplForall vars t) = SplForall vars $ substitute u t

instance Substitute a => Substitute (b, a) where
  substitute u (b, a) = (b, substitute u a)

instance Substitute a => Substitute [a] where
  substitute u l = map (substitute u) l

unify :: AstMeta -> SplType -> SplType -> Typecheck Unifier
unify _ (SplBaseType BaseTypeInt) (SplBaseType BaseTypeInt) = return id
unify _ (SplBaseType BaseTypeBool) (SplBaseType BaseTypeBool) = return id
unify _ (SplBaseType BaseTypeVoid) (SplBaseType BaseTypeVoid) = return id
unify _ (SplTypeVariable v1) (SplTypeVariable v2) | v1 == v2 = return id
unify p s@(SplListType t1) t@(SplListType t2) =
  case runTypecheck (unify p t1 t2) of
    Left _ -> left $ TypeError s t $ sourceLocation p
    Right u -> right u
unify p s@(SplTupleType a1 b1) t@(SplTupleType a2 b2) =
  case runTypecheck (unifyAll p [(a1, a2), (b1, b2)]) of
    Left _ -> left $ TypeError s t $ sourceLocation p
    Right u -> right u
unify p t1@(SplFunctionType args1 ret1) t2@(SplFunctionType args2 ret2) =
  if length args1 == length args2
    then case runTypecheck (unifyAll p $ zip (ret1:args1) (ret2:args2)) of
      Left _ -> left $ TypeError t1 t2 $ sourceLocation p
      Right u -> right u
    else left $ TypeError t1 t2 $ sourceLocation p
unify _ (SplTypeVariable v) t | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify _ t (SplTypeVariable v) | not (elem v (typeVars t)) = return $ substitute $ mkSubstitution v t
unify p t1 t2 = left $ TypeError t1 t2 $ sourceLocation p

mkSubstitution :: String -> SplType -> SplType -> SplType
mkSubstitution v1 t (SplTypeVariable v2) | v1 == v2 = t
mkSubstitution _ _ t = t

unifyAll :: AstMeta -> Constraints -> Typecheck Unifier
unifyAll meta cs = foldM unifyBlaat emptyUnifier cs
  where
  unifyBlaat :: Unifier -> Constraint -> Typecheck Unifier
  unifyBlaat u (a, b) = do
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


quantify :: Environment -> SplType -> SplType
quantify env t =
  let freeVars = typeVars t \\ envFreeTypeVars env in
    if null freeVars then t else (SplForall (nub freeVars) t)


class InferType a where
  inferType :: Environment -> a -> SplType -> Typecheck (Unifier, Environment)


instance InferType a => InferType [a] where
  inferType env [] _ = return (emptyUnifier, env)
  inferType env (x:xs) s = do
    (u,env2) <- inferType env x s
    (u2,env3) <- inferType (substitute u env2) xs (substitute u s)
    return (u2 . u, env3)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

inferDecls :: Environment -> [AstDeclaration] -> Typecheck (Unifier, Environment)
inferDecls env decls = do
    freshVars <- mapM (\d -> claimedType d >>= \t -> return ((declName d, t), doQuantify d)) decls
      :: Typecheck [((String, SplType), Environment -> SplType -> SplType)]
    let env_ = foldl (flip $ uncurry envAdd) env $ map fst freshVars
    un <- blaat env_ emptyUnifier $ zip decls (map (snd . fst) freshVars)
    let quantifiedVars = map (\((name, typ), quant) -> (name, quant (substitute un env) $ substitute un typ)) freshVars -- apply quantify to all types
    let env3 = foldl (flip $ uncurry envAdd) (substitute un env) quantifiedVars
    return (un, env3)

    where
      declName (AstVarDeclaration _ _ name _) = name
      declName (AstFunDeclaration _ _ name _ _ _) = name

      claimedType (AstVarDeclaration _ astType _ _) = astType2splType astType
      claimedType (AstFunDeclaration _ returnType _ formalArgs _ _) = do
        let argTypes = map (\(AstFunctionArgument _ typ _) -> typ) formalArgs
        astType2splType $ FunctionType argTypes returnType

      blaat _ u [] = return u
      blaat e u ((d,a):rest) = do
        (u1,_) <- inferType (substitute u e) d (substitute u a)
        u2 <- blaat e (u1 . u) rest
        return (u2 . u1 . u)

      doQuantify (AstVarDeclaration _ _ _ _) = flip const -- dont quantify variable declarations
      doQuantify (AstFunDeclaration _ _ _ _ _ _) = quantify

instance InferType AstProgram where
  inferType env (AstProgram []) _ = return (emptyUnifier, env)
  inferType env (AstProgram (decls:declss)) s = do
    (_, env2) <- inferDecls env decls
    inferType env2 (AstProgram declss) s


instance InferType AstDeclaration where
  inferType env (AstVarDeclaration _ _ _ expr) s = do
    inferType env expr s

  inferType env (AstFunDeclaration meta returnType _ formalArgs localDecls body) s = do
    splArgs <- mapM (\((AstFunctionArgument _ typ nam)) -> astType2splType typ >>= return . (,) nam) formalArgs
    splReturnType <- astType2splType returnType
    let env2 = foldl (flip $ uncurry envAdd) env splArgs :: Environment
    let splFunctionType = SplFunctionType (map snd splArgs) splReturnType
    (u, env3) <- inferDecls env2 localDecls
    (u2,_) <- inferType env3 (AstBlock body) (substitute u splReturnType)
    u3 <- unify meta (substitute u2 s) (substitute u2 splFunctionType)
    return (u3 . u2 . u, env)


instance InferType AstStatement where
  inferType env (AstReturn meta Nothing) s = do
    u <- unify meta s splTypeVoid
    return (u, env)
  inferType env (AstReturn _ (Just expr)) s = do
    (u,_) <- inferType env expr s
    return (u, env)

  inferType env (AstIfThenElse _ astCondition thenStmt elseStmt) s = do
    (u1,_) <- inferType env astCondition splTypeBool
    (u2,_) <- inferType (substitute u1 env) thenStmt (substitute u1 s)
    (u3,_) <- inferType (substitute (u2 . u1) env) elseStmt (substitute (u2 . u1) s)
    return (u3 . u2 . u1, env)

  inferType env (AstBlock []) _ = return (emptyUnifier, env)
  inferType env (AstBlock (stmt:stmts)) s = do
    (u,_) <- inferType env stmt s
    (u2,_) <- inferType (substitute u env) (AstBlock stmts) (substitute u s)
    return (u2 . u, env)

  inferType env (AstWhile _ condition body) s = do
    (u,_) <- inferType env condition splTypeBool
    (u2,_) <- inferType (substitute u env) body (substitute u s)
    return (u2 . u, env)

  inferType env (AstAssignment meta name expr) _ = do
    nameType <- envLookup meta name env
    inferType env expr nameType

  inferType env (AstFunctionCallStmt f) _ = do
    a <- fresh -- return value is discarded and doesnt matter
    inferType env f a


instance InferType AstExpr where
  inferType env (AstIdentifier meta name) s = do
    typ <- envLookup meta name env
    u <- unify meta s typ
    return (u, env)

  inferType env (AstBoolean meta _) s = do
    u <- unify meta s splTypeBool
    return (u, env)

  inferType env (AstInteger meta _) s = do
    u <- unify meta s splTypeInt
    return (u, env)

  inferType env (AstFunctionCallExpr f) s = inferType env f s

  inferType env (AstEmptyList meta) s = inferType env (AstIdentifier meta "[]") s
  inferType env (AstBinOp meta name lhs rhs) s = inferType env (AstFunctionCall meta name [lhs, rhs]) s
  inferType env (AstUnaryOp meta name arg) s = inferType env (AstFunctionCall meta ("unary " ++ name) [arg]) s
  inferType env (AstTuple meta aExpr bExpr) s = do
    a <- fresh
    b <- fresh
    (u1,_) <- inferType env aExpr a
    (u2,_) <- inferType (substitute u1 env) bExpr b
    u3 <- unify meta (substitute (u2 . u1) s) (SplTupleType (substitute (u2 . u1) a) (substitute (u2 . u1) b))
    return (u3 . u2 . u1, env)

instance InferType AstFunctionCall where
  inferType env (AstFunctionCall meta name actualArgs) s = do
    freshArgTypes <- mapM (\arg -> fresh >>= return . (,) arg) actualArgs :: Typecheck [(AstExpr, SplType)]
    functionType <- envLookup meta name env
    u <- unify meta functionType (SplFunctionType (map snd freshArgTypes) s)
    u2 <- blaat (substitute u env) (substitute u freshArgTypes)
    return (u2 . u, env)
    where
      blaat :: Environment -> [(AstExpr, SplType)] -> Typecheck Unifier
      blaat _ [] = return emptyUnifier
      blaat e ((expr,typ):xs) = do
        (u,_) <- inferType e expr typ
        u2 <- blaat (substitute u e) (substitute u xs)
        return (u2 . u)


defaultEnvironment :: Typecheck Environment
defaultEnvironment = do
  a <- fresh
  b <- fresh
  let av = typeVars a
  let bv = typeVars b
  return $ foldl apply_ emptyEnvironment
    [ envAdd "[]" (SplForall av (SplListType a))
    , envAdd ":" (SplForall av (SplFunctionType [a, SplListType a] (SplListType a)))
    , envAdd "unary -" (SplFunctionType [splTypeInt] (splTypeInt))
    , envAdd "unary !" (SplFunctionType [splTypeBool] (splTypeBool))
    , envAdd "print" (SplForall av (SplFunctionType [a] splTypeVoid))
    , envAdd "fst" (SplForall (av ++ bv) $ SplFunctionType [SplTupleType a b] a)
    , envAdd "snd" (SplForall (av ++ bv) $ SplFunctionType [SplTupleType a b] b)
    , envAdd "head" (SplForall av (SplFunctionType [SplListType a] a))
    , envAdd "tail" (SplForall av (SplFunctionType [SplListType a] (SplListType a)))
    , envAdd "isEmpty" (SplForall av (SplFunctionType [SplListType a] splTypeBool))
    , \e -> foldl (\env o -> envAdd o (SplFunctionType [splTypeInt, splTypeInt] (splTypeInt)) env) e ["+", "-", "*", "/", "%"]
    , \e -> foldl (\env o -> envAdd o (SplFunctionType [splTypeInt, splTypeInt] (splTypeBool)) env) e ["<", ">", "<=", ">=", "==", "!="]
    , \e -> foldl (\env o -> envAdd o (SplFunctionType [splTypeBool, splTypeBool] (splTypeBool)) env) e ["&&", "||"]
    ]

apply_ :: a -> (a -> a) -> a
apply_ x f = f x

typecheck :: AstProgram -> Typecheck Environment
typecheck prog = do
  e <- defaultEnvironment
  (_, env) <- inferType e prog splTypeVoid
  return $ env `without` e

without :: Environment -> Environment -> Environment
without env e =
  filter (\(name, _) -> not $ name `elem` eNames) env
  where
    eNames = map fst e

runTypecheck :: (Typecheck a) -> Either CompileError a
runTypecheck t = evalState (runEitherT t) emptyTypecheckState

prettyprintGlobals :: Environment -> String
prettyprintGlobals env = concatMap (\(name, typ) -> name ++ " : " ++ show (makeNiceAutoTypeVariables typ) ++ "\n") env
-- prettyprintGlobals env = concatMap (\(name, typ) -> name ++ " : " ++ show typ ++ "\n") (env `without` defEnv)
  where defEnv = unRight $ runTypecheck defaultEnvironment
        unRight (Right x) = x

-- Replaces auto-type variables with letters from a-z
makeNiceAutoTypeVariables :: SplType -> SplType
makeNiceAutoTypeVariables t =
  let
    t_ = case t of
      (SplForall _ innerT) -> innerT
      _ -> t
    tvars = Set.fromList $ typeVars t_
    (_, u) = Set.foldl foobar (0, emptyUnifier) tvars
  in
    substitute u t_
  where
    foobar :: (Int, Unifier) -> String -> (Int, Unifier)
    foobar (i, u) var = (i+1, u . mkSubstitution var (SplTypeVariable ((chr $ ord 'a' + i):[])))
