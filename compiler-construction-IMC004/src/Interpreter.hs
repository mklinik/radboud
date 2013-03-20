{--
 - Everything in Interpreter is dynamically typed, with exceptions everywhere and lots of undefined.
 -
 - The idea is that once a program passes the type checker, the undefined cases are never hit.
 --}

module Interpreter where

import Control.Monad.Trans.State.Lazy
import Control.Monad
import qualified Data.Map as Map

import Ast

data Value
  = I Integer
  | B Bool
  | L [Value]
  | V
  | F ([Value] -> Spl Value)

instance Show Value where
  show (I i) = show i
  show (B b) = show b
  show (L l) = show l
  show  V    = "Void"
  show (F _) = "<function>"

instance Eq Value where
  (==) (I i1) (I i2) = i1 == i2
  (==) (B b1) (B b2) = b1 == b2
  (==) (L l1) (L l2) = l1 == l2
  (==)  V      V     = True
  (==) _      _      = error "Eq Value: type error in comparison"

type Environment = (Map.Map String Value, [Map.Map String Value])

emptyEnvironment :: Environment
emptyEnvironment = (Map.empty,[Map.empty])

envLookup :: String -> Environment -> Value
envLookup x (globals, locals:_) =
  case Map.lookup x locals of
    (Just v) -> v
    Nothing  -> case Map.lookup x globals of
      (Just v) -> v
      Nothing  -> error ("envLookup: unknown identifier: '" ++ x ++ "'")

envAddGlobal :: String -> Value -> Environment -> Environment
envAddGlobal name value (globals, locals) = (Map.insert name value globals, locals)

envAdd :: String -> Value -> Environment -> Environment
envAdd name value (globals, l:locals) = (globals, (Map.insert name value l):locals)

envAddDeclaration :: (String -> Value -> Environment -> Environment) -> Environment -> AstDeclaration -> Spl Environment
envAddDeclaration doAdd env (AstVarDeclaration _ _ name expression) = do
  value <- eval expression
  return $ doAdd name value env
envAddDeclaration doAdd env (AstFunDeclaration _ _ name formalArgs localVars body) = do
  let fun = mkFunction formalArgs localVars body
  return $ doAdd name fun env

envUpdate :: String -> Value -> Environment -> Environment
envUpdate name value (globals, l:locals) =
  if Map.member name l
    then (globals, (Map.adjust (const value) name l):locals)
    else if Map.member name globals
      then (Map.adjust (const value) name globals, l:locals)
      else error ("envUpdate: unknown identifier: " ++ name)

envPushScope :: Environment -> Environment
envPushScope (globals, locals) = (globals, Map.empty:locals)

envPopScope :: Environment -> Environment
envPopScope (globals, (_:locals)) = (globals, locals)


type Spl a = State Environment a

-- programs can have side effects
runSpl :: AstProgram -> Spl Value
runSpl (AstProgram globals) = do
  -- put all global declarations in the environment
  env <- foldM (envAddDeclaration envAddGlobal) emptyEnvironment globals
  -- search for the main function
  let (F main) = envLookup "main" env
  put env
  main []

mkFunction :: [AstFunctionArgument] -> [AstDeclaration] -> [AstStatement] -> Value
mkFunction formalArgs decls stmts = F $ \actualArgs -> do
  modify envPushScope
  let nameValues = zipWith (\(AstFunctionArgument _ _ argName) argValue -> (argName, argValue)) formalArgs actualArgs :: [(String, Value)]
  let addActualArgs = map (modify . (uncurry envAdd)) nameValues :: [Spl ()]
  -- put actual parameters to environment
  sequence_ addActualArgs
  -- put local declarations to environment
  get >>= \env -> foldM (envAddDeclaration envAdd) env decls >>= put
  -- interpret statements
  result <- mapM interpret stmts
  modify envPopScope
  -- return value of last statement, which hopefully was a return statement
  return $ last result

interpret :: AstStatement -> Spl Value
interpret (AstReturn _ Nothing) = return V
interpret (AstReturn _ (Just e)) = eval e
interpret (AstAssignment _ var expr) = eval expr >>= \e -> modify (var `envUpdate` e) >> return V
interpret (AstFunctionCallStmt f) = apply f
interpret (AstBlock stmts) = do
  result <- mapM interpret stmts
  return $ last result
interpret while@(AstWhile _ condition stmt) = do
  (B cond) <- eval condition
  if cond
    then do interpret stmt
            interpret while
    else return V
interpret (AstIfThenElse _ condition thenStmt elseStmt) = do
  (B cond) <- eval condition
  interpret $ if cond
    then thenStmt
    else elseStmt

eval :: AstExpr -> Spl Value
eval (AstIdentifier _ i) = gets $ envLookup i
eval (AstInteger _ i) = return $ I i
eval (AstBoolean _ b) = return $ B b
eval (AstBinOp _ "+" l r) = intBinOp (+) l r I
eval (AstBinOp _ "-" l r) = intBinOp (-) l r I
eval (AstBinOp _ "*" l r) = intBinOp (*) l r I
eval (AstBinOp _ "/" l r) = intBinOp (div) l r I
eval (AstBinOp _ "%" l r) = intBinOp (mod) l r I
eval (AstBinOp _ "<" l r) = intBinOp (<) l r B
eval (AstBinOp _ ">" l r) = intBinOp (>) l r B
eval (AstBinOp _ "<=" l r) = intBinOp (<=) l r B
eval (AstBinOp _ ">=" l r) = intBinOp (>=) l r B
eval (AstBinOp _ "||" l r) = boolBinOp (||) l r
eval (AstBinOp _ "&&" l r) = boolBinOp (&&) l r
eval (AstUnaryOp _ "-" e) = do
  (I x) <- eval e
  return $ I $ negate x
eval (AstUnaryOp _ "!" e) = do
  (B x) <- eval e
  return $ B $ not x
eval (AstFunctionCallExpr f) = apply f
eval _ = error "eval: unsupported feature"

apply :: AstFunctionCall -> Spl Value
apply (AstFunctionCall _ name actualArgs) = do
  (F f) <- gets $ envLookup name
  args <- mapM eval actualArgs
  f args

intBinOp :: (Integer -> Integer -> b) -> AstExpr -> AstExpr -> (b -> Value) -> Spl Value
intBinOp f l r c = do
  (I lhs) <- eval l
  (I rhs) <- eval r
  return $ c $ f lhs rhs

boolBinOp :: (Bool -> Bool -> Bool) -> AstExpr -> AstExpr -> Spl Value
boolBinOp f l r = do
  (B lhs) <- eval l
  (B rhs) <- eval r
  return $ B $ f lhs rhs
