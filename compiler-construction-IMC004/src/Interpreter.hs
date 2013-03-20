{--
 - Everything in Interpreter is dynamically typed, with exceptions everywhere and lots of undefined.
 -
 - The idea is that once a program passes the type checker, the undefined cases are never hit.
 --}

module Interpreter where

import Control.Monad.Trans.State.Lazy
import Control.Monad

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

type Environment = String -> Value
type Spl a = State Environment a

lookupEnv :: String -> Environment -> Value
lookupEnv x e = e x

emptyEnvironment :: Environment
emptyEnvironment i = error ("Environment: unknown identifier: '" ++ i ++ "'")

(|->) :: String -> Value -> Environment -> Environment
(|->) ident val s =
  \x -> if x == ident
    then val
    else s x

-- programs can have side effects
runSpl :: AstProgram -> Spl Value
runSpl (AstProgram globals) = do
  -- put all global declarations in the environment
  env <- foldM addDeclaration emptyEnvironment globals
  -- search for the main function
  let (F main) = env "main"
  put env
  main []

addDeclaration :: Environment -> AstDeclaration -> Spl Environment
addDeclaration env (AstVarDeclaration _ _ name expression) = do
  value <- eval expression
  return $ (name |-> value) env
addDeclaration env (AstFunDeclaration _ _ name formalArgs localVars body) = do
  let fun = mkFunction formalArgs localVars body
  return $ (name |-> fun) env

mkFunction :: [AstFunctionArgument] -> [AstDeclaration] -> [AstStatement] -> Value
mkFunction formalArgs decls stmts = F $ \actualArgs -> do
  let nameValues = zipWith (\(AstFunctionArgument _ _ argName) argValue -> (argName, argValue)) formalArgs actualArgs :: [(String, Value)]
  let modifies = map (modify . (uncurry (|->))) nameValues :: [Spl ()]
  -- put actual parameters to environment
  sequence_ modifies
  -- put local declarations to environment
  get >>= \env -> foldM addDeclaration env decls >>= put
  -- interpret statements
  result <- mapM interpret stmts
  -- return value of last statement, which hopefully was a return statement
  return $ last result

interpret :: AstStatement -> Spl Value
interpret (AstReturn _ Nothing) = return V
interpret (AstReturn _ (Just e)) = eval e
interpret (AstAssignment _ var expr) = eval expr >>= \e -> modify (var |-> e) >> return V
interpret (AstFunctionCallStmt f) = apply f

eval :: AstExpr -> Spl Value
eval (AstIdentifier _ i) = gets $ lookupEnv i
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
  (F f) <- gets $ lookupEnv name
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
