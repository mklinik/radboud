{--
 - Everything in Interpreter is dynamically typed, with exceptions everywhere and lots of undefined.
 -
 - The idea is that once a program passes the type checker, the undefined cases are never hit.
 --}

module Interpreter where

import Control.Monad.Trans.State.Lazy
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map

import Ast

data Value
  = I Integer
  | B Bool
  | L [Value]
  | V
  | F ([Value] -> Spl Value)
  | T (Value, Value)

instance Show Value where
  show (I i) = show i
  show (B b) = show b
  show (L l) = show l
  show  V    = "Void"
  show (F _) = "<function>"
  show (T t) = show t

instance Eq Value where
  (==) (I i1) (I i2) = i1 == i2
  (==) (B b1) (B b2) = b1 == b2
  (==) (L l1) (L l2) = l1 == l2
  (==)  V      V     = True
  (==) (T t1) (T t2) = t1 == t2
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
envLookup _ _ = error "envLookup: fatal error"

envAddGlobal :: String -> Value -> Environment -> Environment
envAddGlobal name value (globals, locals) = (Map.insert name value globals, locals)

envAdd :: String -> Value -> Environment -> Environment
envAdd name value (globals, l:locals) = (globals, (Map.insert name value l):locals)
envAdd _ _ _ = error "envAdd: fatal error"

envAddDeclaration :: (String -> Value -> Environment -> Environment) -> Environment -> AstDeclaration -> Spl Environment
envAddDeclaration doAdd env (AstVarDeclaration _ _ name expression) = do
  value <- eval expression
  lift $ return $ doAdd name value env
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
envUpdate _ _ _ = error "envUpdate: fatal error"

envPushScope :: Environment -> Environment
envPushScope (globals, locals) = (globals, Map.empty:locals)

envPopScope :: Environment -> Environment
envPopScope (globals, (_:locals)) = (globals, locals)
envPopScope _ = error "envPopScope: fatal error"


type Spl a = EitherT Value (StateT Environment IO) a

-- programs can have side effects
interpretProgram :: AstProgram -> Spl Value
interpretProgram (AstProgram globals) = do
  -- put all global declarations in the environment
  env <- foldM (envAddDeclaration envAddGlobal) emptyEnvironment globals
  lift $ put env

  -- put all built-in functions in the environment
  lift $ sequence_ $ map (modify . uncurry envAddGlobal) builtins

  -- search for the main function
  F main <- lift $ gets $ envLookup "main"

  -- execute main function
  main []

runProgram :: AstProgram -> IO String
runProgram ast = do
  blaat <- evalStateT (runEitherT (interpretProgram ast)) emptyEnvironment
  return $ show $ either id id blaat

mkFunction :: [AstFunctionArgument] -> [AstDeclaration] -> [AstStatement] -> Value
mkFunction formalArgs decls stmts = F $ \actualArgs -> do
  lift $ modify envPushScope
  let nameValues = zipWith (\(AstFunctionArgument _ _ argName) argValue -> (argName, argValue)) formalArgs actualArgs :: [(String, Value)]
  let blaat = map (uncurry envAdd) nameValues
  let addActualArgs = map (modify) blaat -- :: [Spl ()]

  -- put actual parameters to environment
  lift $ sequence_ addActualArgs

  -- put local declarations to environment
  env <- lift get
  env_ <- foldM (envAddDeclaration envAdd) env decls
  lift $ put env_

  -- interpret statements, catch Left value of first encountered return statement
  (Left result) <- lift $ (runEitherT $ mapM interpret stmts)

  -- restore old environment
  lift $ modify envPopScope

  -- turn the result, which we got as a Left Value, into a Right value
  right result

interpret :: AstStatement -> Spl Value
interpret (AstReturn _ Nothing) = left V
interpret (AstReturn _ (Just e)) = eval e >>= left
interpret (AstAssignment _ var expr) = eval expr >>= \e -> lift $ modify (var `envUpdate` e) >> return V
interpret (AstFunctionCallStmt f) = apply f
interpret (AstBlock stmts) = do
  result <- mapM interpret stmts
  return $ last result
interpret while@(AstWhile _ condition stmt) = do
  (B cond) <- eval condition
  if cond
    then do _ <- interpret stmt
            interpret while
    else return V
interpret (AstIfThenElse _ condition thenStmt elseStmt) = do
  (B cond) <- eval condition
  interpret $ if cond
    then thenStmt
    else elseStmt

eval :: AstExpr -> Spl Value
eval (AstIdentifier _ i) = lift $ gets $ envLookup i
eval (AstInteger _ i) = return $ I i
eval (AstBoolean _ b) = return $ B b
eval (AstEmptyList _) = right $ L []
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
eval (AstBinOp _ "==" l r) = polyBinOp (==) l r B
eval (AstBinOp _ "!=" l r) = polyBinOp (/=) l r B
eval (AstBinOp _ ":" h t) = do
  h_ <- eval h
  (L t_) <- eval t
  right $ L (h_:t_)
eval (AstBinOp _ o _ _) = error ("eval: unsupported binary operator: " ++ o)
eval (AstUnaryOp _ "-" e) = do
  (I x) <- eval e
  return $ I $ negate x
eval (AstUnaryOp _ "!" e) = do
  (B x) <- eval e
  return $ B $ not x
eval (AstUnaryOp _ o _) = error ("eval: unsupported unary operator: " ++ o)
eval (AstFunctionCallExpr f) = apply f
eval (AstTuple _ eX eY) = do
  x <- eval eX
  y <- eval eY
  return $ T (x, y)

apply :: AstFunctionCall -> Spl Value
apply (AstFunctionCall _ name actualArgs) = do
  (F f) <- lift $ gets $ envLookup name
  args <- mapM eval actualArgs
  f args

polyBinOp :: (Value -> Value -> b) -> AstExpr -> AstExpr -> (b -> Value) -> Spl Value
polyBinOp f l r c = do
  lhs <- eval l
  rhs <- eval r
  return $ c $ f lhs rhs

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


-- built in functions
builtins :: [(String, Value)]
builtins =
  [ ("fst", F $ \[(T (l, _))] -> right l)
  , ("snd", F $ \[(T (_, r))] -> right r)
  , ("print", F $ \[v] -> liftIO (print v) >> right V)
  , ("head", F $ \[L (h:_)] -> right h)
  , ("tail", F $ \[L (_:t)] -> right $ L t)
  , ("isEmpty", F $ \[L l] -> right $ B $ null l)
  ]
