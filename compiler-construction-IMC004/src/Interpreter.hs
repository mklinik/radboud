{--
 - Everything in Interpreter is dynamically typed, with exceptions everywhere and lots of undefined.
 -
 - The idea is that once a program passes the type checker, the undefined cases are never hit.
 --}

module Interpreter where

import Control.Monad.Trans.State.Lazy

import Ast

data Value
  = I Integer
  | B Bool
  | L [Value]
  | F (Value -> Value)

instance Show Value where
  show (I i) = show i
  show (B b) = show b
  show (L l) = show l
  show (F _) = "<function>"

instance Eq Value where
  (==) (I i1) (I i2) = i1 == i2
  (==) (B b1) (B b2) = b1 == b2
  (==) (L l1) (L l2) = l1 == l2
  (==) _      _      = error "Eq Value: type error in comparison"

type Environment = String -> Value
type Spl a = State Environment a

emptyState :: Environment
emptyState i = error ("Environment: unknown identifier: '" ++ i ++ "'")

(|->) :: String -> Value -> Environment -> Environment
(|->) ident val s =
  \x -> if x == ident
    then val
    else s x

-- programs can have side effects
runSpl :: AstProgram -> IO ()
runSpl = undefined

interpret :: AstStatement -> IO (Spl ())
interpret = undefined

eval :: AstExpr -> Spl Value
eval (AstIdentifier _ i) = get >>= \s -> return $ s i
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
eval _ = error "eval: unsupported feature"

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
