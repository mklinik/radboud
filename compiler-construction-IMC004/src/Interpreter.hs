{--
 - Everything in Interpreter is dynamically typed, with exceptions everywhere and lots of undefined.
 -
 - The idea is that once a program passes the type checker, the undefined cases are never hit.
 --}

module Interpreter where

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

type State = String -> Value

emptyState :: State
emptyState i = error ("State: unknown identifier: '" ++ i ++ "'")

-- programs can have side effects
runSpl :: AstProgram -> IO State
runSpl = undefined

interpret :: State -> AstStatement -> IO State
interpret = undefined

eval :: State -> AstExpr -> Value
eval s (AstIdentifier _ i) = s i
eval s (AstInteger _ i) = I i
eval s (AstBoolean _ b) = B b
eval s (AstTuple _ _ _) = error ("eval: tuples not yet supported")
eval s (AstEmptyList _) = error ("eval: lists not yet supported")
eval s (AstBinOp _ "+" l r) = intBinOpInt (+) s l r
eval s (AstBinOp _ "-" l r) = intBinOpInt (-) s l r
eval s (AstBinOp _ "*" l r) = intBinOpInt (*) s l r
eval s (AstBinOp _ "/" l r) = intBinOpInt (div) s l r
eval s (AstBinOp _ "%" l r) = intBinOpInt (mod) s l r
eval s (AstBinOp _ ":" l r) = error ("eval: lists not yet supported")
eval s (AstBinOp _ "<" l r) = intBinOpBool (<) s l r
eval s (AstBinOp _ ">" l r) = intBinOpBool (>) s l r
eval s (AstBinOp _ "<=" l r) = intBinOpBool (<=) s l r
eval s (AstBinOp _ ">=" l r) = intBinOpBool (<=) s l r
eval s (AstBinOp _ "||" l r) = boolBinOpBool (||) s l r
eval s (AstBinOp _ "&&" l r) = boolBinOpBool (&&) s l r
eval s (AstBinOp _ _ _ _) = error ("eval: unknown binary operator")
eval s (AstUnaryOp _ "-" e) = intUnaryOpInt negate s e
eval s (AstUnaryOp _ _ _) = error ("eval: unknown unary operator")
eval s (AstFunctionCallExpr _) = error ("eval: function calls not yet supported")

intBinOpInt f s l r = intBinOp_ f (eval s l) (eval s r)
  where
    intBinOp_ f (I l) (I r) = I $ f l r
    intBinOp_ _ _ _ = error ("intBinOpInt: type error")

intUnaryOpInt f s e = intUnaryOp_ f (eval s e)
  where
    intUnaryOp_ f (I i) = I $ f i
    intUnaryOp_ _ _ = error ("intUnaryOpInt: type error")

intBinOpBool f s l r = intBinOpBool_ f (eval s l) (eval s r)
  where
    intBinOpBool_ f (I l_) (I r_) = B $ f l_ r_
    intBinOpBool_ _ _ _ = error ("intBinOpBool: type error")

boolBinOpBool f s l r = boolBinOpBool_ f (eval s l) (eval s r)
  where
    boolBinOpBool_ f (B l_) (B r_) = B $ f l_ r_
    boolBinOpBool_ _ _ _ = error ("boolBinOpBool: type error")
