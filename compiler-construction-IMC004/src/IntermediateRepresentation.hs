module IntermediateRepresentation where

import Ast

data IrExpression
  = IrBinOp IrBinOp IrExpression IrExpression
  | IrConst Int
  | IrTemp IrTemp

data IrBinOp
  = OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq | OpNeq | OpLt | OpLte | OpGt | OpGte
  | OpAnd | OpOr

data IrTemp
  = IrStackPointer
  | IrFramePointer

class Ast2Ir a where
  ast2ir :: a -> IrExpression

instance Ast2Ir AstExpr where
  ast2ir (AstInteger _ i) = IrConst $ fromInteger i
  ast2ir (AstBoolean _ b) = IrConst $ if b then -1 else 0 -- TODO this is already machine-dependent
  ast2ir (AstBinOp _ op lhs rhs) = IrBinOp (op2ir op) (ast2ir lhs) (ast2ir rhs)

op2ir :: String -> IrBinOp
op2ir "+" = OpAdd
op2ir "-" = OpSub
op2ir "*" = OpMul
op2ir "/" = OpDiv
op2ir "%" = OpMod

op2ir "==" = OpEq
op2ir "!=" = OpNeq
op2ir "<" = OpLt
op2ir "<=" = OpLte
op2ir ">" = OpGt
op2ir ">=" = OpGte

op2ir "&&" = OpAnd
op2ir "||" = OpOr

op2ir _ = error "undefined binary operator"
