module BackendSsm where

import IntermediateRepresentation

ssmTrue = -1
ssmFalse = 0

type Asm = [String] -- assembly program

generateE :: Asm -> IrExpression -> Asm
generateE c (IrBinOp op lhs rhs) = c ++ (generateE [] lhs) ++ (generateE [] rhs) ++ (genrerateBinOp op)
generateE c (IrConst i) = c ++ ["ldc " ++ show i]

genrerateBinOp :: IrBinOp -> [String]
genrerateBinOp OpAdd = ["add"]
genrerateBinOp OpSub = ["sub"]
genrerateBinOp OpMul = ["mul"]
genrerateBinOp OpDiv = ["div"]
genrerateBinOp OpMod = ["mod"]

genrerateBinOp OpEq  = ["eq"]
genrerateBinOp OpNeq = ["ne"]
genrerateBinOp OpLt  = ["lt"]
genrerateBinOp OpLte = ["le"]
genrerateBinOp OpGt  = ["gt"]
genrerateBinOp OpGte = ["ge"]

genrerateBinOp OpAnd = ["and"]
genrerateBinOp OpOr  = ["or"]
