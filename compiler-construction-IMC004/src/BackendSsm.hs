module BackendSsm where

import Control.Monad.Trans.State

import IntermediateRepresentation
import Ast

translateSsm :: AstProgram -> Asm
translateSsm prog = generateSs $ evalState (program2ir prog) ssmMachine

ssmMachine :: Machine
ssmMachine = mkMachine (-1) 0 4 makePrologue makeEpilogue accessFunArg makeGlobalInitCode

makePrologue :: IR IrStatement
makePrologue = do
  name <- gets machineCurFunctionName
  s <- gets machineFrameSize
  return $ IrAsm [name ++ ": ldr MP", "ldrr MP SP", "ajs " ++ show s ++ " ; make space for locals "]

makeEpilogue :: IR IrStatement
makeEpilogue = do
  s <- gets machineFrameSize
  return $ IrAsm ["ajs " ++ show (-s) ++ " ; pop locals", "str MP", "ret"]

makeGlobalInitCode :: [IrStatement] -> IR [IrStatement]
makeGlobalInitCode varInits = do
  mainAddr <- envLookup "main"
  s <- gets machineFrameSize
  return $
      [ IrAsm
          [ "ldrr 5 SP ; store global frame pointer"
          , "ajs " ++ show s ++ " ; make space for globals"
          ]
      ]
    ++ varInits
    ++ [IrSeq (IrExp $ IrCall mainAddr []) (IrAsm ["ajs " ++ show (-s) ++ " ; pop globals", "halt"])]

accessFunArg :: IR IrExpression
accessFunArg = do
  argc <- gets machineCurFunctionArgCount
  idx <- gets machineCurFunArgIndex
  modify $ \m -> m { machineCurFunArgIndex = idx + 1 }
  return $ IrMem (IrBinOp OpAdd (IrTemp IrFramePointer) (IrConst (-(2 + (argc - idx - 1)))))

generateE :: IrExpression -> Asm -> Asm
generateE (IrBinOp op lhs rhs) c = genrerateBinOp op $ generateE rhs $ generateE lhs c
generateE (IrConst i) c = c ++ ["ldc " ++ show i]
generateE (IrCall addr args) c =
  c ++ ["ldc 0 ; make space for return value"]
    ++ concat (map (\a -> generateE a []) args)
    ++ generateE addr []
    ++ ["jsr", "ajs -" ++ show (length args) ++ " ; pop function arguments"]
generateE (IrTemp IrFramePointer) c = c ++ ["ldr MP"]
generateE (IrTemp IrStackPointer) c = c ++ ["ldr SP"]
generateE (IrTemp IrGlobalFramePointer) c = c ++ ["ldr 5 ; global frame pointer"]

generateE (IrMem (IrBinOp OpAdd (IrTemp IrFramePointer) (IrConst n))) c = c ++ ["ldl " ++ show n]
generateE (IrMem e) c = generateE e c ++ ["lda 0"]
generateE (IrName name) c = c ++ ["ldc " ++ name]


generateS :: IrStatement -> Asm -> Asm
generateS (IrAsm asm) c = c ++ asm
generateS (IrJump name) c = c ++ ["bra " ++ name]
generateS (IrLabel name) c = c ++ [name ++ ":"]
generateS (IrSeq s1 s2) c = generateS s2 (generateS s1 c)
generateS (IrExp e) c = generateE e c ++ ["ajs -1 ; discard unused value"]
generateS (IrSkip) c = c

generateS (IrMove (IrMem (IrBinOp OpAdd (IrTemp IrFramePointer) (IrConst n))) val) c = generateE val c ++ ["stl " ++ show n]
generateS (IrMove (IrMem dst) val) c = generateE dst (generateE val c) ++ ["sta 0"] -- standard fallback

generateS (IrCNjump condition elseLabel stmt endLabel) c =
  generateE condition c ++ ["brf " ++ elseLabel]
    ++ generateS stmt []
    ++ [endLabel ++ ":"]


generateSs :: [IrStatement] -> Asm
generateSs stmts = (concat $ map (\s -> generateS s []) stmts)

genrerateBinOp :: IrBinOp -> Asm -> Asm
genrerateBinOp OpAdd c = c ++ ["add"]
genrerateBinOp OpSub c = c ++ ["sub"]
genrerateBinOp OpMul c = c ++ ["mul"]
genrerateBinOp OpDiv c = c ++ ["div"]
genrerateBinOp OpMod c = c ++ ["mod"]

genrerateBinOp OpEq  c = c ++ ["eq"]
genrerateBinOp OpNeq c = c ++ ["ne"]
genrerateBinOp OpLt  c = c ++ ["lt"]
genrerateBinOp OpLte c = c ++ ["le"]
genrerateBinOp OpGt  c = c ++ ["gt"]
genrerateBinOp OpGte c = c ++ ["ge"]

genrerateBinOp OpAnd c = c ++ ["and"]
genrerateBinOp OpOr  c = c ++ ["or"]
genrerateBinOp OpXor c = c ++ ["xor"]
