module IntermediateRepresentation where

import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Map (Map)

import Ast

type Asm = [String] -- assembly program

data IrExpression
  = IrBinOp IrBinOp IrExpression IrExpression
  | IrConst Int
  | IrTemp IrTemp
  | IrMem IrExpression
  | IrCall String [IrExpression]
  deriving (Show)

data IrBinOp
  = OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq | OpNeq | OpLt | OpLte | OpGt | OpGte
  | OpAnd | OpOr
  deriving (Show)

data IrTemp
  = IrStackPointer
  | IrFramePointer
  deriving (Show)

data IrStatement
  = IrMove IrExpression IrExpression
  | IrSeq IrStatement IrStatement
  | IrLabel String
  | IrJump String
  | IrAsm Asm
  | IrExp IrExpression
  deriving (Show)

data Machine = Machine
  { machineTrue  :: Int -- encoding of the truth value True
  , machineFalse :: Int -- encoding of the truth value False
  , machineWord  :: Int -- number of bytes in a machine word
  , machineEnv :: Map String IrExpression
  , machineFrameSize :: Int -- size of current frame; i.e. the number of local variables
  , machineMakePrologue :: Machine -> IR IrStatement -- TOOD: remove Machine argument, it's already in IR
  , machineMakeEpilogue :: Machine -> IR IrStatement
  , machineCurFunctionName :: String
  , machineCurFunctionArgCount :: Int
  , machineCurFunArgIndex :: Int
  , machineAccessFunArg :: IR IrExpression
  }

-- mkMachine :: Int -> Int -> Int -> (Machine -> IR IrStatement) -> (Machine -> IR IrStatement) -> Machine
mkMachine tru fals word mkPrologue mkEpilogue accessFunArg = Machine
  { machineTrue = tru
  , machineFalse = fals
  , machineWord = word
  , machineEnv = Map.empty
  , machineFrameSize = 0
  , machineMakePrologue = mkPrologue
  , machineMakeEpilogue = mkEpilogue
  , machineCurFunctionName = ""
  , machineCurFunctionArgCount = 0
  , machineCurFunArgIndex = 0
  , machineAccessFunArg = accessFunArg
  }

type IR a = State Machine a

curFrameSize :: IR Int
curFrameSize = gets machineFrameSize

-- increase Frame Size by one machine word
bumpFrameSize :: IR ()
bumpFrameSize = modify $ \m -> m { machineFrameSize = 1 + machineFrameSize m }

envAddFunArg :: AstFunctionArgument -> IR ()
envAddFunArg (AstFunctionArgument _ _ name) = do
  lookupCode <- gets machineAccessFunArg >>= id
  modify $ \m -> m { machineEnv = Map.insert name lookupCode (machineEnv m) }

envLookup :: String -> IR IrExpression
envLookup name = do
  env <- gets machineEnv
  case Map.lookup name env of
    Nothing -> error ("unknown identifier " ++ name) -- should never happen
    Just e  -> return e

-- addGlobalVariable :: String -> IR IrExpression
-- addGlobalVariable name = do
  -- s <- curFrameSize
  -- bumpFrameSize
  -- envAdd name (IrMem $ IrConst s)
  -- envLookup name

-- varDecl2ir :: AstDeclaration -> IR IrStatement
-- varDecl2ir (AstVarDeclaration _ _ name expr) = do
  -- addr <- addGlobalVariable name
  -- e <- exp2ir expr
  -- return $ IrMove addr e

-- varDecls2ir :: [AstDeclaration] -> IR IrStatement
-- varDecls2ir [] = do
  -- s <- curFrameSize
  -- let sp = (IrTemp IrStackPointer)
  -- return $ IrMove sp (IrBinOp OpAdd sp (IrConst s))
-- varDecls2ir (d:ds) = do
  -- foo <- varDecl2ir d
  -- bar <- varDecls2ir ds
  -- return $ IrSeq foo bar

funDecl2ir :: AstDeclaration -> IR IrStatement
funDecl2ir (AstFunDeclaration _ _ name formalArgs _ body) = do
  modify $ \m -> m { machineCurFunArgIndex = 0 }
  modify $ \m -> m { machineCurFunctionName = name }
  modify $ \m -> m { machineCurFunctionArgCount = length formalArgs }
  oldEnv <- gets machineEnv
  mapM_ envAddFunArg formalArgs
  m <- get
  prologue <- (machineMakePrologue m) m
  b <- stmts2ir body
  epilogue <- (machineMakeEpilogue m) m
  modify $ \m -> m { machineEnv = oldEnv }
  return $ IrSeq (IrSeq prologue b) (IrSeq (IrLabel $ name ++ "_return") epilogue)

stmt2ir :: AstStatement -> IR IrStatement
stmt2ir (AstReturn _ Nothing) = do
  name <- gets machineCurFunctionName
  return $ IrJump $ name ++ "_return"
stmt2ir (AstReturn _ (Just e)) = do
  name <- gets machineCurFunctionName
  expr <- exp2ir e
  nArgs <- gets machineCurFunctionArgCount
  return $ IrSeq
    (IrMove (IrMem $ IrBinOp OpAdd (IrTemp IrFramePointer) (IrConst $ -(nArgs + 2) {- return address, frame pointer -})) expr)
    (IrJump $ name ++ "_return")
stmt2ir (AstFunctionCallStmt f) = funCall2ir f >>= return . IrExp
stmt2ir (AstAsm asm) = return $ IrAsm asm

funCall2ir :: AstFunctionCall -> IR IrExpression
funCall2ir (AstFunctionCall _ name actualArgs) = do
  args <- mapM exp2ir actualArgs
  return $ IrCall name args

stmts2ir :: [AstStatement] -> IR IrStatement
stmts2ir [] = error "empty statement list"
stmts2ir (s:[]) = stmt2ir s
stmts2ir (s:ss) = do
  s_ <- stmt2ir s
  ss_ <- stmts2ir ss
  return $ IrSeq s_ ss_

builtins :: [AstDeclaration]
builtins =
  [ AstFunDeclaration emptyMeta (BaseType emptyMeta "Void") "print"
      [ AstFunctionArgument emptyMeta (PolymorphicType emptyMeta "a") "x" ]
      []
      [ AstAsm ["ldl -2", "trap 0"] ]
  ]

program2ir :: AstProgram -> IR [IrStatement]
program2ir (AstProgram decls) =
  decls2ir $ concat decls ++ builtins

decls2ir :: [AstDeclaration] -> IR [IrStatement]
decls2ir decls = mapM funDecl2ir funDecls
  where
    funDecls = filter isFunDecl decls
    isFunDecl (AstFunDeclaration _ _ _ _ _ _) = True
    isFunDecl _ = False

exp2ir :: AstExpr -> IR IrExpression
exp2ir (AstInteger _ i) = return $ IrConst $ fromInteger i
exp2ir (AstBoolean _ b) = do
  m <- get
  return $ IrConst $ if b then machineTrue m else machineFalse m
exp2ir (AstBinOp _ op lhs rhs) = do
  l <- exp2ir lhs
  r <- exp2ir rhs
  return $ IrBinOp (op2ir op) l r
exp2ir (AstIdentifier _ name) = envLookup name
exp2ir (AstFunctionCallExpr f) = funCall2ir f

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
