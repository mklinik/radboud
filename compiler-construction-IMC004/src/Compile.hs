module Compile where

import Parser
import Utils
import CompileError
import Typechecker
import IntermediateRepresentation
import BackendSsm

compileSsm :: String -> String -> Either CompileError [String]
compileSsm fileName input = do
  ast <- runParser_ fileName pProgram input
  _ <- runTypecheck $ typecheck ast
  return $ translateSsm ast
