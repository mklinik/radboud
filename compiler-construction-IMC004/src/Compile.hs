module Compile where

import Parser
import Utils
import CompileError
import Typechecker
import BackendSsm

compileSsm :: String -> String -> Either CompileError [String]
compileSsm fileName input = do
  ast <- runParser_ fileName pProgram input
  _ <- runTypecheck $ typecheck ast
  return $ translateSsm ast

compileSsmNoTypecheck :: String -> String -> Either CompileError [String]
compileSsmNoTypecheck fileName input = do
  ast <- runParser_ fileName pProgram input
  return $ translateSsm ast
