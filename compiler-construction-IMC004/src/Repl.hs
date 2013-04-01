module Repl where

import qualified System.Console.Readline as Readline

import Parser
import Typechecker
import Utils
import Interpreter

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
  maybeLine <- Readline.readline "> "
  case maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just line   -> do
      Readline.addHistory line
      case runParser_ "interactive" pProgram line of
        Right ast -> print $ runTypecheck (inferType ast)
        -- Right ast -> runProgram ast >>= putStrLn
        Left err -> print err
      readEvalPrintLoop
