module CompileError where

import Text.ParserCombinators.UU.BasicInstances
import Ast
import SplType

data CompileError
  = TypeError SplType SplType LineColPos
  | UnknownIdentifier String AstMeta
  | ParseError String
  | InternalError String

-- default show LineColPos is not nice
lcpToString :: LineColPos -> String
lcpToString (LineColPos l c _) = show (l+1) ++ ":" ++ show (c+1)

position :: LineColPos -> String
position p = "at position " ++ lcpToString p
