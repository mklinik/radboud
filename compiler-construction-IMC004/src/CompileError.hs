module CompileError where

import Text.ParserCombinators.UU.BasicInstances
import Ast
import SplType

data CompileError
  = TypeError SplType SplType LineColPos
  | RowError Row Row LineColPos
  | UnknownIdentifier String AstMeta
  | ParseError String
  | PolymorphicVariable String AstMeta -- TODO unused
  | InternalError String
  | ErrorWithLocation String AstMeta -- all other errors

-- default show LineColPos is not nice
lcpToString :: LineColPos -> String
lcpToString (LineColPos l c _) = show (l+1) ++ ":" ++ show (c+1)

position :: LineColPos -> String
position p = "at position " ++ lcpToString p
