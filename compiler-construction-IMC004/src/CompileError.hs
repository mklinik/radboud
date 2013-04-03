module CompileError where

import Text.ParserCombinators.UU.BasicInstances
import Ast
import SplType

data CompileError
  = TypeError SplType SplType LineColPos
  | UnknownIdentifier String AstMeta
  | ParseError String
  | InternalError String

instance Show CompileError where
  show (TypeError expected got p) =
    "Couldn't match expected type `" ++ show expected
    ++ "' with actual type `" ++ show got ++ "' "
    ++ lcpToString p
  show (UnknownIdentifier ident meta) =
    "Unknown identifier `" ++ ident ++ "' " ++ position meta
  show (ParseError message) = message
  show (InternalError x) = "Internal error `" ++ x ++ "'" -- should never happen, but you know...

-- default show LineColPos is not nice
lcpToString :: LineColPos -> String
lcpToString (LineColPos l c _) = show (l+1) ++ ":" ++ show (c+1)

position :: AstMeta -> String
position meta = "at position " ++ lcpToString (sourceLocation meta)
