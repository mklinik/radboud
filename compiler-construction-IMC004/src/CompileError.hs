module CompileError where

import Ast
import SplType

data CompileError
  = TypeError SplType SplType AstMeta
  | UnknownIdentifier String AstMeta
  | ParseError String
  | InternalError String

instance Show CompileError where
  show (TypeError expected got meta) =
    "Couldn't match expected type `" ++ show expected
    ++ "' with actual type `" ++ show got ++ "' "
    ++ position meta
  show (UnknownIdentifier ident meta) =
    "Unknown identifier `" ++ ident ++ "' " ++ position meta
  show (ParseError message) = message
  show (InternalError x) = "Internal error `" ++ x ++ "'" -- should never happen, but you know...

position :: AstMeta -> String
position meta = "at position " ++ show (sourceLocation meta)
