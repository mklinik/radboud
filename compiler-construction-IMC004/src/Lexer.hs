module Lexer where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils hiding (pDigit)

keywords =
  [ "Void"
	, "Int"
	, "Bool"
	, "if"
	, "else"
	, "while"
	, "return"
	, "False"
	, "True"
	]
