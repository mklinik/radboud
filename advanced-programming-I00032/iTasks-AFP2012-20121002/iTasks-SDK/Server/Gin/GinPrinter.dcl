definition module GinPrinter

from Map import ::Map

from GinParser import :: GPath(..), ::GPathNode(..), :: GResourceId(..)

from PPrint import ::Doc
class Printer a
where 
	def :: a -> a
	scope :: [a] -> a
	newscope :: [a] -> a
	align :: a -> a
	(<->) infixr 6 :: a a -> a
	(<+>) infixr 6 :: a a -> a
	(</>) infixr 5 :: a a -> a
	(<$>) infixr 5 :: a a -> a
	(<$?>) infixr 5 :: a a -> a
	empty :: a
	space :: a
	comma :: a
	char :: Char -> a
	text :: String -> a
	string :: String -> a
	parens :: a -> a
	brackets :: a -> a
	braces :: a -> a
	dquotes :: a -> a
	tupled :: [a] -> a
	punctuate :: a [a] -> [a]
	hsep :: [a] -> a
	fillSep :: [a] -> a
	position :: GPath -> a

instance Printer Doc
prettyPrint :: Doc -> String

:: LineMap :== Map Int GPath

::PDoc
instance Printer PDoc
positionPrint :: PDoc -> (String, LineMap)
