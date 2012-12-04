definition module CleanPrettyPrinter

from syntax import	::AType,
					::ParsedSelector,
					::ParsedTypeDef,
					::RhsDefsOfType,
					::TypeContext,
					::TypeDef
					
from PPrint import	class Pretty,
					::Doc

printAType :: !Bool !AType  -> Doc

//instance Pretty TypeContext
instance Pretty [TypeContext]

instance Pretty ParsedTypeDef
instance Pretty ParsedSelector

instance Pretty Doc

prettyPrint :: Int a -> String | Pretty a


