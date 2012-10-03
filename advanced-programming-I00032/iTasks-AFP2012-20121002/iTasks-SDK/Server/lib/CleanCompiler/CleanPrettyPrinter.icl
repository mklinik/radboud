implementation module CleanPrettyPrinter

import syntax

from PPrint import	class Pretty(..), ::Doc, ::SimpleDoc, <->, </>, <$>, align, brackets, comma, display, empty, fill, fillSep, 
					hang, hcat, hsep, lbrace, linebreak, nest, parens, punctuate, rbrace, renderPretty, text, tupled, vsep

instance Pretty AType where
	pretty atype = printAType False atype 
	
instance Pretty ATypeVar where
	pretty {atv_variable} = pretty atv_variable

printAType :: !Bool !AType  -> Doc
printAType withParens atype = printType withParens atype.at_type

instance Pretty BasicType where
	pretty bt = text (toString bt)

instance Pretty DefinedSymbol where
	pretty {ds_ident} = pretty ds_ident

instance Pretty (Global a) | Pretty a where
	pretty { Global | glob_object = obj} = pretty obj

instance Pretty Ident where
	pretty {id_name} = text id_name

instance Pretty ParsedConstructor where
	pretty { pc_cons_ident, pc_arg_types }
		= fillSep [ pretty pc_cons_ident : map pretty pc_arg_types ]
	
instance Pretty ParsedSelector
where
	pretty ps = printSelector 0 ps

printSelector :: !Int !ParsedSelector -> Doc
printSelector len {ps_field_ident, ps_field_type } = fill len (pretty ps_field_ident) </> text "::" </> pretty ps_field_type

instance Pretty ParsedTypeDef where
	pretty { td_ident, td_args, td_rhs } = text "::" </> pretty td_ident </> hsep (map pretty td_args) </> pretty td_rhs

instance Pretty RhsDefsOfType where
	pretty (ConsList constructors) 
		= text "=" </> hang -2 (vsep [pretty (hd constructors) : [ text "|" </> pretty c \\ c <- tl constructors ]])
	pretty (SelectorList ident typeVars isBoxed selectors) 
		# len = maxList [ size s.ps_field_ident.id_name \\ s <- selectors ]
		= text "=" </> align ((vsep [lbrace </> printSelector len (hd selectors) : [ comma </> printSelector len c \\ c <- tl selectors]]) <$> rbrace)
	pretty (TypeSpec atype) = text ":==" </> pretty atype
	pretty (EmptyRhs _) = empty
	pretty 	_ = text "= ??"

instance Pretty TCClass where
	pretty (TCClass global) = pretty global
	pretty (TCGeneric {gtc_class}) = pretty gtc_class

instance Pretty Type where
	pretty type = printType False type

printType :: !Bool !Type -> Doc
printType withParens (TA ident []) 
	| ident.type_ident.id_name == "_String" = text "String"
printType withParens (TA ident []) = pretty ident
printType withParens (TA ident [param])
	| ident.type_ident.id_name == "_List" = brackets (pretty param)
printType withParens (TA ident params)
	| ident.type_ident.id_name % (0,5) == "_Tuple" = parens (intersperse comma (map (printAType False) params))
printType withParens (TA ident params) 
	= (if withParens parens id) (fillSep [pretty ident : map (printAType True) params])
printType withParens (TAS ident params _)
	= printType withParens (TA ident params) // skip strictness annotations
printType withParens (TV tv) = pretty tv
printType withParens (--> a b) = parens (pretty a </> text "->" </> pretty b)
printType withParens (TB bt) = pretty bt
printType _ _ = text "(unknown type)"

instance Pretty TypeContext where
	pretty { tc_class, tc_types } = pretty tc_class </> intersperse comma (map (printType False) tc_types)

instance Pretty [TypeContext] where
	pretty [] = empty
	pretty tcs = text "|" </> intersperse (text " & ") (map pretty tcs) 

instance Pretty TypeSymbIdent where
	pretty {type_ident} = pretty type_ident

instance Pretty TypeVar where
	pretty {tv_ident} = pretty tv_ident

intersperse :: Doc [Doc] -> Doc
intersperse sep xs = hcat (punctuate sep xs)

prettyPrint :: Int a -> String | Pretty a
prettyPrint width x = display (renderPretty 0.9 width (nest 4 (pretty x)))

instance Pretty Doc where
	pretty doc = doc
