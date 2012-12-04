implementation module StringAppender

import StdString, StdArray, StdInt

:: StringAppender = { elements 		:: [String]
		 		    , full_length 	:: Int
		 		    }

newAppender :: StringAppender		  
newAppender = {full_length = 0, elements = []}		  
		   
append :: StringAppender a -> StringAppender | toString a
append appender a 
	# str = toString a
	= {full_length = appender.full_length + (size str), elements = [str:appender.elements]} 
	
concat_rev :: ![String] !Int -> String
concat_rev xs full_length = concat` xs (createArray full_length '\0') 0
where
	concat` []     dst _		= dst
	concat` [x:xs] dst offset	= concat` xs (copyChars (full_length - offset - (size x)) 0 (size x) x dst) (offset + size x)
	
	copyChars offset i num src dst
	| i == num		= dst
	| otherwise		= copyChars offset (i+1) num src {dst & [offset + i] = src.[i]}	
	
joinList :: !String [a] StringAppender -> StringAppender | toString a
joinList sep [t] a = append a t
joinList sep [t:ts] a = joinList sep ts (append (append a t) sep)
joinList sep [] a = a	
	
instance toString StringAppender			   
where
	toString appender = concat_rev appender.elements appender.full_length

instance Appendable String
where
	(<++) a b = append a b

instance Appendable Int
where
	(<++) a b = append a b

instance Appendable Real
where
	(<++) a b = append a b

instance Appendable (StringAppender -> StringAppender)
where
	(<++) a f = f a
