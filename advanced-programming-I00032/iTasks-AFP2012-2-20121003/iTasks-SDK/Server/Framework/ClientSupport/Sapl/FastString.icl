implementation module FastString

import StdEnv

startsWith :: !String !String -> Bool
startsWith start str
	| (size str) >= (size start)
		= startsWith_ ((size start)-1)
		= False
where
	startsWith_ -1 = True
	startsWith_ starti
		= case start.[starti] == str.[starti] of
				True = startsWith_ (starti-1)
					 = False
					 
endsWith :: !String !String -> Bool
endsWith end str
	| (size str) >= (size end)
		= endsWith_ ((size end)-1) ((size str)-1)
		= False
where
	endsWith_ -1 _ = True
	endsWith_ endi stri 
		= case end.[endi] == str.[stri] of
				True = endsWith_ (endi-1) (stri-1)
					 = False

charIndex :: !String !Int !Char -> (!Bool,!Int)
charIndex s i char
	| i == (size s)
		= (False,size s)
		
		| i < (size s)
			| s.[i] == char
				= (True,i)
				= charIndex s (i+1) char
			= abort "CharIndex: index out of range"
					
charIndexBackwards :: !String !Int !Char -> (!Bool,!Int)
charIndexBackwards s i char
	| i == (-1)
		= (False,size s)
		
		| s.[i] == char
			= (True,i)
			= charIndexBackwards s (i-1) char
			
matchAt :: !String !String !Int -> Bool
matchAt s1 s2 p 
	| ((size s1) + p) > (size s2) 
				= False
				= and [s1.[i] == s2.[p + i] \\ i <- [0..((size s1) - 1)]]
