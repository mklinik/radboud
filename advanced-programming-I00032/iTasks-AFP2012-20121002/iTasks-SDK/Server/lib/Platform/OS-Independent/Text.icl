implementation module Text

import StdOverloaded, StdString, StdArray, StdChar, StdInt, StdBool, StdClass, StdList

instance Text String
	where
	textSize :: !String -> Int 
	textSize s = size s	

	concat :: ![String] -> String
	concat xs = concat` xs (createArray (foldl (\s a -> s+size a) 0 xs) '\0') 0
		where
		concat` []     dst _		= dst
		concat` [x:xs] dst offset	= concat` xs (copyChars offset 0 (size x) x dst) (offset + size x)

		copyChars :: !Int !Int !Int !String !*String -> *String
		copyChars offset i num src dst
		| i == num		= dst
		| otherwise		= copyChars offset (inc i) num src {dst & [offset + i] = src.[i]}

	split :: !String !String -> [String]
	split sep s = splitAfter 0 (size s-1) sep s
	where
		splitAfter :: !Int !Int !String !String -> [String]
		splitAfter offs end sep s
			# index = indexOfAfter offs sep s
			| index == -1	= [s%(offs,end)]
							= [s%(offs,index-1) : splitAfter (index+size sep) end sep s]

	join :: !String ![String] -> String
	join sep xs = concat (join` sep xs)
	where
		join` :: !String ![String] -> [String]
		join` sep [] = []
		join` sep [x] = [x]
		join` sep [x:xs] = [x, sep : join` sep xs]

    indexOf :: !String !String -> Int
	indexOf needle haystack = indexOfAfter 0 needle haystack

    lastIndexOf :: !String !String -> Int
	lastIndexOf needle haystack
		| size needle==0
			= -1
			= lastIndexOf` (size haystack - size needle) needle.[0] needle haystack
		where
		lastIndexOf` :: !Int !Char !{#Char} !{#Char} -> Int
		lastIndexOf` offs needleChar0 needle haystack
			| offs>=0
				| haystack.[offs]<>needleChar0
					= lastIndexOf` (offs - 1) needleChar0 needle haystack
					= equalStringOrIndexOfPrevious 1 offs needle haystack
				= -1		

		equalStringOrIndexOfPrevious :: !Int !Int !{#Char} !{#Char} -> Int
		equalStringOrIndexOfPrevious i offs needle haystack
			| i<size needle
				| needle.[i]==haystack.[i+offs]
					= equalStringOrIndexOfPrevious (i+1) offs needle haystack
					= lastIndexOf` (offs - 1) needle.[0] needle haystack
				= offs
																					
	indexOfAfter :: !Int !String !String -> Int
	indexOfAfter offs needle haystack
		| size needle==0
			= -1
			= indexOf` offs needle.[0] (size haystack - size needle) needle haystack
		where
		indexOf` :: !Int !Char !Int !{#Char} !{#Char} -> Int
		indexOf` offs needleChar0 max_offs needle haystack
			| offs <= max_offs
				| haystack.[offs]<>needleChar0
					= indexOf` (offs + 1) needleChar0 max_offs needle haystack
					= equalStringOrIndexOfNext 1 offs max_offs needle haystack
				= -1

		equalStringOrIndexOfNext :: !Int !Int !Int !{#Char} !{#Char} -> Int
		equalStringOrIndexOfNext i offs max_offs needle haystack
			| i<size needle
				| needle.[i]==haystack.[i+offs]
					= equalStringOrIndexOfNext (i+1) offs max_offs needle haystack
					= indexOf` (offs + 1) needle.[0] max_offs needle haystack
				= offs

    startsWith :: !String !String -> Bool
	startsWith needle haystack
		= s_needle <= size haystack && needle == haystack%(0,s_needle-1)
	where
		s_needle	= size needle

    endsWith :: !String !String -> Bool
	endsWith needle haystack
		= s_needle <= s_haystack && needle == haystack%(s_haystack-s_needle,s_haystack-1)
	where
		s_needle	= size needle
		s_haystack	= size haystack

    subString :: !Int !Int !String -> String
	subString start len haystack = haystack % (start, start + len - 1)

	replaceSubString :: !String !String !String -> String
	replaceSubString needle replacement haystack
		| index == -1	= haystack
		| otherwise		= start +++ replacement +++ (replaceSubString needle replacement end)
			where
			index	= indexOf needle haystack
			start	= subString 0 index haystack
			end		= subString (index + size needle) (size haystack) haystack
    
    trim :: !String -> String
	trim s = ltrim (rtrim s)

	ltrim :: !String -> String
	ltrim s
		| non_space_index == 0
						= s
						= s%(non_space_index,size_s-1)
	where
		size_s			= size s
		non_space_index	= non_space_left 0
		
		non_space_left :: !Int -> Int
		non_space_left i
			| i < size_s && isSpace s.[i]	= non_space_left (i+1)
											= i

	rtrim :: !String -> String
	rtrim s
		| non_space_index == size_s-1
						= s
						= s%(0,non_space_index)
	where
		size_s			= size s
		non_space_index	= non_space_right (size_s-1)
		
		non_space_right :: !Int -> Int
		non_space_right i
			| i >= 0 && isSpace s.[i]	= non_space_right (i-1)
										= i

	lpad :: !String !Int !Char -> String
	lpad s w  c
		= let boundary = w - size s in {if (i < boundary) c s.[i - boundary] \\ i <- [0.. w - 1]}
	
	rpad :: !String !Int !Char -> String
    rpad s w c
    	= let boundary = size s in {if (i < boundary) s.[i] c \\ i <- [0.. w - 1]}

    toLowerCase :: !String -> String
	toLowerCase s = {toLower c \\ c <-: s}

    toUpperCase :: !String -> String
	toUpperCase s = {toUpper c \\ c <-: s}
	
	upperCaseFirst :: !String -> String
	upperCaseFirst "" = ""
	upperCaseFirst s = s:=(0,toUpper s.[0])
