implementation module Base64

import StdChar, StdString, StdList, StdArray, StdMisc, StdBool

//65th character is padding-character
stdAlphabet :== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
urlAlphabet :== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="
			 
base64Encode :: !.String -> .String
base64Encode s = encodeString s stdAlphabet

base64EncodeLen :: !.String !Length -> .String
base64EncodeLen s l = addLineBreaks (encodeString s stdAlphabet) l

base64URLEncode :: !.String -> .String
base64URLEncode s = encodeString s urlAlphabet

base64URLEncodeLen :: !.String !Length -> .String
base64URLEncodeLen s l = addLineBreaks (encodeString s urlAlphabet) l

encodeString :: !.{#Char} !Alphabet -> .{#Char}
encodeString s a
	# destSize		= 4*((srcSize + 2) / 3)
	# destString	= createArray destSize '\0'
	= encodeString` destString 0 0
where
	encodeString` :: !*{#Char} !Int !Int -> *{#Char}
	encodeString` dest src_o dest_o
		# r = srcSize - src_o
		| r >= 3	= encodeString` (encodeOctet s.[src_o] s.[src_o+1] s.[src_o+2] 0 dest) (src_o + 3) (dest_o + 4)
		| r == 2	= encodeOctet s.[src_o] s.[src_o+1] (fromInt 0) 1 dest
		| r == 1	= encodeOctet s.[src_o] (fromInt 0) (fromInt 0) 2 dest
		| r == 0	= dest
	where
		encodeOctet :: !Char !Char !Char !Padding !*{#Char} -> *{#Char}
		encodeOctet c1 c2 c3 p dest = encodeOctet` ((toInt c1)<<16 + (toInt c2)<<8 + (toInt c3)) 3 p dest
		where
			encodeOctet` :: !Int !Offset !Padding !*{#Char} -> *{#Char}
			encodeOctet` _ -1 _ s = s
			encodeOctet` oct off p s
				| p > 0		= encodeOctet` (oct>>6) (off-1) (p-1) {s & [off+dest_o] = a.[64]}
				| otherwise	= encodeOctet` (oct>>6) (off-1) p {s & [off+dest_o] = (a.[(oct bitand 63)])}
		
	srcSize = size s

addLineBreaks :: !.String Length -> .String
addLineBreaks s l
| l > 0 = addLineBreaks` s "" l
| otherwise = abort "Length cannot be 0 or less."
where
	addLineBreaks` :: !.String !.String !Length -> .String
	addLineBreaks` src dest len
	| len >= (size src) = dest +++. src
	| otherwise = addLineBreaks` (src % (len,(size src))) (dest+++(src % (0,len-1))+++"\n") len

base64Decode :: !.String -> .String
base64Decode s = decodeString (removeLineBreaks s) stdAlphabet

base64URLDecode :: !.String -> .String
base64URLDecode s = decodeString (removeLineBreaks s) urlAlphabet

decodeString :: !.String !Alphabet -> .String
decodeString s a
	| srcSize - (srcSize/4*4) <> 0 = abort "Base64: Invalid length, size of decoding string must be a multitude of 4."
	# destString = createArray destSize '\0'
	= decodeString` destString 0 0
where
	decodeString` :: !*{#Char} !Int !Int -> *{#Char}
	decodeString` dest src_o dest_o
		| src_o < srcSize	= decodeString` (decodeOctet dest) (src_o + 4) (dest_o + 3)
		| otherwise			= dest
	where
		decodeOctet :: !*{#Char} -> *{#Char}
		decodeOctet dest
		| s.[2+src_o] == a.[64]	= decodeOctet` (((getValue s.[0+src_o])<<6+(getValue s.[1+src_o]))>>4) 0 dest													//lose the last four obsolete bits (2*6-8b)
		| s.[3+src_o] == a.[64]	= decodeOctet` (((getValue s.[0+src_o])<<12+(getValue s.[1+src_o])<<6+(getValue s.[2+src_o]))>>2) 1 dest						//lose the last two obsolete bits (3*6-2*8b)
		| otherwise				= decodeOctet` ((getValue s.[0+src_o])<<18+(getValue s.[1+src_o])<<12+(getValue s.[2+src_o])<<6+(getValue s.[3+src_o])) 2 dest
		where
			decodeOctet` :: !Int !Offset !*{#Char} -> *{#Char}
			decodeOctet` _ -1 s		= s
			decodeOctet` oct off s	= decodeOctet` (oct>>8) (off-1) {s & [off+dest_o] = (fromInt (oct bitand 255))}
		
			getValue :: !Char -> Int
			getValue c = hd ([i \\ i<-[0..(size a-2)] | (a.[i] == c)])
		
	srcSize = size s
	destSize
		| srcSize == 0				= 0
		# d = srcSize*3/4
		| s.[srcSize-2] == a.[64]	= d - 2
		| s.[srcSize-1] == a.[64]	= d - 1
		| otherwise					= d

removeLineBreaks :: !{#Char} -> {#Char}
removeLineBreaks src = {char \\ char <-: src | char <> '\n'}
