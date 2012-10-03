implementation module dictionary


import StdArray, StdBool, StdList, StdString


::	Dictionary
	=	Leaf !String
	|	Node Dictionary !String Dictionary


// Create a new dictionary.
newDictionary :: Dictionary
newDictionary
	= Leaf ""

// Sorted list to dictionary.
sortlistToDictionary :: ![String] -> Dictionary
sortlistToDictionary xs
	| isEmpty xs	= Leaf ""
	| nrwords==1	= Leaf (toString (hd xs))
	| otherwise		= Node (sortlistToDictionary firsthalf) middle (sortlistToDictionary secondhalf)
where
	nrwords							= length xs
	(firsthalf,[middle:secondhalf])	= splitAt (nrwords/2) xs


// Return all words of the dictionary in lexicographical order.
allMembers :: !Dictionary -> [String]
allMembers (Leaf word)
	| word==""
		= []
	| otherwise
		= [word]
allMembers (Node l word r)
	| word==""
		= allMembers l ++ allMembers r
	| otherwise
		= allMembers l ++ [word: allMembers r]

// Return all words starting with the given character.
membersStartingWith :: !Char !Dictionary -> [String]
membersStartingWith letter (Node l w r)
	| w.[0]>letter
	= membersStartingWith letter l
	| w.[0]<letter
	= membersStartingWith letter r
	= membersStartingWithleft letter l [w:membersStartingWithright letter r]
where
	membersStartingWithleft :: !Char !Dictionary ![String] -> [String]
	membersStartingWithleft letter (Node l w r) t
		| w.[0]==letter
		= membersStartingWithleft letter l [w:wordsintree r t]
		= membersStartingWithleft letter r t
	membersStartingWithleft letter (Leaf b) t
		| size b>0 && b.[0]==letter
		= [b:t]
		= t
	
	membersStartingWithright :: !Char !Dictionary -> [String]
	membersStartingWithright letter (Node l w r)
		| w.[0]==letter
		= wordsintree l [w:membersStartingWithright letter r]
		= membersStartingWithright letter l
	membersStartingWithright letter b
		= membersStartingWith letter b
	
	wordsintree :: !Dictionary ![String] -> [String]
	wordsintree (Node l w r) t
		= wordsintree l [w:wordsintree r t]
	wordsintree (Leaf b) t
		| size b<>0
		= [b:t]
		= t
membersStartingWith letter (Leaf b)
	| size b>0 && b.[0]==letter
	= [b]
	= []


// Add a word to the dictionary.
addToDictionary :: !String !Dictionary -> Dictionary
addToDictionary x b=:(Node l w r)
	| x<w		= Node (addToDictionary x l) w r
	| x>w		= Node l w (addToDictionary x r)
	| otherwise	= b
addToDictionary x b=:(Leaf w)
	| x<w		= Node (Leaf x)  w (Leaf "")
	| x>w		= Node (Leaf "") w (Leaf x)
	| otherwise	= b


// True iff word exists in dictionary.
isMemberDictionary :: !String !Dictionary -> Bool
isMemberDictionary x (Leaf b)		= b==x
isMemberDictionary x (Node l w r)	= w==x || (x<w && isMemberDictionary x l) || (x>w && isMemberDictionary x r)


// Return the number of words.
sizeDictionary :: !Dictionary -> Int
sizeDictionary (Leaf _)		= 1
sizeDictionary (Node l _ r)	= sizeDictionary l+sizeDictionary r+1
