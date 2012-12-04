definition module dictionary


import StdString


::	Dictionary


newDictionary		:: Dictionary							// Create a new dictionary.
sortlistToDictionary:: ![String] -> Dictionary				// Sorted list to dictionary.
allMembers			:: !Dictionary -> [String]				// Return all words of the dictionary in lexicographical order.
membersStartingWith	:: !Char !Dictionary -> [String]		// Return all words starting with the given character.
addToDictionary		:: !String !Dictionary -> Dictionary	// Add a word to the dictionary.
isMemberDictionary	:: !String !Dictionary -> Bool			// True iff word exists in dictionary.
sizeDictionary		:: !Dictionary -> Int					// Return the number of words.
