definition module oskey

//	Clean Object I/O library, version 1.2

import	StdOverloaded

::	SpecialKey

instance	==			SpecialKey					// Equality on SpecialKey
instance	toString	SpecialKey					// Name of the SpecialKey

backSpaceKey:: SpecialKey							// Backspace
beginKey	:: SpecialKey							// Begin of text
clearKey	:: SpecialKey							// Clear
deleteKey	:: SpecialKey							// Delete
downKey		:: SpecialKey							// Arrow down
endKey		:: SpecialKey							// End of text
enterKey	:: SpecialKey							// Enter
escapeKey	:: SpecialKey							// Escape
f1Key		:: SpecialKey							// Function 1
f2Key		:: SpecialKey							// Function 2
f3Key		:: SpecialKey							// Function 3
f4Key		:: SpecialKey							// Function 4
f5Key		:: SpecialKey							// Function 5
f6Key		:: SpecialKey							// Function 6
f7Key		:: SpecialKey							// Function 7
f8Key		:: SpecialKey							// Function 8
f9Key		:: SpecialKey							// Function 9
f10Key		:: SpecialKey							// Function 10
f11Key		:: SpecialKey							// Function 11
f12Key		:: SpecialKey							// Function 12
f13Key		:: SpecialKey							// Function 13
f14Key		:: SpecialKey							// Function 14
f15Key		:: SpecialKey							// Function 15
helpKey		:: SpecialKey							// Help
leftKey		:: SpecialKey							// Arrow left
pgDownKey	:: SpecialKey							// Page down
pgUpKey		:: SpecialKey							// Page up
returnKey	:: SpecialKey							// Return
rightKey	:: SpecialKey							// Arrow right
upKey		:: SpecialKey							// Arrow up

toSpecialKey:: !Int -> SpecialKey					// Convert Int to SpecialKey
isSpecialKey:: !Int -> Bool							// Check for one of the upper SpecialKeys
