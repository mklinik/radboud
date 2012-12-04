definition module StdClipboard


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdClipboard specifies all functions on the clipboard.
//	********************************************************************************


import	StdMaybe
from	iostate import :: PSt


//	Clipboard data items:

::	ClipboardItem

class Clipboard item where
	toClipboard		:: !item			-> ClipboardItem
	fromClipboard	:: !ClipboardItem	-> Maybe item
/*	toClipboard
		makes an item transferable to the clipboard.
	fromClipboard
		attempts to retrieve an item of the instance type from the clipboard item.
		If this fails, the result is Nothing, otherwise it is (Just item).
*/
	
instance Clipboard {#Char}


//	Access to the current content of the clipboard:

setClipboard :: ![ClipboardItem] !(PSt .l) -> PSt .l
getClipboard :: !(PSt .l) -> (![ClipboardItem],!PSt .l)
/*	setClipboard
		replaces the current content of the clipboard with the argument list. 
		Of the list only the first occurence of a ClipboardItem of the same type
		will be stored in the clipboard. 
		Note that setClipboard [] erases the clipboard.
	getClipboard
		gets the current content of the clipboard without changing the content.
*/


clipboardHasChanged	:: !(PSt .l) -> (!Bool,!PSt .l)
/*	clipboardHasChanged holds if the current content of the clipboard is different
	from the last access to the clipboard.
*/
