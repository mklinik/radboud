definition module osclipboard

//	Clean Object I/O library, version 1.2

//	Clipboard operations.

import	ostoolbox
from clipboardCrossCall_12	import CF_TEXT


::	OSClipboardItemType
	:==	Int
OSClipboardText
	:==	CF_TEXT

osInitialiseClipboard :: !*OSToolbox -> *OSToolbox
//	osInitialiseClipboard should be evaluated before any of the functions below.

osHasClipboardText :: !*OSToolbox -> (!Bool,!*OSToolbox)
//	osHasClipboardText checks whether the clipboard currently contains a text item.

osSetClipboardText :: !{#Char} !*OSToolbox -> *OSToolbox
//	osSetClipboardText empties the clipboard and sets the text to the clipboard.
//	The return Int is the new version number.

osGetClipboardText :: !*OSToolbox -> (!{#Char},!*OSToolbox)
//	osGetClipboardText retrieves the current clipboard text item, which is empty if not present.

osGetClipboardContent :: !*OSToolbox -> (![OSClipboardItemType],!*OSToolbox)
//	osGetClipboardContent retrieves the current item types that are stored in the clipboard.

osGetClipboardVersion :: !Int !*OSToolbox -> (!Int,!*OSToolbox)
//	osGetClipboardVersion given the previous version number returns the new, current version number.
