implementation module osclipboard

//	Clipboard operations.

import StdInt
import clipboardCrossCall_12

::	OSClipboardItemType
	:==	Int
OSClipboardText
	:==	CF_TEXT

osInitialiseClipboard :: !*OSToolbox -> *OSToolbox
osInitialiseClipboard tb
	= winInitialiseClipboard tb

osHasClipboardText :: !*OSToolbox -> (!Bool,!*OSToolbox)
osHasClipboardText tb
	= winHasClipboardText tb

osSetClipboardText :: !{#Char} !*OSToolbox -> *OSToolbox
osSetClipboardText text tb
	= winSetClipboardText text tb

osGetClipboardText :: !*OSToolbox -> (!{#Char},!*OSToolbox)
osGetClipboardText tb
	= winGetClipboardText tb

osGetClipboardContent :: !*OSToolbox -> (![OSClipboardItemType],!*OSToolbox)
osGetClipboardContent tb
	# (hasText,tb)	= winHasClipboardText tb
	= (if hasText [OSClipboardText] [],tb)

osGetClipboardVersion :: !Int !*OSToolbox -> (!Int,!*OSToolbox)
osGetClipboardVersion nr tb
	= winGetClipboardCount tb
