definition module clipboardCrossCall_12

import	clCrossCall_12

//	Predefined Clipboard Formats.
CF_TEXT             :==	1
CF_BITMAP           :==	2
CF_METAFILEPICT     :==	3
CF_SYLK             :==	4
CF_DIF              :==	5
CF_TIFF             :==	6
CF_OEMTEXT          :==	7
CF_DIB              :==	8
CF_PALETTE          :==	9
CF_PENDATA          :==	10
CF_RIFF             :==	11
CF_WAVE             :==	12
CF_UNICODETEXT      :==	13
CF_ENHMETAFILE      :==	14


winInitialiseClipboard	::         !*OSToolbox ->           *OSToolbox
winGetClipboardText		::         !*OSToolbox -> (!String,!*OSToolbox)
winSetClipboardText		:: !String !*OSToolbox ->           *OSToolbox
winHasClipboardText		::         !*OSToolbox -> (!Bool,  !*OSToolbox)
winGetClipboardCount	::         !*OSToolbox -> (!Int,   !*OSToolbox)
