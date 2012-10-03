implementation module clipboardCrossCall_12


import	StdBool, StdClass, StdInt, StdMisc
import	clCrossCall_12
from	clCCall_12	import winMakeCString, winGetCStringAndFree, winReleaseCString, :: CSTR
import	code from "cCrossCallClipboard_121."


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


winInitialiseClipboard :: !*OSToolbox -> *OSToolbox
winInitialiseClipboard _
	= code
	{
		.inline InstallCrossCallClipboard
			ccall InstallCrossCallClipboard "I-I"
		.end
	}
		
winGetClipboardText :: !*OSToolbox -> (!String,!*OSToolbox)
winGetClipboardText tb
	# (rcci,tb)			= issueCleanRequest2 (errorCallback2 "winGetClipboardText") (Rq0Cci CcRqGETCLIPBOARDTEXT) tb
	  rmsg				= rcci.ccMsg
	| rmsg==CcRETURN1	= winGetCStringAndFree rcci.p1 tb
	| rmsg==CcWASQUIT	= ("",tb)
	| otherwise			= abort "[winGetClipboardText] expected CcRETURN1 value.\n"

winSetClipboardText :: !String !*OSToolbox -> *OSToolbox
winSetClipboardText text tb
	# (textptr,tb)		= winMakeCString text tb
	# (_,tb)			= issueCleanRequest2 (errorCallback2 "winSetClipboardText") (Rq1Cci CcRqSETCLIPBOARDTEXT textptr) tb
	# tb				= winReleaseCString textptr tb
	= tb

winHasClipboardText :: !*OSToolbox -> (!Bool,!*OSToolbox)
winHasClipboardText tb
	# (rcci,tb)			= issueCleanRequest2 (errorCallback2 "winHasClipboardText") (Rq0Cci CcRqCLIPBOARDHASTEXT) tb
	  rmsg				= rcci.ccMsg
	| rmsg==CcRETURN1	= (rcci.p1<>0,tb)
	| rmsg==CcWASQUIT	= (False,     tb)
	| otherwise			= abort "[winHasClipboardText] expected CcRETURN1 value."

winGetClipboardCount :: !*OSToolbox -> (!Int,!*OSToolbox)
winGetClipboardCount tb
	# (rcci,tb)			= issueCleanRequest2 (errorCallback2 "winGetClipboardCount") (Rq0Cci CcRqGETCLIPBOARDCOUNT) tb
	  rmsg				= rcci.ccMsg
	| rmsg==CcRETURN1	= (rcci.p1,tb)
	| rmsg==CcWASQUIT	= (0,      tb)
	| otherwise			= abort "[winGetClipboardCount] expected CcRETURN1 value.\n"
