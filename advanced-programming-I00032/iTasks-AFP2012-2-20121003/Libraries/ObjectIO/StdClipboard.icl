implementation module StdClipboard


import	StdFunc, StdList, StdMisc, StdString
import	osclipboard
import	StdMaybe
from	commondef	import fatalError, strictSeq, strictSeqList, remove, :: Cond
from	iostate		import :: PSt{..}, :: IOSt, getIOToolbox, setIOToolbox, accIOToolbox,
						ioStGetClipboardState, ioStSetClipboardState, :: ClipboardState(..)


stdClipboardFatalError :: String String -> .x
stdClipboardFatalError function error
	= fatalError function "StdClipboard" error


//	The clipboard item type:

::	ClipboardItem
	=	ClipboardString !String			// Support for strings
//	|	ClipboardPict	Handle			// Support for pictures (PA: not supported yet)

class Clipboard item where
	toClipboard		:: !item			-> ClipboardItem
	fromClipboard	:: !ClipboardItem	-> Maybe item

instance Clipboard {#Char} where
	toClipboard :: !{#Char} -> ClipboardItem
	toClipboard string = ClipboardString string
	
	fromClipboard :: !ClipboardItem -> Maybe {#Char}
	fromClipboard (ClipboardString string) = Just string


//	Reading and writing the value of the selection to the clipboard:

setClipboard :: ![ClipboardItem] !(PSt .l) -> PSt .l
setClipboard clipItems pState=:{io}
	# (tb,ioState)	= getIOToolbox io
	# tb			= osInitialiseClipboard tb
	# tb			= strictSeq (map clipboardItemToScrap singleItems) tb
	# ioState		= setIOToolbox tb ioState
	= {pState & io=ioState}
where
	singleItems		= removeDuplicateClipItems clipItems
	
	removeDuplicateClipItems :: ![ClipboardItem] -> [ClipboardItem]
	removeDuplicateClipItems [item:items]
		# (_,_,items)	= remove (eqClipboardType item) undef items
		= [item:removeDuplicateClipItems items]
	where
		eqClipboardType :: !ClipboardItem !ClipboardItem -> Bool
		eqClipboardType (ClipboardString _) item	= case item of
														(ClipboardString _)	-> True
														_					-> False
	removeDuplicateClipItems items
		= items
	
	clipboardItemToScrap :: !ClipboardItem !*OSToolbox -> *OSToolbox
	clipboardItemToScrap (ClipboardString text) tb
		= osSetClipboardText text tb

getClipboard :: !(PSt .l) -> (![ClipboardItem],!PSt .l)
getClipboard pState
	# (tb,ioState)		= getIOToolbox pState.io
	# tb				= osInitialiseClipboard tb
	# (contents,tb)		= osGetClipboardContent tb
	  contents			= filter ((==) OSClipboardText) contents
	# (clipItems,tb)	= strictSeqList (map scrapToClipboardItem contents) tb
	# (cbs,ioState)		= ioStGetClipboardState ioState
	# (version,tb)		= osGetClipboardVersion cbs.cbsCount tb
	# ioState			= ioStSetClipboardState {cbs & cbsCount=version} ioState
	# ioState			= setIOToolbox tb ioState
	= (clipItems,{pState & io=ioState})
where
	scrapToClipboardItem :: !Int !*OSToolbox -> (!ClipboardItem,!*OSToolbox)
	scrapToClipboardItem OSClipboardText tb
		# (text,tb)	= osGetClipboardText tb
		= (ClipboardString text,tb)
	scrapToClipboardItem type tb
		= stdClipboardFatalError "getClipboard" ("unimplemented clipboard content of type: "+++toString type)

clipboardHasChanged :: !(PSt .l) -> (!Bool,!PSt .l)
clipboardHasChanged pState
	# (cbs,ioState)		= ioStGetClipboardState pState.io
	  oldCount			= cbs.cbsCount
	# (tb,ioState)		= getIOToolbox ioState
	# tb				= osInitialiseClipboard tb
	# (newCount,tb)		= osGetClipboardVersion oldCount tb
	# ioState			= setIOToolbox tb ioState
	= (oldCount<>newCount,{pState & io=ioState})
