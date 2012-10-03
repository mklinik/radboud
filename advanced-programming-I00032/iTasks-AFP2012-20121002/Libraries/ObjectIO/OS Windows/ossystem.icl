implementation module ossystem

import	StdBool, StdInt, StdReal, StdString
import	StdMaybe
import	clCCall_12, clCrossCall_12, windowCrossCall_12
import	osdocumentinterface, osfont
from	ostypes import :: OSRect{..}


::	OSWindowMetrics
	=	{	osmFont				:: !Font				// The internal Font used in Windows for controls
		,	osmFontMetrics		:: !(!Int,!Int,!Int)	// The ascent, descent, leading of osmFont
		,	osmHeight			:: !Int					// The height of the internal Font
		,	osmHorMargin		:: !Int					// The default horizontal margin
		,	osmVerMargin		:: !Int					// The default vertical   margin
		,	osmHorItemSpace		:: !Int					// The default horizontal item space
		,	osmVerItemSpace		:: !Int					// The default vertical   item space
		,	osmHSliderHeight	:: !Int					// The default height of a horizontal slider control
		,	osmVSliderWidth		:: !Int					// The default width  of a vertical   slider control
		}

OSdirseparator	:==	'\\'				// OS separator between folder- and filenames in a pathname

osHomepath :: !String -> String
osHomepath fname = theApplicationPath +++ fname

osApplicationpath :: !String -> String
osApplicationpath fname = theApplicationPath +++ fname

theApplicationPath =: path
where
	ptr		= winGetAppPath 
	(path,_)= winGetCStringAndFree ptr 99

OSnewlineChars			:== "\xD\xA" // MW11++

OStickspersecond :== 1000				// OS max resolution of ticks per second

osMMtoHPixels :: !Real -> Int
osMMtoHPixels mm = toInt ( (mm/25.4) * toReal winGetHorzResolution )

osMMtoVPixels :: !Real -> Int
osMMtoVPixels mm = toInt ( (mm/25.4) * toReal winGetVertResolution )

osMaxScrollWindowSize :: (!Int,!Int)
osMaxScrollWindowSize = winMaxScrollWindowSize

osMaxFixedWindowSize :: (!Int,!Int)
osMaxFixedWindowSize = winMaxFixedWindowSize

osScreenrect :: !*OSToolbox -> (!OSRect,!*OSToolbox)
osScreenrect tb
	# (screenWidth, tb)	= winScreenXSize tb
	# (screenHeight,tb)	= winScreenYSize tb
	= ({rleft=0,rtop=0,rright=screenWidth,rbottom=screenHeight},tb)

osPrintSetupTypical :: Bool // MW11++
osPrintSetupTypical = False

osGetProcessWindowDimensions :: !OSDInfo !*OSToolbox -> (!OSRect,!*OSToolbox)
osGetProcessWindowDimensions osdinfo tb
	# maybeOSInfo		= getOSDInfoOSInfo osdinfo
	| isNothing maybeOSInfo
		= osScreenrect tb
	| otherwise
		# osinfo		= fromJust maybeOSInfo
		# ((x,y),tb)	= winGetWindowPos  osinfo.osFrame  tb
		# ((w,h),tb)	= winGetClientSize osinfo.osClient tb
		= ({rleft=x,rtop=y,rright=x+w,rbottom=y+h},tb)

osDefaultWindowMetrics :: !*OSToolbox -> (!OSWindowMetrics,!*OSToolbox)
osDefaultWindowMetrics tb
	# (font,tb)							= osDialogfont tb
	# ((ascent,descent,leading,_),tb)	= osGetfontmetrics False 0 font tb
	  height							= ascent+descent+leading
	  unit								= (toReal height)/8.0
	  margin							= toInt (unit*7.0)
	  itemspace							= toInt (unit*4.0)
	# (scrollWidth,scrollHeight,tb)		= winScrollbarSize tb
	= (	{	osmFont				= font
		,	osmFontMetrics		= (ascent,descent,leading)
		,	osmHeight			= height
		,	osmHorMargin		= margin
		,	osmVerMargin		= margin
		,	osmHorItemSpace		= itemspace
		,	osmVerItemSpace		= itemspace
		,	osmHSliderHeight	= scrollHeight
		,	osmVSliderWidth		= scrollWidth
		}
	  ,	tb
	  )

/*	osStripOuterSize isMDI isResizable (width,height)
		returns (dw,dh) required to add/subtract to view size/outer size in order to obtain
		outer size/view size.
*/
osStripOuterSize :: !Bool !Bool !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osStripOuterSize isMDI isResizable tb
	| isMDI
		# (dw,dh,tb)	= winMDIClientToOuterSizeDims styleFlags tb
		= ((dw,dh),tb)
	| otherwise
		# (dw,dh,tb)	= winSDIClientToOuterSizeDims styleFlags tb
		= ((dw,dh),tb)
where
	styleFlags			= if isResizable WS_THICKFRAME 0
