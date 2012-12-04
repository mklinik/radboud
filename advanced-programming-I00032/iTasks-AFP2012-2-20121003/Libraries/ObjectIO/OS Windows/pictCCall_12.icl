implementation module pictCCall_12


from	ostypes	import :: OSRect{..}, :: HDC
import	rgnCCall_12


::	*PIC
	:== (	!HDC
		,	!*OSToolbox
		)
::	Pt
	:== (	!Int
		,	!Int
		)
::	RGBcolor
	:==	(	!Int
		,	!Int
		,	!Int
		)
::	Fnt
	:== (	!{#Char}
		,	!Int
		,	!Int
		)



iWhitePattern		:== 4
iLtGreyPattern		:== 3
iGreyPattern		:== 2
iDkGreyPattern		:== 1
iBlackPattern		:== 0

iModeNotBic			:== 7
iModeNotXor			:== 6
iModeNotOr			:== 5
iModeNotCopy		:== 4
iModeBic			:== 3
iModeXor			:== 2
iModeOr				:== 1
iModeCopy			:== 0

iStrikeOut			:== 8
iUnderline			:== 4
iItalic				:== 2
iBold				:== 1

//	PA: constants for drawing polygons.
ALTERNATE			:== 1
WINDING				:== 2
//	PA: end of addition.


//	PA: win(Create/Destroy)ScreenHDC added to temporarily create a HDC of a screen.
//		Never use these values for a window or control.
winCreateScreenHDC :: !*OSToolbox -> PIC
winCreateScreenHDC _
	= code
	{
		.inline WinCreateScreenHDC
			ccall WinCreateScreenHDC "I-II"
		.end
	}

winDestroyScreenHDC :: !PIC -> *OSToolbox
winDestroyScreenHDC _
	= code
	{
		.inline WinDestroyScreenHDC
			ccall WinDestroyScreenHDC "II-I"
		.end
	}

// MW: this is never used in the object IO
winGetPicStringWidth ::  !{#Char} !PIC -> ( !Int, !PIC)
winGetPicStringWidth _ _
	= code
	{
		.inline WinGetPicStringWidth
			ccall WinGetPicStringWidth "SII-III"
		.end
	}

winGetPicCharWidth ::  !Char !PIC -> ( !Int, !PIC)
winGetPicCharWidth _ _
	= code
	{
		.inline WinGetPicCharWidth
			ccall WinGetPicCharWidth "III-III"
		.end
	}
// END MW

winGetStringWidth ::  !{#Char} !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)
winGetStringWidth _ _ _ _ _
	= code
	{
		.inline WinGetStringWidth
			ccall WinGetStringWidth "SSIIIII-II"
		.end
	}

winGetCharWidth ::  !Char !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)
winGetCharWidth _ _ _ _ _
	= code
	{
		.inline WinGetCharWidth
			ccall WinGetCharWidth "ISIIIII-II"
		.end
	}

winGetPicFontInfo ::  !PIC -> ( !Int, !Int, !Int, !Int, !PIC)
winGetPicFontInfo _
	= code
	{
		.inline WinGetPicFontInfo
			ccall WinGetPicFontInfo "II-IIIIII"
		.end
	}

winGetFontInfo ::  !Fnt !Int !HDC !*OSToolbox -> ( !Int, !Int, !Int, !Int, !*OSToolbox)
winGetFontInfo _ _ _ _
	= code
	{
		.inline WinGetFontInfo
			ccall WinGetFontInfo "SIIIII-IIIII"
		.end
	}

winSetFontStyle ::  !Int !PIC ->  PIC
winSetFontStyle _ _
	= code
	{
		.inline WinSetFontStyle
			ccall WinSetFontStyle "III-II"
		.end
	}

winSetFontSize ::  !Int !PIC ->  PIC
winSetFontSize _ _
	= code
	{
		.inline WinSetFontSize
			ccall WinSetFontSize "III-II"
		.end
	}

winSetFontName ::  !{#Char} !PIC ->  PIC
winSetFontName _ _
	= code
	{
		.inline WinSetFontName
			ccall WinSetFontName "SII-II"
		.end
	}

winSetFont ::  !Fnt !PIC ->  PIC
winSetFont _ _
	= code
	{
		.inline WinSetFont
			ccall WinSetFont "SIIII-II"
		.end
	}


winPrintResizedBitmap :: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !{#Char} !PIC -> PIC
winPrintResizedBitmap _ _ _ _ _
	= code
	{
		.inline WinPrintResizedBitmap
			ccall WinPrintResizedBitmap "IIIIIISII-II"
		.end
	}

//	PA: Routines to DRAW bitmaps.
winDrawBitmap :: !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
winDrawBitmap _ _ _ _
	= code
	{
		.inline WinDrawBitmap
			ccall WinDrawBitmap "IIIIIII-II"
		.end
	}

winDrawResizedBitmap :: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
winDrawResizedBitmap _ _ _ _ _
	= code
	{
		.inline WinDrawResizedBitmap
			ccall WinDrawResizedBitmap "IIIIIIIII-II"
		.end
	}

winCreateBitmap :: !Int !{#Char} !HDC !*OSToolbox -> (!Int,!*OSToolbox)
winCreateBitmap _ _ _ _
	= code
	{
		.inline WinCreateBitmap
			ccall WinCreateBitmap "ISII-II"
		.end
	}

winInvertPolygon ::  !PIC ->  PIC
winInvertPolygon _
	= code
	{
		.inline WinInvertPolygon
			ccall WinInvertPolygon "II-II"
		.end
	}

winErasePolygon ::  !PIC ->  PIC
winErasePolygon _
	= code
	{
		.inline WinErasePolygon
			ccall WinErasePolygon "II-II"
		.end
	}

winFillPolygon ::  !PIC ->  PIC
winFillPolygon _
	= code
	{
		.inline WinFillPolygon
			ccall WinFillPolygon "II-II"
		.end
	}

winDrawPolygon ::  !PIC ->  PIC
winDrawPolygon _
	= code
	{
		.inline WinDrawPolygon
			ccall WinDrawPolygon "II-II"
		.end
	}

winAddPolygonPoint ::  !Pt !*OSToolbox ->  *OSToolbox
winAddPolygonPoint _ _
	= code
	{
		.inline WinAddPolygonPoint
			ccall WinAddPolygonPoint "III-I"
		.end
	}

winStartPolygon ::  !Int !*OSToolbox ->  *OSToolbox
winStartPolygon _ _
	= code
	{
		.inline WinStartPolygon
			ccall WinStartPolygon "II-I"
		.end
	}

winEndPolygon :: !*OSToolbox -> *OSToolbox
winEndPolygon _
	= code
	{	
		.inline WinEndPolygon
			ccall WinEndPolygon "I-I"
		.end
	}

/*	Operations to create, modify, and destroy polygon shapes.
*/
winAllocPolyShape :: !Int !*OSToolbox -> (!Int,!*OSToolbox)
winAllocPolyShape _ _
	= code
	{	
		.inline WinAllocPolyShape
			ccall WinAllocPolyShape "II-II"
		.end
	}

winSetPolyPoint :: !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
winSetPolyPoint _ _ _ _ _
	= code
	{	
		.inline WinSetPolyPoint
			ccall WinSetPolyPoint "IIIII-I"
		.end
	}

winFreePolyShape :: !Int !*OSToolbox -> *OSToolbox
winFreePolyShape _ _
	= code
	{	
		.inline WinFreePolyShape
			ccall WinFreePolyShape "II-I"
		.end
	}


winInvertWedge ::  !OSRect !Pt !Pt !PIC ->  PIC
winInvertWedge _ _ _ _
	= code
	{
		.inline WinInvertWedge
			ccall WinInvertWedge "IIIIIIIIII-II"
		.end
	}

winEraseWedge ::  !OSRect !Pt !Pt !PIC ->  PIC
winEraseWedge _ _ _ _
	= code
	{
		.inline WinEraseWedge
			ccall WinEraseWedge "IIIIIIIIII-II"
		.end
	}

winFillWedge ::  !OSRect !Pt !Pt !PIC ->  PIC
winFillWedge _ _ _ _
	= code
	{
		.inline WinFillWedge
			ccall WinFillWedge "IIIIIIIIII-II"
		.end
	}

winDrawWedge ::  !OSRect !Pt !Pt !PIC ->  PIC
winDrawWedge _ _ _ _
	= code
	{
		.inline WinDrawWedge
			ccall WinDrawWedge "IIIIIIIIII-II"
		.end
	}


winInvertCircle ::  !Pt !Int !PIC ->  PIC
winInvertCircle _ _ _
	= code
	{
		.inline WinInvertCircle
			ccall WinInvertCircle "IIIII-II"
		.end
	}

winEraseCircle ::  !Pt !Int !PIC ->  PIC
winEraseCircle _ _ _
	= code
	{
		.inline WinEraseCircle
			ccall WinEraseCircle "IIIII-II"
		.end
	}

winFillCircle ::  !Pt !Int !PIC ->  PIC
winFillCircle _ _ _
	= code
	{
		.inline WinFillCircle
			ccall WinFillCircle "IIIII-II"
		.end
	}

winDrawCircle ::  !Pt !Int !PIC ->  PIC
winDrawCircle _ _ _
	= code
	{
		.inline WinDrawCircle
			ccall WinDrawCircle "IIIII-II"
		.end
	}


winInvertOval ::  !OSRect !PIC ->  PIC
winInvertOval _ _
	= code
	{
		.inline WinInvertOval
			ccall WinInvertOval "IIIIII-II"
		.end
	}

winEraseOval ::  !OSRect !PIC ->  PIC
winEraseOval _ _
	= code
	{
		.inline WinEraseOval
			ccall WinEraseOval "IIIIII-II"
		.end
	}

winFillOval ::  !OSRect !PIC ->  PIC
winFillOval _ _
	= code
	{
		.inline WinFillOval
			ccall WinFillOval "IIIIII-II"
		.end
	}

winDrawOval ::  !OSRect !PIC ->  PIC
winDrawOval _ _
	= code
	{
		.inline WinDrawOval
			ccall WinDrawOval "IIIIII-II"
		.end
	}


winInvertRoundRectangle ::  !OSRect !Int !Int !PIC ->  PIC
winInvertRoundRectangle _ _ _ _
	= code
	{
		.inline WinInvertRoundRectangle
			ccall WinInvertRoundRectangle "IIIIIIII-II"
		.end
	}

winEraseRoundRectangle ::  !OSRect !Int !Int !PIC ->  PIC
winEraseRoundRectangle _ _ _ _
	= code
	{
		.inline WinEraseRoundRectangle
			ccall WinEraseRoundRectangle "IIIIIIII-II"
		.end
	}

winFillRoundRectangle ::  !OSRect !Int !Int !PIC ->  PIC
winFillRoundRectangle _ _ _ _
	= code
	{
		.inline WinFillRoundRectangle
			ccall WinFillRoundRectangle "IIIIIIII-II"
		.end
	}

winDrawRoundRectangle ::  !OSRect !Int !Int !PIC ->  PIC
winDrawRoundRectangle _ _ _ _
	= code
	{
		.inline WinDrawRoundRectangle
			ccall WinDrawRoundRectangle "IIIIIIII-II"
		.end
	}


winScrollRectangle :: !OSRect !Pt !PIC -> (!OSRect,!PIC)
winScrollRectangle _ _ _
	= code
	{
		.inline WinScrollRectangle
			ccall WinScrollRectangle "IIIIIIII-IIIIII"
		.end
	}

winScrollRectangle2 :: !OSRect !Pt !PIC -> (!OSRect,!PIC)
winScrollRectangle2 _ _ _
	= code
	{
		.inline WinScrollRectangle2
			ccall WinScrollRectangle2 "IIIIIIII-IIIIII"
		.end
	}

winCopyRectangle ::  !OSRect !Pt !PIC ->  PIC
winCopyRectangle _ _ _
	= code
	{
		.inline WinCopyRectangle
			ccall WinCopyRectangle "IIIIIIII-II"
		.end
	}

winCopyRectangleTo ::  !OSRect !Pt !PIC ->  PIC
winCopyRectangleTo _ _ _
	= code
	{
		.inline WinCopyRectangleTo
			ccall WinCopyRectangleTo "IIIIIIII-II"
		.end
	}

winMoveRectangle ::  !OSRect !Pt !PIC ->  PIC
winMoveRectangle _ _ _
	= code
	{
		.inline WinMoveRectangle
			ccall WinMoveRectangle "IIIIIIII-II"
		.end
	}

winMoveRectangleTo ::  !OSRect !Pt !PIC ->  PIC
winMoveRectangleTo _ _ _
	= code
	{
		.inline WinMoveRectangleTo
			ccall WinMoveRectangleTo "IIIIIIII-II"
		.end
	}


winInvertRectangle ::  !OSRect !PIC ->  PIC
winInvertRectangle _ _
	= code
	{
		.inline WinInvertRectangle
			ccall WinInvertRectangle "IIIIII-II"
		.end
	}

winEraseRectangle ::  !OSRect !PIC ->  PIC
winEraseRectangle _ _
	= code
	{
		.inline WinEraseRectangle
			ccall WinEraseRectangle "IIIIII-II"
		.end
	}

winFillRectangle ::  !OSRect !PIC ->  PIC
winFillRectangle _ _
	= code
	{
		.inline WinFillRectangle
			ccall WinFillRectangle "IIIIII-II"
		.end
	}

winDrawRectangle ::  !OSRect !PIC ->  PIC
winDrawRectangle _ _
	= code
	{
		.inline WinDrawRectangle
			ccall WinDrawRectangle "IIIIII-II"
		.end
	}


winDrawChar ::  !Int !PIC ->  PIC
winDrawChar _ _
	= code
	{
		.inline WinDrawChar
			ccall WinDrawChar "III-II"
		.end
	}

winDrawString ::  !{#Char} !PIC ->  PIC
winDrawString _ _
	= code
	{
		.inline WinDrawString
			ccall WinDrawString "SII-II"
		.end
	}


winDrawCCurve ::  !OSRect !Pt !Pt !RGBcolor !PIC ->  PIC
winDrawCCurve _ _ _ _ _
	= code
	{
		.inline WinDrawCCurve
			ccall WinDrawCCurve "IIIIIIIIIIIII-II"
		.end
	}

winDrawCLine ::  !Pt !Pt !RGBcolor !PIC ->  PIC
winDrawCLine _ _ _ _
	= code
	{
		.inline WinDrawCLine
			ccall WinDrawCLine "IIIIIIIII-II"
		.end
	}

winDrawCPoint ::  !Pt !RGBcolor !PIC ->  PIC
winDrawCPoint _ _ _
	= code
	{
		.inline WinDrawCPoint
			ccall WinDrawCPoint "IIIIIII-II"
		.end
	}

winDrawCurve ::  !OSRect !Pt !Pt !PIC ->  PIC
winDrawCurve _ _ _ _
	= code
	{
		.inline WinDrawCurve
			ccall WinDrawCurve "IIIIIIIIII-II"
		.end
	}

winDrawLine ::  !Pt !Pt !PIC ->  PIC
winDrawLine _ _ _
	= code
	{
		.inline WinDrawLine
			ccall WinDrawLine "IIIIII-II"
		.end
	}

winDrawPoint ::  !Pt !PIC ->  PIC
winDrawPoint _ _
	= code
	{
		.inline WinDrawPoint
			ccall WinDrawPoint "IIII-II"
		.end
	}


winLinePen ::  !Pt !PIC ->  PIC
winLinePen _ _
	= code
	{
		.inline WinLinePen
			ccall WinLinePen "IIII-II"
		.end
	}

winLinePenTo ::  !Pt !PIC ->  PIC
winLinePenTo _ _
	= code
	{
		.inline WinLinePenTo
			ccall WinLinePenTo "IIII-II"
		.end
	}

winMovePen ::  !Pt !PIC ->  PIC
winMovePen _ _
	= code
	{
		.inline WinMovePen
			ccall WinMovePen "IIII-II"
		.end
	}

winMovePenTo ::  !Pt !PIC ->  PIC
winMovePenTo _ _
	= code
	{
		.inline WinMovePenTo
			ccall WinMovePenTo "IIII-II"
		.end
	}

winGetPenPos :: !PIC -> (!Int,!Int,!HDC,!*OSToolbox)
winGetPenPos _
	= code
	{	
		.inline WinGetPenPos
			ccall WinGetPenPos "II-IIII"
		.end
	}


winSetPenSize ::  !Int !PIC ->  PIC
winSetPenSize _ _
	= code
	{
		.inline WinSetPenSize
			ccall WinSetPenSize "III-II"
		.end
	}

winSetPattern ::  !Int !PIC ->  PIC
winSetPattern _ _
	= code
	{
		.inline WinSetPattern
			ccall WinSetPattern "III-II"
		.end
	}

winSetMode ::  !Int !PIC ->  PIC
winSetMode _ _
	= code
	{
		.inline WinSetMode
			ccall WinSetMode "III-II"
		.end
	}

winSetBackColor ::  !RGBcolor !PIC ->  PIC
winSetBackColor _ _
	= code
	{
		.inline WinSetBackColor
			ccall WinSetBackColor "IIIII-II"
		.end
	}

winSetPenColor ::  !RGBcolor !PIC ->  PIC
winSetPenColor _ _
	= code
	{
		.inline WinSetPenColor
			ccall WinSetPenColor "IIIII-II"
		.end
	}


winClipPicture ::  !OSRect !PIC ->  PIC
winClipPicture _ _
	= code
	{
		.inline WinClipPicture
			ccall WinClipPicture "IIIIII-II"
		.end
	}

//	PA: operation to set the clipping region.
winClipRgnPicture :: !HRGN !PIC -> PIC
winClipRgnPicture _ _
	= code
	{	
		.inline WinClipRgnPicture
			ccall WinClipRgnPicture "III-II"
		.end
	}

//	PA+++: new operation to set the complete clipping region.
winSetClipRgnPicture :: !HRGN !PIC -> PIC
winSetClipRgnPicture _ _
	= code
	{
		.inline WinSetClipRgnPicture
			ccall WinSetClipRgnPicture "III-II"
		.end
	}

//	PA+++: new operation to retrieve the current clipping region.
winGetClipRgnPicture :: !PIC -> (!HRGN,!PIC)
winGetClipRgnPicture _
	= code
	{
		.inline WinGetClipRgnPicture
			ccall WinGetClipRgnPicture "II-III"
		.end
	}

winDeleteObject :: !Int !*OSToolbox -> *OSToolbox
winDeleteObject _ _
	= code
	{	
		.inline WinDeleteObject
			ccall WinDeleteObject "II-I"
		.end
	}


winDonePicture :: !PIC -> (!Int,!Int,!RGBcolor,!RGBcolor,!Pt,!Fnt,!PIC)
winDonePicture _
	= code
	{
		.inline WinDonePicture
			ccall WinDonePicture "II-IIIIIIIIIISIIII"
		.end
	}

winInitPicture :: !Int !Int !RGBcolor !RGBcolor !Pt !Fnt !Pt !PIC -> PIC
winInitPicture _ _ _ _ _ _ _ _
	= code
	{
		.inline WinInitPicture
			ccall WinInitPicture "IIIIIIIIIISIIIIII-II"
		.end
	}
