definition module pictCCall_12


from	rgnCCall_12	import :: HRGN
from	ostoolbox	import :: OSToolbox
from	ostypes		import :: OSRect, :: HDC


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


/*	win(Create/Destroy)ScreenHDC added to temporarily create a HDC of a screen.
	Never use these values for a window or control.
*/
winCreateScreenHDC		:: !*OSToolbox -> PIC
winDestroyScreenHDC		:: !PIC -> *OSToolbox

winGetPicStringWidth	:: !{#Char} !PIC -> ( !Int, !PIC)
winGetPicCharWidth		:: !Char !PIC -> ( !Int, !PIC)
winGetStringWidth		:: !{#Char} !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)
winGetCharWidth			:: !Char !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)

winGetPicFontInfo		:: !PIC -> ( !Int, !Int, !Int, !Int, !PIC)
winGetFontInfo			:: !Fnt !Int !HDC !*OSToolbox -> ( !Int, !Int, !Int, !Int, !*OSToolbox)
winSetFontStyle			:: !Int !PIC ->  PIC
winSetFontSize			:: !Int !PIC ->  PIC
winSetFontName			:: !{#Char} !PIC ->  PIC
winSetFont				:: !Fnt !PIC ->  PIC

/*	Routines to PRINT bitmaps (winPrint(Resized)Bitmap).
	Routines to DRAW  bitmaps (winDraw(Resized)Bitmap).
	Create a bitmap (winCreateBitmap).
*/
// MW11 winPrintBitmap			:: !(!Int,!Int) !(!Int,!Int) !{#Char} !PIC -> PIC
winPrintResizedBitmap	:: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !{#Char} !PIC -> PIC
winDrawBitmap			:: !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
winDrawResizedBitmap	:: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
winCreateBitmap			:: !Int !{#Char} !HDC !*OSToolbox -> (!Int,!*OSToolbox)


winInvertPolygon		:: !PIC ->  PIC
winErasePolygon			:: !PIC ->  PIC
winFillPolygon			:: !PIC ->  PIC
winDrawPolygon			:: !PIC ->  PIC
winAddPolygonPoint		:: !Pt !*OSToolbox ->  *OSToolbox
winStartPolygon			:: !Int !*OSToolbox ->  *OSToolbox
winEndPolygon			:: !*OSToolbox -> *OSToolbox

winAllocPolyShape		:: !Int !*OSToolbox -> (!Int,!*OSToolbox)
winSetPolyPoint			:: !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
winFreePolyShape		:: !Int !*OSToolbox -> *OSToolbox


winInvertWedge			:: !OSRect !Pt !Pt !PIC ->  PIC
winEraseWedge			:: !OSRect !Pt !Pt !PIC ->  PIC
winFillWedge			:: !OSRect !Pt !Pt !PIC ->  PIC
winDrawWedge			:: !OSRect !Pt !Pt !PIC ->  PIC


winInvertCircle			:: !Pt !Int !PIC ->  PIC
winEraseCircle			:: !Pt !Int !PIC ->  PIC
winFillCircle			:: !Pt !Int !PIC ->  PIC
winDrawCircle			:: !Pt !Int !PIC ->  PIC


winInvertOval			:: !OSRect !PIC ->  PIC
winEraseOval			:: !OSRect !PIC ->  PIC
winFillOval				:: !OSRect !PIC ->  PIC
winDrawOval				:: !OSRect !PIC ->  PIC


winInvertRoundRectangle	:: !OSRect !Int !Int !PIC ->  PIC
winEraseRoundRectangle	:: !OSRect !Int !Int !PIC ->  PIC
winFillRoundRectangle	:: !OSRect !Int !Int !PIC ->  PIC
winDrawRoundRectangle	:: !OSRect !Int !Int !PIC ->  PIC


winScrollRectangle		:: !OSRect !Pt !PIC -> (!OSRect,!PIC)
winScrollRectangle2		:: !OSRect !Pt !PIC -> (!OSRect,!PIC)
winCopyRectangle		:: !OSRect !Pt !PIC ->  PIC
winCopyRectangleTo		:: !OSRect !Pt !PIC ->  PIC
winMoveRectangle		:: !OSRect !Pt !PIC ->  PIC
winMoveRectangleTo		:: !OSRect !Pt !PIC ->  PIC


winInvertRectangle		:: !OSRect !PIC ->  PIC
winEraseRectangle		:: !OSRect !PIC ->  PIC
winFillRectangle		:: !OSRect !PIC ->  PIC
winDrawRectangle		:: !OSRect !PIC ->  PIC


winDrawChar				:: !Int !PIC ->  PIC
winDrawString			:: !{#Char} !PIC ->  PIC


winDrawCCurve			:: !OSRect !Pt !Pt !RGBcolor !PIC ->  PIC
winDrawCLine			:: !Pt !Pt !RGBcolor !PIC ->  PIC
winDrawCPoint			:: !Pt !RGBcolor !PIC ->  PIC
winDrawCurve			:: !OSRect !Pt !Pt !PIC ->  PIC
winDrawLine				:: !Pt !Pt !PIC ->  PIC
winDrawPoint			:: !Pt !PIC ->  PIC


winLinePen				:: !Pt !PIC ->  PIC
winLinePenTo			:: !Pt !PIC ->  PIC

winMovePen				:: !Pt !PIC ->  PIC
winMovePenTo			:: !Pt !PIC ->  PIC
winGetPenPos			:: !PIC -> (!Int,!Int,!HDC,!*OSToolbox)

winSetPenSize			:: !Int !PIC ->  PIC
winSetPattern			:: !Int !PIC ->  PIC
winSetMode				:: !Int !PIC ->  PIC
winSetBackColor			:: !RGBcolor !PIC ->  PIC
winSetPenColor			:: !RGBcolor !PIC ->  PIC

winClipPicture			:: !OSRect !PIC ->  PIC
winClipRgnPicture		:: !HRGN !PIC -> PIC			//	Operation to set the clipping region
winSetClipRgnPicture	:: !HRGN !PIC -> PIC			//	Operation to completely set the clipping region
winGetClipRgnPicture	::       !PIC -> (!HRGN,!PIC)	//  Operation to retrieve the current clipping region

winDeleteObject			:: !Int !*OSToolbox -> *OSToolbox

winDonePicture			:: !PIC -> ( !Int, !Int, !RGBcolor, !RGBcolor, !Pt, !Fnt, !PIC)
winInitPicture			:: !Int !Int !RGBcolor !RGBcolor !Pt !Fnt !Pt !PIC ->  PIC
