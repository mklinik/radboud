definition module ospicture


//	Clean Object I/O library, version 1.2

/*	Drawing functions and other operations on Pictures. 
*/

import	osrgn, ostypes
from	StdFunc			import :: St
from	osfont			import :: Font
from	ostoolbox		import :: OSToolbox
import	StdPictureDef


::	Picture
::	Origin
	:==	Point2
/*	PA: moved to ostypes
::	OSPictContext
	:==	HDC
*/
::  Pen
	=	{	penSize			:: !Int				// The width and height of the pen
  		,	penForeColour	:: !Colour			// The drawing colour of the pen
		,	penBackColour	:: !Colour			// The background colour of the pen
		,	penPos			:: !.Point2			// The pen position in local coordinates
		,	penFont			:: !Font			// The font information to draw text and characters
		}

/*	Conversion operations to and from Picture.
	To open/close a picture context:
		packPicture   creates a Picture, given the proper OS resources. The picture is initialised.
		unpackPicture unpacks a Picture, releasing the proper OS resources.
	To open/close an existing picture:
		peekPicture   gives you the components of a Picture
		unpeekPicture restores the components to a Picture
	To obtain the graphics context of an existing picture:
		peekOSPictContext
	To obtain a read-only picture:
		sharePicture  creates a copy of the Picture. This copy does not occupy OS resources.
	To obtain temporary access to the screen:
		peekScreen    creates a window picture, applies the argument function to it, and releases the OS resources. 
*/
packPicture				:: !Origin !*Pen !Bool !OSPictContext !*OSToolbox -> *Picture
unpeekPicture			:: !Origin !*Pen !Bool !OSPictContext !*OSToolbox -> *Picture
unpackPicture			:: !*Picture -> (!Origin,!*Pen,!Bool,!OSPictContext,!*OSToolbox)
peekPicture				:: !*Picture -> (!Origin,!*Pen,!Bool,!OSPictContext,!*OSToolbox)
peekOSPictContext		:: !*Picture -> (!OSPictContext,!*Picture)
sharePicture			:: !*Picture -> (!Picture,      !*Picture)
peekScreen				:: !.(St *Picture .x) !*OSToolbox -> (!.x,!*OSToolbox)

defaultPen				:: *Pen		// The Pen for customised drawing operations
dialogPen				:: *Pen		// The Pen for system     drawing operations
setPenAttribute			:: !PenAttribute !u:Pen -> u:Pen
sharePen				:: !*Pen -> (!Pen,!*Pen)
copyPen					:: ! Pen -> *Pen


//	Picture interface functions.
apppicttoolbox			:: !(IdFun *OSToolbox)		!*Picture -> *Picture
accpicttoolbox			:: !(St *OSToolbox .x)		!*Picture -> (!.x,!*Picture)


/*	Attribute functions.
*/
//	Access to Origin and Pen:
setpictpen				:: !Pen						!*Picture -> *Picture
getpictpen				::							!*Picture -> (!Pen,   !*Picture)
setpictorigin			:: !Origin					!*Picture -> *Picture
getpictorigin			::							!*Picture -> (!Origin,!*Picture)

//	PenPos attributes:
setpictpenpos			:: !Point2					!*Picture -> *Picture
getpictpenpos			::							!*Picture -> (!Point2,!*Picture)
movepictpenpos			:: !Vector2					!*Picture -> *Picture
//	Move the pen position over the given vector

//	PenSize attributes:
setpictpensize			:: !Int						!*Picture -> *Picture 	
getpictpensize			::							!*Picture -> (!Int,!*Picture)

//	PenColour attributes:
setpictpencolour		:: !Colour					!*Picture -> *Picture
setpictbackcolour		:: !Colour					!*Picture -> *Picture
getpictpencolour		::							!*Picture -> (!Colour,!*Picture)
getpictbackcolour		::							!*Picture -> (!Colour,!*Picture)
toRGBtriple				:: !Colour -> (!Int,!Int,!Int)

//	PenFont attributes:
setpictpenfont			:: !Font					!*Picture -> *Picture
getpictpenfont			::							!*Picture -> (!Font,!*Picture)
setpictpendefaultfont	::							!*Picture -> *Picture
//	setpictpendefaultfont opens and sets the defaultFont (see StdFont).


/*	Drawing mode setting functions.
*/
setpictxormode			::							!*Picture -> *Picture
setpicthilitemode		::							!*Picture -> *Picture
setpictnormalmode		::							!*Picture -> *Picture


/*	Point2 drawing operations.
	pictdrawpoint
		only draws a point at that position. The pen position is not changed.
*/
pictdrawpoint			:: !Point2					!*Picture -> *Picture


/*	Line drawing operations.
	pictdrawlineto
		draws a line from the current pen position to the given pen position. 
		The new pen position is the endpoint of the line.	
	pictdrawline
		draws a line from the first point to the second point. The pen position
		is not changed.
	pictundrawlineto
		is the same as pictdrawlineto except that temporarily the background colour is used.
	pictundrawline
		is the same as pictdrawline except that temporarily the background colour is used.
*/
pictdrawlineto			:: !Point2					!*Picture -> *Picture
pictdrawline			:: !Point2 !Point2			!*Picture -> *Picture
pictundrawlineto		:: !Point2					!*Picture -> *Picture
pictundrawline			:: !Point2 !Point2			!*Picture -> *Picture


/*	Text drawing operations.
	pictdraw(char/string) draws a char/string at the current pen position. The new
		pen position is immediately after the drawn char/string.
*/
pictdrawchar			:: !Char					!*Picture -> *Picture
pictundrawchar			:: !Char					!*Picture -> *Picture
pictdrawstring			:: !String					!*Picture -> *Picture
pictundrawstring		:: !String					!*Picture -> *Picture


/*	Oval drawing operations.
	pict(draw/fill)oval center oval 
		draws/fills an oval at center with horizontal and vertical radius. The new
		pen position is not changed.
	pict(undraw/unfill)oval center oval
		draw/fill an oval at center with horizontal and vertical radius using the
		background colour of the picture.
*/
pictdrawoval			:: !Point2 !Oval			!*Picture -> *Picture
pictfilloval			:: !Point2 !Oval			!*Picture -> *Picture
pictundrawoval			:: !Point2 !Oval			!*Picture -> *Picture
pictunfilloval			:: !Point2 !Oval			!*Picture -> *Picture


/*	Curve drawing operations.
	pict(draw/fill)curve movePen point curve
		draws/fills a curve starting at point with a shape defined by curve. If movePen
		is True, then the new pen position is at the end of the curve, otherwise it does
		not change.
	pictun(draw/fill)curve 
		is equal to pict(draw/fill)curve, using the background colour temporarily.
	getcurve_rect_begin_end point curve
		returns the enclosing rect of the curve and begin and end point lying on that
		curve.
*/
pictdrawcurve			:: !Bool !Point2 !Curve		!*Picture -> *Picture
pictundrawcurve			:: !Bool !Point2 !Curve		!*Picture -> *Picture
pictfillcurve			:: !Bool !Point2 !Curve		!*Picture -> *Picture
pictunfillcurve			:: !Bool !Point2 !Curve		!*Picture -> *Picture
getcurve_rect_begin_end	::       !Point2 !Curve -> (!OSRect,!Point2,!Point2)


/*	OSRect drawing operations.
	pict(draw/fill)rect rect
		draws/fills a rect. The pen position is not changed.
	pictun(draw/fill)rect 
		is equal to pict(draw/fill)rect, using the background colour temporarily.
*/
pictdrawrect			:: !OSRect	!*Picture -> *Picture
pictundrawrect			:: !OSRect	!*Picture -> *Picture
pictfillrect			:: !OSRect	!*Picture -> *Picture
pictunfillrect			:: !OSRect	!*Picture -> *Picture


/*	Scrolling operation (handle with care).
*/
pictscroll				:: !OSRect !Vector2 !*Picture -> (!OSRect,!*Picture)
pictscroll2				:: !OSRect !Vector2 !*Picture -> (!OSRect,!*Picture)


/*	Polygon drawing operations.
	pict(draw/fill)polygon point polygon
		draws/fills a polygon starting at point. The pen position is not changed.
	pictun(draw/fill)polygon
		is equal to pict(draw/fill)polygon, using the background colour temporarily.
*/
pictdrawpolygon			:: !Point2 !Polygon			!*Picture -> *Picture
pictundrawpolygon		:: !Point2 !Polygon			!*Picture -> *Picture
pictfillpolygon			:: !Point2 !Polygon			!*Picture -> *Picture
pictunfillpolygon		:: !Point2 !Polygon			!*Picture -> *Picture


/*	Clipping operations.
	pictgetcliprgn gets the current clipping region.
	pictsetcliprgn sets the given clipping region.
	pictandcliprgn takes the intersection of the current clipping region and the argument region.
*/
pictgetcliprgn			::							!*Picture -> (!OSRgnHandle,!*Picture)
pictsetcliprgn			:: !OSRgnHandle				!*Picture -> *Picture
pictandcliprgn			:: !OSRgnHandle				!*Picture -> *Picture


/*	Resolution access function (added by MW):
*/
getResolutionC :: !OSPictContext !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)

// MW: scaling of screen coordinates to printer coordinates.
getPictureScalingFactors:: !OSPictContext !*OSToolbox -> (!(!Int,!Int),!(!Int,!Int),!OSPictContext,!*OSToolbox)

getpictpenattributes	:: !*Picture	-> (![PenAttribute],!*Picture)
getPenPenPos			:: !*Pen		-> (!Point2,!*Pen)
