definition module StdPicture


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdPicture contains the drawing operations and access to Pictures.
//	********************************************************************************


from	StdFunc		import :: St
from	osfont		import :: Font
from	ospicture	import :: Picture
import	StdPictureDef


//	Pen attribute functions:

setPenAttributes		:: ![PenAttribute]		!*Picture -> *Picture
getPenAttributes		::						!*Picture
							-> (![PenAttribute],!*Picture)

//	Pen position attributes:
setPenPos				:: !Point2				!*Picture -> *Picture
getPenPos				::						!*Picture -> (!Point2,!*Picture)

class movePenPos figure	:: !figure				!*Picture -> *Picture
//	Move the pen position as much as when drawing the figure.
instance movePenPos Vector2
instance movePenPos Curve

//	Pen size attributes:
setPenSize				:: !Int					!*Picture -> *Picture 	
getPenSize				::						!*Picture -> (!Int,!*Picture)

setDefaultPenSize		::						!*Picture -> *Picture
//	setDefaultPenSize = setPenSize 1

//	Pen colour attributes:
setPenColour			:: !Colour				!*Picture -> *Picture
getPenColour			::						!*Picture -> (!Colour,!*Picture)
setPenBack				:: !Colour				!*Picture -> *Picture
getPenBack				::						!*Picture -> (!Colour,!*Picture)

setDefaultPenColour		::						!*Picture -> *Picture
setDefaultPenBack		::						!*Picture -> *Picture

toRGBColour				:: !Colour -> RGBColour	// Convert a colour to RGBColour

//	setDefaultPenColour = setPenColour Black
//	setDefaultPenBack   = setPenBack   White

//	Pen font attributes:
setPenFont				:: !Font				!*Picture -> *Picture
getPenFont				::						!*Picture -> (!Font,!*Picture)

setDefaultPenFont		::						!*Picture -> *Picture


/*	Font operations:
*/
openFont		:: !FontDef	!*Picture -> (!(!Bool,!Font),!*Picture)
openDefaultFont	::			!*Picture -> (!Font,		 !*Picture)
openDialogFont	::			!*Picture -> (!Font,		 !*Picture)
/*	openFont
		creates the font as specified by the name, stylistic variations, and size. 
		The Boolean result is True only if the font is available and need not be 
		scaled. 
		In all other cases, an existing font is returned (depending on the system).
	openDefaultFont
		returns the font used by default by applications. 
	openDialogFont
		returns the font used by default by the system.
*/

getFontNames	::						!*Picture -> (![FontName],	 !*Picture)
getFontStyles	::			 !FontName	!*Picture -> (![FontStyle],	 !*Picture)
getFontSizes	:: !Int !Int !FontName	!*Picture -> (![FontSize],	 !*Picture)
/*	getFontNames
		returns the FontNames  of all available fonts.
	getFontStyles
		returns the FontStyles of all available styles of a particular FontName.
	getFontSizes
		returns all FontSizes in increasing order of a particular FontName that are 
		available without scaling. The sizes inspected are inclusive between the two
		Integer arguments. (Negative values are set to zero.) 
		In case the requested font is unavailable, the styles or sizes of the 
		default font are returned.
*/

getFontDef :: !Font -> FontDef
/*	getFontDef returns the name, stylistic variations and size of the argument Font.
*/

getPenFontCharWidth		::       ! Char		!*Picture -> (! Int,		!*Picture)
getPenFontCharWidths	::       ![Char]	!*Picture -> (![Int],		!*Picture)
getPenFontStringWidth	::       ! String	!*Picture -> (! Int,		!*Picture)
getPenFontStringWidths	::       ![String]	!*Picture -> (![Int],		!*Picture)
getPenFontMetrics		::					!*Picture -> (!FontMetrics,	!*Picture)

getFontCharWidth		:: !Font ! Char		!*Picture -> (!Int,			!*Picture)
getFontCharWidths		:: !Font ![Char]	!*Picture -> (![Int],		!*Picture)
getFontStringWidth		:: !Font ! String	!*Picture -> (!Int,			!*Picture)
getFontStringWidths		:: !Font ![String]	!*Picture -> (![Int],		!*Picture)
getFontMetrics			:: !Font			!*Picture -> (!FontMetrics,	!*Picture)
/*	get(Pen)Font(Char/String)Width(s)
		return the width of the argument (Char/String)(s) given the Font argument
		or current PenFont attribute.
	get(Pen)FontMetrics
		returns the FontMetrics of the Font argument or current PenFont attribute.
*/


/*	Region functions.
	A Region is defined by a collection of shapes.
*/
::	Region

//	Basic access functions on Regions:

isEmptyRegion	:: !Region -> Bool
getRegionBound	:: !Region -> Rectangle
/*	isEmptyRegion
		holds if the argument region covers no pixels (it is empty).
	getRegionBound
		returns the smallest enclosing rectangle of the argument region.
		If the region is empty, zero is returned.
*/

//	Constructing a region:

class toRegion area :: !area -> Region

::	PolygonAt
	=	{	polygon_pos	:: !Point2
		,	polygon		:: !Polygon
		}

instance toRegion Rectangle
instance toRegion PolygonAt
instance toRegion [area]			| toRegion area
instance toRegion (:^: area1 area2)	| toRegion area1 & toRegion area2


//	Drawing and restoring picture attributes:

appPicture		:: !.(IdFun *Picture) !*Picture -> *Picture
accPicture		:: !.(St *Picture .x) !*Picture -> (.x,!*Picture)
/*	(app/acc)Picture f pict
		apply f to pict. After drawing,  the picture attributes of the result
		picture are restored to those of pict.
*/


//	Drawing within in a clipping region:

appClipPicture :: !Region !.(IdFun *Picture) !*Picture -> *Picture
accClipPicture :: !Region !.(St *Picture .x) !*Picture -> (.x,!*Picture)


//	Drawing in 'exclusive or' mode:

appXorPicture	:: !.(IdFun *Picture) !*Picture -> *Picture
accXorPicture	:: !.(St *Picture .x) !*Picture -> (.x,!*Picture)
/*	(app/acc)XorPicture f pict
		apply f to pict in the appropriate platform xor mode. 
*/


//	Drawing in 'hilite' mode:

class Hilites figure where
	hilite	::         !figure !*Picture -> *Picture
	hiliteAt:: !Point2 !figure !*Picture -> *Picture
/*	hilite
		draws figures in the appropriate 'hilite' mode at the current  pen position.
	hiliteAt
		draws figures in the appropriate 'hilite' mode at the argument pen position.
	Both functions reset the 'hilite' mode after drawing.
*/

instance Hilites Box		// Hilite a box
instance Hilites Rectangle	// Hilite a rectangle (note: hiliteAt pos r = hilite r)


//	Drawing points:

drawPoint		::                !*Picture -> *Picture
drawPointAt		:: !Point2        !*Picture -> *Picture
/*	drawPoint
		plots a point at the current  pen position p and moves to p+{vx=1,vy=0}
	drawPointAt
		plots a point at the argument pen position, but retains the pen position.
*/

//	Drawing lines:

drawLineTo		:: !Point2         !*Picture -> *Picture
drawLine		:: !Point2 !Point2 !*Picture -> *Picture
/*	drawLineTo
		draws a line from the current pen position to the argument point which
		becomes the new pen position.
	drawLine
		draws a line between the two argument points, but retains the pen position.
*/


/*	Drawing and filling operations.
	These functions are divided into the following classes:
	Drawables:
		draw     'line-oriented' figures at the current  pen position.
		drawAt   'line-oriented' figures at the argument pen position.
		undraw     f = appPicture (draw     f o setPenColour background)
		undrawAt x f = appPicture (drawAt x f o setPenColour background)
	Fillables:
		fill     'area-oriented' figures at the current  pen position.
		fillAt   'area-oriented' figures at the argument pen position.
		unfill     f = appPicture (fill     f o setPenColour background)
		unfillAt x f = appPicture (fillAt x f o setPenColour background)
*/
class Drawables figure where
	draw	::         !figure !*Picture -> *Picture
	drawAt	:: !Point2 !figure !*Picture -> *Picture
	undraw	::         !figure !*Picture -> *Picture
	undrawAt:: !Point2 !figure !*Picture -> *Picture

class Fillables figure where
	fill	::         !figure !*Picture -> *Picture
	fillAt	:: !Point2 !figure !*Picture -> *Picture
	unfill	::         !figure !*Picture -> *Picture
	unfillAt:: !Point2 !figure !*Picture -> *Picture


//	Text drawing operations:
//	Text is always drawn with the baseline at the y coordinate of the pen.

instance Drawables	Char
instance Drawables	{#Char}
/*	draw     text:
		draws the text starting at the current pen position.
		The new pen position is directly after the drawn text including spacing.
	drawAt p text:
		draws the text starting at p.
*/


//	Line2 drawing operations:
instance Drawables Line2
/*	draw     l:
		is equal to drawLine l.line_end1 l.line_end2.
	drawAt p l:
		draw l
	None of these functions change the pen position.
*/

//	Vector2 drawing operations:
instance Drawables Vector2
/*	draw     v:
		draws a line from the current pen position pen to pen+v.
	drawAt p v:
		draws a line from p to p+v.
*/


/*	Oval drawing operations:
	An Oval o is a transformed unit circle 
	with	horizontal radius rx	o.oval_rx
			vertical   radius ry	o.oval_ry
	Let (x,y) be a point on the unit circle:
		then (x`,y`) = (x*rx,y*ry) is a point on o.
	Let (x,y) be a point on o:
		then (x`,y`) = (x/rx,y/ry) is a point on the unit circle.
*/
instance Drawables Oval
instance Fillables Oval
/*	draw     o:
		draws an oval with the current pen position being the center of the oval.
	drawAt p o:
		draws an oval with p being the center of the oval.
	fill     o:
		fills an oval with the current pen position being the center of the oval.
	fillAt p o:
		fills an oval with p being the center of the oval.
	None of these functions change the pen position.
*/


/*	Curve drawing operations:
	A Curve c is a slice of an oval o
	with	start angle	a	c.curve_from
			end   angle	b	c.curve_to
			direction   d	c.curve_clockwise
	The angles are taken in radians (counter-clockwise). 
	If d holds then the drawing direction is clockwise, otherwise drawing occurs 
	counter-clockwise.
*/
instance Drawables Curve
instance Fillables Curve
/*	draw     c:
		draws a curve with the starting angle a at the current pen position.
		The pen position ends at ending angle b.
	drawAt p c:
		draws a curve with the starting angle a at p.
	fill     c:
		fills the figure obtained by connecting the endpoints of the drawn curve 
		(draw c) with the center of the curve oval.
		The pen position ends at ending angle b.
	fillAt p c:
		fills the figure obtained by connecting the endpoints of the drawn curve 
		(drawAt p c) with the center of the curve oval.
*/


/*	Box drawing operations:
	A Box b is a horizontally oriented rectangle
	with	width  w		b.box_w
			height h		b.box_h
	In case w==0 (h==0),   the Box collapses to a vertical (horizontal) vector.
	In case w==0 and h==0, the Box collapses to a point.
*/
instance Drawables	Box
instance Fillables	Box
/*	draw     b:
		draws a box with left-top corner at the current pen position p and 
		right-bottom corner at p+(w,h).
	drawAt p b:
		draws a box with left-top corner at p and right-bottom corner at p+(w,h).
	fill     b:
		fills a box with left-top corner at the current pen position p and 
		right-bottom corner at p+(w,h).
	fillAt p b:
		fills a box with left-top corner at p and right-bottom corner at p+(w,h).
	None of these functions change the pen position.
*/


/*	Rectangle drawing operations:
	A Rectangle r is always horizontally oriented
	with	width  w		abs (r.corner1.x-r.corner2.x)
			height h		abs (r.corner1.y-r.corner2.y)
	In case w==0 (h==0),   the Rectangle collapses to a vertical (horizontal) vector.
	In case w==0 and h==0, the Rectangle collapses to a point.
*/
instance Drawables Rectangle
instance Fillables Rectangle
/*	draw     r:
		draws a rectangle with diagonal corners r.corner1 and r.corner2.
	drawAt p r:
		draw r
	fill     r:
		fills a rectangle with diagonal corners r.corner1 and r.corner2.
	fillAt p r:
		fill r
	None of these functions change the pen position.
*/


/*	Polygon drawing operations:
	A Polygon p is a figure 
	with	shape	p.polygon_shape
	A polygon p at a point base is drawn as follows:
		drawPicture	[setPenPos base:map draw shape]++[drawToPoint base]
*/
instance Drawables Polygon
instance Fillables Polygon
/*	None of these functions change the pen position.
*/


getResolution :: !*Picture -> (!(!Int,!Int),!*Picture)
/*	getResolution returns the horizontal and vertical resolution of a Picture in dpi
	(dots per inch). 
	In case of a printer Picture: 
		the return values are the printer resolution (if it is not emulating the 
        screen resolution).
	In case of a screen Picture:
		the resolution is the number of pixels that fit in one "screen inch". 
		A "screen inch" is the physical	size of a 72 point font on the screen. 
		Although some screens allow the user to alter the screen resolution, 
		getResolution always returns the same value for the same screen.
		The reason is that if a user for instance increases the screen resolution, 
		not only the size of a pixel decreases, but also the size of a "screen 
		inch". So a 12 point font will not appear with 12 point size, but smaller 
		(A point is a physical unit, defined as 1 point is approximately 1/72 inch.)
*/
