definition module StdPictureDef


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdPictureDef contains the predefined figures that can be drawn.
//	********************************************************************************


import	StdIOBasic
from	osfont		import :: Font


::	Line2									// A line connects two points
	=	{	line_end1		:: !Point2		// The first  point
		,	line_end2		:: !Point2		// The second point
		}
::	Box										// A box is a rectangle
	=	{	box_w			:: !Int			// The width  of the box
		,	box_h			:: !Int			// The height of the box
		}
::	Oval									// An oval is a stretched unit circle
	=	{	oval_rx			:: !Int			// The horizontal radius (stretch)
		,	oval_ry			:: !Int			// The vertical   radius (stretch)
		}
::	Curve									// A curve is a slice of an oval
	=	{	curve_oval		:: !Oval		// The source oval
		,	curve_from		:: !Real		// Starting angle (in radians)
		,	curve_to		:: !Real		// Ending   angle (in radians)
		,	curve_clockwise	:: !Bool		// Direction: True iff clockwise
		}
::	Polygon									// A polygon is an outline shape
	=	{	polygon_shape	:: ![Vector2]	// The shape of the polygon
		}
::	FontDef
	=	{	fName			:: !FontName	// Name of the font
		,	fStyles			:: ![FontStyle]	// Stylistic variations
		,	fSize			:: !FontSize	// Size in points
		}
::	FontMetrics
	=	{	fAscent			:: !Int			// Distance between top    and base line
		,	fDescent		:: !Int			// Distance between bottom and base line
		,	fLeading		:: !Int			// Distance between two text lines
		,	fMaxWidth		:: !Int			// Max character width including spacing
		}
::	FontName	:==	String
::	FontStyle	:==	String
::	FontSize	:==	Int
::	Colour
	=	RGB RGBColour
	|	Black		| White
	|	DarkGrey	| Grey		| LightGrey	// 75%, 50%, and 25% Black
	|	Red			| Green		| Blue
	|	Cyan		| Magenta	| Yellow
::	RGBColour
	=	{	r	:: !Int						// The contribution of red
		,	g	:: !Int						// The contribution of green
		,	b	:: !Int						// The contribution of blue
		}
::	PenAttribute							// Default:
	=	PenSize		Int						// 1
	|	PenPos		Point2					// zero
	|	PenColour	Colour					// Black
	|	PenBack		Colour					// White
	|	PenFont		Font					// DefaultFont

//	Colour constants:
BlackRGB				:==	{r=MinRGB,g=MinRGB,b=MinRGB}
WhiteRGB				:==	{r=MaxRGB,g=MaxRGB,b=MaxRGB}
MinRGB					:== 0
MaxRGB					:== 255

//	Font constants:
SerifFontDef			:: FontDef
SansSerifFontDef		:: FontDef
SmallFontDef			:: FontDef
NonProportionalFontDef	:: FontDef
SymbolFontDef			:: FontDef

//	Font style constants:
ItalicsStyle			:== "Italic"
BoldStyle				:== "Bold"
UnderlinedStyle			:== "Underline"

//	Standard lineheight of a font is the sum of its leading, ascent and descent:
fontLineHeight fMetrics	:==	fMetrics.fLeading + fMetrics.fAscent + fMetrics.fDescent

//	Useful when working with Ovals and Curves:
PI						:== 3.1415926535898
