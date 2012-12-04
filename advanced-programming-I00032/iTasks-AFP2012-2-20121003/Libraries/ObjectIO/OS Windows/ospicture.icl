implementation module ospicture

import	StdBool, StdFunc, StdInt, StdList, StdReal, StdTuple
import	pictCCall_12, osfont, ostypes
from	osrgn		import :: OSRgnHandle
from	ostoolbox	import OSNewToolbox
import	StdPictureDef
from	commondef	import class toTuple(..), instance toTuple Point2, instance toTuple Vector2,
						class subVector(..), instance subVector OSRect, setBetween

::	Picture
	=	{	pictContext		:: !OSPictContext	// The context for drawing operations
		,	pictToolbox		:: !.OSToolbox		// The continuation value
		,	pictOrigin		:: !Origin			// The current origin of the picture
		,	pictPen			:: !.Pen			// The current state of the pen
		,	pictToScreen	:: !Bool			// Flag: the output goes to screen (True) or printer (False)
		}
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


//	Conversion operations to and from Picture
/*
initialisePicture :: !Origin !Pen !OSPictContext !*OSToolbox -> (!OSPictContext,!*OSToolbox)
initialisePicture origin pen=:{penSize,penForeColour,penBackColour,penPos,penFont} hdc tb
	# {osfontname,osfontstyles,osfontsize}	= osFontgetimp penFont
	# (hdc,tb)								= winInitPicture
												penSize
												iModeCopy
												initforecolour
												initbackcolour
												initpen
												(osfontname,osfontstyles,osfontsize)
												(0,0)
												(hdc,tb)
	# (_,_,_,_,_,_,(hdc,tb))	= winDonePicture (hdc,tb)
	= (hdc,tb)
where
	initforecolour	= toRGBtriple penForeColour
	initbackcolour	= toRGBtriple penBackColour
	initpen			= toTuple (penPos-origin)
*/
packPicture :: !Origin !*Pen !Bool !OSPictContext !*OSToolbox -> *Picture
packPicture origin pen=:{penSize,penForeColour,penBackColour,penPos,penFont} isScreenOutput hdc tb
	#! {osfontname,osfontstyles,osfontsize}= osFontgetimp penFont
	#! (hdc,tb)		= winInitPicture
						penSize
						iModeCopy
						initforecolour
						initbackcolour
						initpen
						(osfontname,osfontstyles,osfontsize)
						(0,0)
						(hdc,tb)
	= {	pictContext	= hdc
	  ,	pictToolbox	= tb
	  ,	pictOrigin	= origin
	  ,	pictPen		= pen
	  ,	pictToScreen= isScreenOutput
	  }
where
	initforecolour	= toRGBtriple penForeColour
	initbackcolour	= toRGBtriple penBackColour
	initpen			= toTuple (penPos-origin)

unpackPicture :: !*Picture -> (!Origin,!*Pen,!Bool,!OSPictContext,!*OSToolbox)
unpackPicture {pictOrigin,pictPen,pictToScreen,pictContext,pictToolbox}
// PA: intend to use simplified version of winDonePicture; crashes for some reason.
	# (_,_,_,_,_,_,(hdc,tb))	= winDonePicture (pictContext,pictToolbox)
//	# (hdc,tb)	= WinDonePicture (pictContext,pictToolbox)
	= (pictOrigin,pictPen,pictToScreen,hdc,tb)

peekPicture :: !*Picture -> (!Origin,!*Pen,!Bool,!OSPictContext,!*OSToolbox)
peekPicture {pictOrigin,pictPen,pictToScreen,pictContext,pictToolbox}
	= (pictOrigin,pictPen,pictToScreen,pictContext,pictToolbox)

unpeekPicture :: !Origin !*Pen !Bool !OSPictContext !*OSToolbox -> *Picture
unpeekPicture origin pen isScreenOutput hdc tb
	= {pictOrigin=origin,pictPen=pen,pictToScreen=isScreenOutput,pictContext=hdc,pictToolbox=tb}

peekOSPictContext :: !*Picture -> (!OSPictContext,!*Picture)
peekOSPictContext picture=:{pictContext}
	= (pictContext,picture)

sharePicture :: !*Picture -> (!Picture,!*Picture)
sharePicture picture=:{pictOrigin,pictPen,pictToScreen}
	# (sPen,uPen)	= sharePen pictPen
	= ({pictContext=0,pictToolbox=OSNewToolbox,pictOrigin=pictOrigin,pictPen=sPen,pictToScreen=pictToScreen},{picture & pictPen=uPen})

sharePen :: !*Pen -> (!Pen,!*Pen)
sharePen pen=:{penSize,penForeColour,penBackColour,penPos,penFont}
	# (sPenPos,uPenPos)	= sharePoint penPos
	= ({penSize=penSize,penForeColour=penForeColour,penBackColour=penBackColour,penPos=sPenPos,penFont=penFont},{pen & penPos=uPenPos})
where
	sharePoint :: !*Point2 -> (!Point2,!*Point2)
	sharePoint point=:{x,y} = ({x=x,y=y},point)

copyPen :: !Pen -> *Pen
copyPen {penSize,penForeColour,penBackColour,penPos={x,y},penFont}
	= {penSize=penSize,penForeColour=penForeColour,penBackColour=penBackColour,penPos={x=x,y=y},penFont=penFont}

peekScreen :: !.(St *Picture .x) !*OSToolbox -> (!.x,!*OSToolbox)
peekScreen f tb
	# (hdc,tb)		= winCreateScreenHDC tb
	# picture		= packPicture zero defaultPen True hdc tb
	# (x,picture)	= f picture
	# (_,_,_,hdc,tb)= unpackPicture picture
	# tb			= winDestroyScreenHDC (hdc,tb)
	= (x,tb)


defaultPen :: *Pen
defaultPen
	= {	penSize			= 1
	  ,	penForeColour	= Black
	  ,	penBackColour	= White
	  ,	penPos			= {x=0,y=0}
	  ,	penFont			= defaultFont
	  }
where
	(defaultFont,_)		= osDefaultfont OSNewToolbox

dialogPen :: *Pen
dialogPen
	= {	penSize			= 1
	  ,	penForeColour	= Black
	  ,	penBackColour	= White
	  ,	penPos			= {x=0,y=0}
	  ,	penFont			= dialogFont
	  }
where
	(dialogFont,_)		= osDialogfont OSNewToolbox

setPenAttribute :: !PenAttribute !u:Pen -> u:Pen
setPenAttribute (PenSize   size)   pen = {pen & penSize      =max 1 size}
setPenAttribute (PenPos    {x,y})  pen = {pen & penPos       ={x=x,y=y} }
setPenAttribute (PenColour colour) pen = {pen & penForeColour=colour    }
setPenAttribute (PenBack   colour) pen = {pen & penBackColour=colour    }
setPenAttribute (PenFont   font)   pen = {pen & penFont      =font      }


/*	Picture interface functions.
*/
apppicttoolbox :: !(IdFun *OSToolbox) !*Picture -> *Picture
apppicttoolbox f picture=:{pictToolbox}
	= {picture & pictToolbox=f pictToolbox}

accpicttoolbox :: !(St *OSToolbox .x) !*Picture -> (!.x,!*Picture)
accpicttoolbox f picture=:{pictToolbox}
	# (x,tb)	= f pictToolbox
	= (x,{picture & pictToolbox=tb})


/*	Attribute functions.
*/
//	Access to Origin and Pen:
getpictorigin :: !*Picture -> (!Origin,!*Picture)
getpictorigin picture=:{pictOrigin}
	= (pictOrigin,picture)

setpictorigin :: !Origin !*Picture -> *Picture
setpictorigin origin picture
	= {picture & pictOrigin=origin}

getpictpen :: !*Picture -> (!Pen,!*Picture)
getpictpen picture=:{pictPen}
	# (sPen,uPen)	= sharePen pictPen
	= (sPen,{picture & pictPen=uPen})

setpictpen :: !Pen !*Picture -> *Picture
setpictpen {penSize,penForeColour,penBackColour,penPos,penFont} picture
	# picture	= setpictpensize    penSize       picture
	# picture	= setpictpencolour  penForeColour picture
	# picture	= setpictbackcolour penBackColour picture
	# picture	= setpictpenpos     penPos        picture
	# picture	= setpictpenfont    penFont       picture
	= picture


//	Change the pen position:
setpictpenpos :: !Point2 !*Picture -> *Picture
setpictpenpos newpos=:{x=x`,y=y`} picture=:{pictToolbox,pictOrigin,pictPen=pen=:{penPos={x,y}},pictContext}
	| x==x` && y==y`
		= picture
	| otherwise
		# (context,tb)	= winMovePenTo (toTuple (newpos-pictOrigin)) (pictContext,pictToolbox)
		  pen			= {pen & penPos={x=x`,y=y`}}
		= {picture & pictToolbox=tb,pictContext=context,pictPen=pen}

getpictpenpos :: !*Picture -> (!Point2,!*Picture)
getpictpenpos picture=:{pictPen={penPos={x,y}}}
	= ({x=x,y=y},picture)

movepictpenpos :: !Vector2 !*Picture -> *Picture
movepictpenpos v=:{vx,vy} picture=:{pictToolbox,pictPen=pen=:{penPos={x,y}},pictContext}
	# (context,tb)	= winMovePen (toTuple v) (pictContext,pictToolbox)
	  pen			= {pen & penPos={x=x+vx,y=y+vy}}
	= {picture & pictToolbox=tb,pictContext=context,pictPen=pen}

//	Change the pen size:
setpictpensize :: !Int !*Picture -> *Picture
setpictpensize w picture=:{pictToolbox,pictContext,pictPen}
	| w`==pictPen.penSize
		= picture
	| otherwise
		# (context,tb)	= winSetPenSize w` (pictContext,pictToolbox)
		  pen			= {pictPen & penSize=w`}
		= {picture & pictToolbox=tb,pictContext=context,pictPen=pen}
where
	w` = max 1 w

getpictpensize :: !*Picture -> (!Int,!*Picture)
getpictpensize picture=:{pictPen={penSize}}
	= (penSize,picture)


//	Change the PenColour:
setpictpencolour :: !Colour !*Picture -> *Picture
setpictpencolour colour picture=:{pictToolbox,pictPen,pictContext}
	| reqRGB==curRGB
		= picture
	| otherwise
		# (context,tb)	= winSetPenColor reqRGB (pictContext,pictToolbox)
		  pen			= {pictPen & penForeColour=colour}
		= {picture & pictPen=pen,pictToolbox=tb,pictContext=context}
where
	reqRGB				= toRGBtriple colour
	curRGB				= toRGBtriple pictPen.penForeColour

setpictbackcolour :: !Colour !*Picture -> *Picture
setpictbackcolour colour picture=:{pictToolbox,pictPen,pictContext}
	| reqRGB==curRGB
		= picture
	| otherwise
		# (context,tb)	= winSetBackColor (toRGBtriple colour) (pictContext,pictToolbox)
		  pen			= {pictPen & penBackColour=colour}
		= {picture & pictPen=pen,pictToolbox=tb,pictContext=context}
where
	reqRGB				= toRGBtriple colour
	curRGB				= toRGBtriple pictPen.penBackColour

toRGBtriple :: !Colour -> (!Int,!Int,!Int)
toRGBtriple (RGB {r,g,b})	= (setBetween r MinRGB MaxRGB,setBetween g MinRGB MaxRGB,setBetween b MinRGB MaxRGB)
toRGBtriple Black			= (MinRGB,MinRGB,MinRGB)
toRGBtriple DarkGrey		= ( MaxRGB>>2,    MaxRGB>>2,    MaxRGB>>2)
toRGBtriple Grey			= ( MaxRGB>>1,    MaxRGB>>1,    MaxRGB>>1)
toRGBtriple LightGrey		= ((MaxRGB>>2)*3,(MaxRGB>>2)*3,(MaxRGB>>2)*3)
toRGBtriple White			= (MaxRGB,MaxRGB,MaxRGB)
toRGBtriple Red				= (MaxRGB,MinRGB,MinRGB)
toRGBtriple Green			= (MinRGB,MaxRGB,MinRGB)
toRGBtriple Blue			= (MinRGB,MinRGB,MaxRGB)
toRGBtriple Cyan			= (MinRGB,MaxRGB,MaxRGB)
toRGBtriple Magenta			= (MaxRGB,MinRGB,MaxRGB)
toRGBtriple Yellow			= (MaxRGB,MaxRGB,MinRGB)

getpictpencolour :: !*Picture -> (!Colour,!*Picture)
getpictpencolour picture=:{pictPen={penForeColour}}
	= (penForeColour,picture)

getpictbackcolour :: !*Picture -> (!Colour,!*Picture)
getpictbackcolour picture=:{pictPen={penBackColour}}
	= (penBackColour,picture)


//	Change the font attributes:
setpictpenfont :: !Font !*Picture -> *Picture
setpictpenfont font picture=:{pictToolbox,pictContext,pictPen=pen}
	| imp==osFontgetimp pen.penFont
		= picture
	| otherwise
		# (context,tb)	= winSetFont (osfontname,osfontstyles,osfontsize) (pictContext,pictToolbox)
		  pen			= {pen & penFont=font}
		= {picture & pictToolbox=tb,pictContext=context,pictPen=pen}
where
	imp										= osFontgetimp font
	{osfontname,osfontstyles,osfontsize}	= imp

getpictpenfont :: !*Picture -> (!Font,!*Picture)
getpictpenfont picture=:{pictPen={penFont}}
	= (penFont,picture)

setpictpendefaultfont :: !*Picture -> *Picture
setpictpendefaultfont picture=:{pictToolbox,pictContext,pictPen}
	# (font,tb)		= osDefaultfont pictToolbox
	  {osfontname,osfontstyles,osfontsize}
	  				= osFontgetimp font
	# (context,tb)	= winSetFont (osfontname,osfontstyles,osfontsize) (pictContext,tb)
	  pen			= {pictPen & penFont=font}
	= {picture & pictToolbox=tb,pictContext=context,pictPen=pen}


/*	Drawing mode setting functions.
*/
setpictxormode :: !*Picture -> *Picture
setpictxormode picture=:{pictToolbox,pictContext}
	# (context,tb)	= winSetMode iModeXor (pictContext,pictToolbox)
	= {picture & pictToolbox=tb,pictContext=context}

setpicthilitemode :: !*Picture -> *Picture
setpicthilitemode picture=:{pictToolbox,pictContext}
	# (context,tb)	= winSetMode iModeXor (pictContext,pictToolbox)
	= {picture & pictToolbox=tb,pictContext=context}

setpictnormalmode :: !*Picture -> *Picture
setpictnormalmode picture=:{pictToolbox,pictContext}
	# (context,tb)	= winSetMode iModeCopy (pictContext,pictToolbox)
	= {picture & pictToolbox=tb,pictContext=context}


/*	Point2 drawing operations.
	pictdrawpoint
		only draws a point at that position. The pen position is not changed.
*/
pictdrawpoint :: !Point2 !*Picture -> *Picture
pictdrawpoint pos=:{x,y} picture=:{pictPen={penSize},pictOrigin={x=ox,y=oy},pictToolbox,pictContext}
	| penSize==1
		# (context,tb)	= winDrawPoint (x`,y`) (pictContext,pictToolbox)
		= {picture & pictToolbox=tb,pictContext=context}
	| otherwise
		# (context,tb)	= winFillRectangle {rleft=x`,rtop=y`,rright=x`+penSize,rbottom=y`+penSize} (pictContext,pictToolbox)
		= {picture & pictToolbox=tb,pictContext=context}
where
	(x`,y`)	= (x-ox,y-oy)


/*	Line drawing operations.
	pictdrawlineto
		draws a line from the current pen position to the given pen position. 
		The new pen position is the endpoint of the line.	
	pictdrawline
		draws a line from the first point to the second point. The pen position
		is not changed.
*/
pictdrawlineto :: !Point2 !*Picture -> *Picture
pictdrawlineto pos=:{x,y} picture=:{pictOrigin,pictToolbox,pictContext,pictPen}
	# (context,tb)	= winLinePenTo (toTuple (pos-pictOrigin)) (pictContext,pictToolbox)
	  pen			= {pictPen & penPos={x=x,y=y}}
	= {picture & pictToolbox=tb,pictContext=context,pictPen=pen}

pictundrawlineto :: !Point2 !*Picture -> *Picture
pictundrawlineto pos=:{x,y} picture=:{pictOrigin,pictToolbox,pictContext,pictPen=pen=:{penForeColour,penBackColour}}
	# (context,tb)	= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)	= winLinePenTo (toTuple (pos-pictOrigin)) (context,tb)
	# (context,tb)	= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	= {picture & pictToolbox=tb,pictContext=context,pictPen={pen & penPos={x=x,y=y}}}

pictdrawline :: !Point2 !Point2 !*Picture -> *Picture
pictdrawline a b picture=:{pictOrigin,pictToolbox,pictContext}
	# (context,tb)	= winDrawLine (toTuple (a-pictOrigin)) (toTuple (b-pictOrigin)) (pictContext,pictToolbox)
	= {picture & pictToolbox=tb,pictContext=context}

pictundrawline :: !Point2 !Point2 !*Picture -> *Picture
pictundrawline a b picture=:{pictOrigin,pictToolbox,pictContext,pictPen={penForeColour,penBackColour}}
	# (context,tb)	= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)	= winDrawLine (toTuple (a-pictOrigin)) (toTuple (b-pictOrigin)) (context,tb)
	# (context,tb)	= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	= {picture & pictToolbox=tb,pictContext=context}


/*	Text drawing operations.
	pictdraw(char/string) draws a char/string at the current pen position. The new
		pen position is immediately after the drawn char/string.
*/
pictdrawchar :: !Char !*Picture -> *Picture
pictdrawchar char picture=:{pictContext,pictToolbox,pictPen,pictOrigin}
	# (context,tb)		= winDrawChar (toInt char) (pictContext,pictToolbox)
	# (x`,y`,context,tb)= winGetPenPos (context,tb)
	#! {x,y}			= pictOrigin
	#! pen				= {pictPen & penPos={x=x+x`,y=y+y`}}
	= {picture & pictContext=context,pictToolbox=tb,pictPen=pen}

pictundrawchar :: !Char !*Picture -> *Picture
pictundrawchar char picture=:{pictContext,pictToolbox,pictPen=pen=:{penForeColour,penBackColour},pictOrigin={x=ox,y=oy}}
	# (context,tb)		= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)		= winDrawChar (toInt char) (context,tb)
	# (context,tb)		= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	# (x,y,context,tb)	= winGetPenPos (context,tb)
	= {picture & pictContext=context,pictToolbox=tb,pictPen={pen & penPos={x=x+ox,y=y+oy}}}

pictdrawstring :: !String !*Picture -> *Picture
pictdrawstring string picture=:{pictContext,pictToolbox,pictPen,pictOrigin={x=ox,y=oy}}	// PA:
	# (context,tb)		= winDrawString string (pictContext,pictToolbox)
	# (x,y,context,tb)	= winGetPenPos (context,tb)
	  pen				= {pictPen & penPos={x=x+ox,y=y+oy}}
	= {picture & pictContext=context,pictToolbox=tb,pictPen=pen}

pictundrawstring :: !String !*Picture -> *Picture
pictundrawstring string picture=:{pictContext,pictToolbox,pictPen=pen=:{penForeColour,penBackColour},pictOrigin={x=ox,y=oy}}
	# (context,tb)		= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)		= winDrawString string (context,tb)
	# (context,tb)		= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	# (x,y,context,tb)	= winGetPenPos (context,tb)
	= {picture & pictContext=context,pictToolbox=tb,pictPen={pen & penPos={x=x+ox,y=y+oy}}}


/*	Oval drawing operations.
	pict(draw/fill)oval center oval 
		draws/fills an oval at center with horizontal and vertical radius. The new
		pen position is not changed.
*/
pictdrawoval :: !Point2 !Oval !*Picture -> *Picture
pictdrawoval center oval picture=:{pictContext,pictToolbox,pictOrigin}
	# (context,tb)	= winDrawOval rect (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}
where
	rect	= ovalToRect (center-pictOrigin) oval

pictundrawoval :: !Point2 !Oval !*Picture -> *Picture
pictundrawoval center oval picture=:{pictContext,pictToolbox,pictOrigin,pictPen={penBackColour,penForeColour}}
	# (context,tb)	= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)	= winDrawOval rect (context,tb)
	# (context,tb)	= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	= {picture & pictContext=context,pictToolbox=tb}
where
	rect	= ovalToRect (center-pictOrigin) oval

pictfilloval :: !Point2 !Oval !*Picture -> *Picture
pictfilloval center oval picture=:{pictContext,pictToolbox,pictOrigin}
	# (context,tb)	= winFillOval rect (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}
where
	rect	= ovalToRect (center-pictOrigin) oval

pictunfilloval :: !Point2 !Oval !*Picture -> *Picture
pictunfilloval center oval picture=:{pictContext,pictToolbox,pictOrigin,pictPen}
	# (context,tb)	= winEraseOval rect (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}
where
	rect	= ovalToRect (center-pictOrigin) oval

ovalToRect :: !Point2 !Oval -> OSRect
ovalToRect {x,y} {oval_rx,oval_ry}
	= {rleft=x-rx,rtop=y-ry,rright=x+rx,rbottom=y+ry}
where
	rx	= abs oval_rx
	ry	= abs oval_ry


/*	Curve drawing operations.
	pict(draw/fill)curve movePen point curve
		draws/fills a curve starting at point with a shape defined by curve. If movePen
		is True, then the new pen position is at the end of the curve, otherwise it does
		not change.
*/
pictdrawcurve :: !Bool !Point2 !Curve !*Picture -> *Picture
pictdrawcurve movePen start=:{x,y} curve picture=:{pictContext,pictToolbox,pictOrigin}
	# (context,tb)		= winDrawCurve wrect (toTuple wstart) (toTuple wend) (pictContext,pictToolbox)
	# picture			= {picture & pictContext=context,pictToolbox=tb}
	| not movePen		= picture
	| otherwise			= setpictpenpos end picture
where
	start`				= start-pictOrigin
	(wrect,wstart,wend)	= getcurve_rect_begin_end start` curve
	end					= wend+pictOrigin

pictundrawcurve :: !Bool !Point2 !Curve !*Picture -> *Picture
pictundrawcurve movePen start=:{x,y} curve picture=:{pictContext,pictToolbox,pictOrigin,pictPen={penForeColour,penBackColour}}
	# (context,tb)		= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)		= winDrawCurve wrect (toTuple wstart) (toTuple wend) (context,tb)
	# (context,tb)		= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	# picture			= {picture & pictContext=context,pictToolbox=tb}
	| not movePen		= picture
	| otherwise			= setpictpenpos end picture
where
	start`				= start-pictOrigin
	(wrect,wstart,wend)	= getcurve_rect_begin_end start` curve
	end					= wend+pictOrigin

pictfillcurve :: !Bool !Point2 !Curve !*Picture -> *Picture
pictfillcurve movePen start curve picture=:{pictContext,pictToolbox,pictOrigin}
	# (context,tb)		= winFillWedge wrect (toTuple wstart) (toTuple wend) (pictContext,pictToolbox)
	# picture			= {picture & pictContext=context,pictToolbox=tb}
	| not movePen		= picture
	| otherwise			= setpictpenpos end picture
where
	start`				= start-pictOrigin
	(wrect,wstart,wend)	= getcurve_rect_begin_end start` curve
	end					= wend+pictOrigin

pictunfillcurve :: !Bool !Point2 !Curve !*Picture -> *Picture
pictunfillcurve movePen start curve picture=:{pictContext,pictToolbox,pictOrigin,pictPen={penForeColour,penBackColour}}
	# (context,tb)		= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)		= winFillWedge wrect (toTuple wstart) (toTuple wend) (context,tb)
	# (context,tb)		= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	# picture			= {picture & pictContext=context,pictToolbox=tb}
	| not movePen		= picture
	| otherwise			= setpictpenpos end picture
where
	start`				= start-pictOrigin
	(wrect,wstart,wend)	= getcurve_rect_begin_end start` curve
	end					= wend+pictOrigin

getcurve_rect_begin_end :: !Point2 !Curve -> (!OSRect,!Point2,!Point2)
getcurve_rect_begin_end start=:{x,y} {curve_oval={oval_rx,oval_ry},curve_from,curve_to,curve_clockwise}
	| curve_clockwise	= (rect,end,start)
	| otherwise			= (rect,start,end)
where
	rx`					= toReal (abs oval_rx)
	ry`					= toReal (abs oval_ry)
	cx					= x -(toInt ((cos curve_from)*rx`))
	cy					= y +(toInt ((sin curve_from)*ry`))
	ex					= cx+(toInt ((cos curve_to  )*rx`))
	ey					= cy-(toInt ((sin curve_to  )*ry`))
	end					= {x=ex,y=ey}
	rect				= {rleft=cx-oval_rx,rtop=cy-oval_ry,rright=cx+oval_rx,rbottom=cy+oval_ry}


/*	OSRect drawing operations.
	pict(draw/fill)rect rect
		draws/fills a rect. The pen position is not changed.
*/
pictdrawrect :: !OSRect !*Picture -> *Picture
pictdrawrect r picture=:{pictContext,pictToolbox,pictOrigin}
	# (context,tb)	= winDrawRectangle (subVector (toVector pictOrigin) r) (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}

pictundrawrect :: !OSRect !*Picture -> *Picture
pictundrawrect r picture=:{pictContext,pictToolbox,pictOrigin,pictPen={penForeColour,penBackColour}}
	# (context,tb)	= winSetPenColor (toRGBtriple penBackColour) (pictContext,pictToolbox)
	# (context,tb)	= winDrawRectangle (subVector (toVector pictOrigin) r) (context,tb)
	# (context,tb)	= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	= {picture & pictContext=context,pictToolbox=tb}

pictfillrect :: !OSRect !*Picture -> *Picture
pictfillrect r picture=:{pictContext,pictToolbox,pictOrigin}
	# (context,tb)	= winFillRectangle (subVector (toVector pictOrigin) r) (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}

pictunfillrect :: !OSRect !*Picture -> *Picture
pictunfillrect r picture=:{pictContext,pictToolbox,pictOrigin}
	# (context,tb)	= winEraseRectangle (subVector (toVector pictOrigin) r) (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}


/*	Scrolling operation (handle with care).
*/
pictscroll :: !OSRect !Vector2 !*Picture -> (!OSRect,!*Picture)
pictscroll r v picture=:{pictContext,pictToolbox,pictOrigin}
	# (updRect,(context,tb))	= winScrollRectangle (subVector (toVector pictOrigin) r) (toTuple v) (pictContext,pictToolbox)
	= (updRect,{picture & pictContext=context,pictToolbox=tb})

pictscroll2 :: !OSRect !Vector2 !*Picture -> (!OSRect,!*Picture)
pictscroll2 r v picture=:{pictContext,pictToolbox,pictOrigin}
	# (updRect,(context,tb))	= winScrollRectangle2 (subVector (toVector pictOrigin) r) (toTuple v) (pictContext,pictToolbox)
	= (updRect,{picture & pictContext=context,pictToolbox=tb})

/*	Polygon drawing operations.
	pict(draw/fill)polygon point polygon
		draws/fills a polygon starting at point. The pen position is not changed.
*/
pictdrawpolygon :: !Point2 !Polygon !*Picture -> *Picture
pictdrawpolygon start {polygon_shape} picture=:{pictContext,pictToolbox,pictOrigin}
	# tb			= transferPolygon (start-pictOrigin) polygon_shape pictToolbox
	# (context,tb)	= winDrawPolygon (pictContext,tb)
	# tb			= winEndPolygon tb
	= {picture & pictContext=context,pictToolbox=tb}

pictundrawpolygon :: !Point2 !Polygon !*Picture -> *Picture
pictundrawpolygon start {polygon_shape} picture=:{pictContext,pictToolbox,pictOrigin,pictPen={penForeColour,penBackColour}}
	# tb			= transferPolygon (start-pictOrigin) polygon_shape pictToolbox
	# (context,tb)	= winSetPenColor (toRGBtriple penBackColour) (pictContext,tb)
	# (context,tb)	= winDrawPolygon (context,tb)
	# tb			= winEndPolygon tb
	# (context,tb)	= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	= {picture & pictContext=context,pictToolbox=tb}

pictfillpolygon :: !Point2 !Polygon !*Picture -> *Picture
pictfillpolygon start {polygon_shape} picture=:{pictPen={penSize},pictContext,pictToolbox,pictOrigin}
	# tb			= transferPolygon (start-pictOrigin) polygon_shape pictToolbox
	# (context,tb)	= winSetPenSize 1 (pictContext,tb)
	# (context,tb)	= winFillPolygon (context,tb)
	# (context,tb)	= winDrawPolygon (context,tb)
	# (context,tb)	= winSetPenSize penSize (context,tb)
	# tb			= winEndPolygon tb
	= {picture & pictContext=context,pictToolbox=tb}

pictunfillpolygon :: !Point2 !Polygon !*Picture -> *Picture
pictunfillpolygon start {polygon_shape} picture=:{pictPen={penSize,penForeColour,penBackColour},pictContext,pictToolbox,pictOrigin}
	# tb			= transferPolygon (start-pictOrigin) polygon_shape pictToolbox
	# (context,tb)	= winSetPenColor (toRGBtriple penBackColour) (pictContext,tb)
	# (context,tb)	= winSetPenSize 1 (context,tb)
	# (context,tb)	= winFillPolygon  (context,tb)
	# (context,tb)	= winDrawPolygon  (context,tb)
	# (context,tb)	= winSetPenSize penSize (context,tb)
	# tb			= winEndPolygon tb
	# (context,tb)	= winSetPenColor (toRGBtriple penForeColour) (context,tb)
	= {picture & pictContext=context,pictToolbox=tb}

transferPolygon :: !Point2 ![Vector2] !*OSToolbox -> *OSToolbox
transferPolygon start vs tb
	# tb	= winStartPolygon (1 + length vs) tb
	# tb	= winAddPolygonPoint wstart tb
	# tb	= transferShape wstart vs tb
	= tb
where
	wstart	= toTuple start
	
	transferShape :: !(!Int,!Int) ![Vector2] !*OSToolbox -> *OSToolbox
	transferShape (x,y) [{vx,vy}:vs] tb
   		= transferShape newpos vs (winAddPolygonPoint newpos tb)
	where
		newpos = (x+vx,y+vy)
	transferShape _ _ tb
		= tb

/*	Clipping operations.
	pictgetcliprgn gets the current clipping region.
	pictsetcliprgn sets the given clipping region.
	pictandcliprgn takes the intersection of the current clipping region and the argument region.
*/
pictgetcliprgn :: !*Picture -> (!OSRgnHandle,!*Picture)
pictgetcliprgn picture=:{pictContext,pictToolbox}
	# (cliprgn,(context,tb)) = winGetClipRgnPicture (pictContext,pictToolbox)
	= (cliprgn,{picture & pictContext=context,pictToolbox=tb})

pictsetcliprgn :: !OSRgnHandle !*Picture -> *Picture
pictsetcliprgn cliprgn picture=:{pictContext,pictToolbox}
	# (context,tb)	= winSetClipRgnPicture cliprgn (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}

pictandcliprgn :: !OSRgnHandle !*Picture -> *Picture
pictandcliprgn cliprgn picture=:{pictContext,pictToolbox}
	# (context,tb)	= winClipRgnPicture cliprgn (pictContext,pictToolbox)
	= {picture & pictContext=context,pictToolbox=tb}

/*	Resolution access function (added by MW):
*/
getResolutionC :: !OSPictContext !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
getResolutionC _ _
	= code 	{
 				ccall getResolutionC "I:VII:I"
			}

// MW: scaling of screen coordinates to printer coordinates.
getPictureScalingFactors :: !OSPictContext !*OSToolbox -> (!(!Int,!Int),!(!Int,!Int),!OSPictContext,!*OSToolbox)
getPictureScalingFactors _ _
	= code
	{
		ccall WinGetPictureScaleFactor "II-IIIIII"
	}

getpictpenattributes :: !*Picture -> (![PenAttribute],!*Picture)
getpictpenattributes picture
	# (pen,picture)	= getpictpen picture
	= (getpenattribute pen,picture)
where
	getpenattribute :: !Pen -> [PenAttribute]
	getpenattribute {penSize,penForeColour,penBackColour,penPos,penFont}
		= [PenSize penSize,PenPos penPos,PenColour penForeColour,PenBack penBackColour,PenFont penFont]

getPenPenPos :: !*Pen -> (!Point2,!*Pen)
getPenPenPos pen=:{penPos={x,y}} = ({x=x,y=y},pen)
