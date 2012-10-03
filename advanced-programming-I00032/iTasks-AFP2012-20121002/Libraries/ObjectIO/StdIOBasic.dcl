definition module StdIOBasic


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdIOBasic defines basic types and access functions for the I/O library.
//	********************************************************************************


import	StdOverloaded, StdString


/*	General type constructors for composing context-independent data structures.
*/
::	:^:		t1 t2			= (:^:) infixr 9 t1 t2


/*	General type constructors for composing context-dependent data structures.
*/
::	:~:		t1 t2		cs	= (:~:) infixr 9 (t1 cs) (t2 cs)
::	ListCS		t		cs	= ListCS .[t cs]
::	NilCS				cs	= NilCS


/*	General type constructors for composing local and context-dependent 
	data structures.
*/
::	:+:		t1 t2	ls	cs	= (:+:) infixr 9 (t1 ls cs) (t2 ls cs)
::	ListLS		t	ls	cs	= ListLS .[t ls cs]
::	NilLS			ls	cs	= NilLS
::	NewLS		t	ls	cs	= E. .new: {newLS :: new, newDef :: t   new		cs}
::	AddLS		t	ls	cs	= E. .add: {addLS :: add, addDef :: t *(add,ls)	cs}

noLS ::     (.a->.b)    !(.c,.a) -> (.c,.b)	// Lift function    a  ->  b
											// to             (c,a)->(c,b)
noLS1:: (.x->.a->.b) .x !(.c,.a) -> (.c,.b)	// Lift function x->  a  ->  b
											// to            x->(c,a)->(c,b)


::	Index			:==	Int
::	Title			:==	String


::	Vector2			=	{vx::!Int,vy::!Int}

instance			==			Vector2		// @1-@2==zero
instance			+			Vector2		// {vx=@1.vx+@2.vx,vy=@1.vy+@2.vy}
instance			-			Vector2		// {vx=@1.vx-@2.vx,vy=@1.vy-@2.vy}
instance			zero		Vector2		// {vx=0,vy=0}
instance			~			Vector2		// zero-@1
instance			toString	Vector2

class toVector		x :: !x ->	Vector2


::	Size			=	{w ::!Int,h ::!Int}

instance			==			Size		// @1.w==@2.w && @1.h==@2.h
instance			zero		Size		// {w=0,h=0}
instance			toVector	Size		// {w,h}->{vx=w,vy=h}
instance			toString	Size


::	Point2
	=	{	x		:: !Int
		,	y		:: !Int
		}
::	Rectangle
	=	{	corner1	:: !Point2
		,	corner2	:: !Point2
		}

instance			==			Point2		// @1-@2==zero
instance			+			Point2		// {x=@1.x+@2.x,y=@1.y+@2.y}
instance			-			Point2 		// {x=@1.x-@2.x,y=@1.y-@2.y}
instance			zero		Point2		// {x=0,y=0}
instance			~			Point2		// zero-@1
instance			toVector	Point2		// {x,y}->{vx=x,vy=y}
instance			toString	Point2

instance			==			Rectangle	//    @1.corner1==@2.corner1
											// && @1.corner2==@2.corner2
instance			zero		Rectangle	// {corner1=zero,corner2=zero}
instance			toString	Rectangle

rectangleSize		:: !Rectangle -> Size	// {w=abs (@1.corner1-@1.corner2).x,
											//  h=abs (@1.corner1-@1.corner2).y}
movePoint	:: !Vector2 !Point2 -> .Point2	// {vx,vy} {x,y} -> {vx+x,vy+y}


::	IdFun st		:==	st -> st

::	Void			=	Void


::	Alt2 a b		= Alt1Of2 a | Alt2Of2 b		// Alternative data type in cases where Either causes name clashes
alt1Of2				:: !(Alt2 a b) -> a			// Take first  alternative if applicable (test with isAlt1Of2)
alt2Of2				:: !(Alt2 a b) -> b			// Take second alternative if applicable (test with isAlt2Of2)
isAlt1Of2			:: !(Alt2 a b) -> Bool		// True iff (Alt1Of2 _)
isAlt2Of2			:: !(Alt2 a b) -> Bool		// True iff (Alt2Of2 _)
