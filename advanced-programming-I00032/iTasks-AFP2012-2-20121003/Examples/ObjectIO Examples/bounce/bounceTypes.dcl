definition module bounceTypes


import	StdInt, StdClass, StdPicture, StdIOCommon


::	Barrel
	=	{	bBase	:: Point2			// the base point of the barrel
		,	bWalls	:: [Wall]			// the walls of the barrel
		,	bDomain	:: BarrelDomain		// the enclosing rectangular area of the barrel
		}
::	Wall
	:==	(	!Vector2					// the displacement of the wall (a la Polygon)
		,	!Interior					// the sign at what side the wall faces the interior
		)
::	SingleWall
	:==	(	!Line						// the exact pixel position of the wall
		,	!Interior
		)
::	Line			:==	(!Point2,!Point2)
::	BarrelDomain	:== Rectangle
::	Interior		:== Int
::	Scale			:==	(!Real,!Real)	// (horizontal scale, vertical scale)
::	Radius			:== Int

::	Ball
	=	{	bCenter	:: Point2			// the center of the ball
		,	bRadius	:: Radius			// the radius of the ball
		,	bSpeed	:: Vector2			// the direction and speed of the ball
		,	bColour	:: Colour			// the colour of the ball
		}

rightBarrelSetUp	:: (Barrel,[Ball])
leftBarrelSetUp		:: (Barrel,[Ball])
BarrelToPolygon		:: !Scale !Barrel -> (!Point2,!Polygon)
splitWallsInBarrel	:: !Barrel -> (![SingleWall],![SingleWall])

between				:: !a !a !a			-> Bool		| Ord a
dist				:: !Point2 !Point2	-> Real
scaleSize			:: !Size !Size		-> Scale
