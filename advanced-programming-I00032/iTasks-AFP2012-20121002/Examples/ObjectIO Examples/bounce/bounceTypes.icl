implementation module bounceTypes


import	StdInt, StdBool, StdReal, StdList, StdFunc, StdTuple
import	StdPicture, StdIOCommon


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

leftBarrelSetUp :: (Barrel,[Ball])
leftBarrelSetUp
	=	(	{	bBase	= {x=600,y=100}
			,	bWalls	= [	({vx = -400,vy = 0  },   1)
						  ,	({vx = 0,   vy = -99},  -1)
						  ,	({vx = -199,vy = 0  },   1)
						  ,	({vx = 0,   vy = 299},   1)
						  ,	({vx = 199, vy = 0  },  -1)
						  ,	({vx = 0,   vy = -100}, -1)
						  ,	({vx = 400, vy = 0  },  -1)
						  ]
			,	bDomain	= {corner1={x=(-10),y=(-10)},corner2={x=600,y=310}}
			}
			,	[	{bCenter={x=30,y=150},bRadius=15,bSpeed={vx=10,vy=3},   bColour=Red   }
				,	{bCenter={x=60,y=150},bRadius=10,bSpeed={vx=5, vy=(-9)},bColour=Yellow}
				]
		)

rightBarrelSetUp :: (Barrel,[Ball])
rightBarrelSetUp
	=	(	{	bBase	= {x=600,y=100}
			,	bWalls	= [	({vx = 400, vy = 0   }, 1)
						  ,	({vx = 0,   vy = -99 }, 1)
						  ,	({vx = 200, vy = 0   }, 1)
						  ,	({vx = 0,   vy = 299 },-1)
						  ,	({vx = -200,vy = 0   },-1)
						  ,	({vx = 0,   vy = -100}, 1)
						  ,	({vx = -400,vy = 0   },-1)
						  ]
			,	bDomain	= {corner1={x=600,y=(-10)},corner2={x=1210,y=310}}
			}
		,	[	{bCenter={x=750,y=150},bRadius=8,bSpeed={vx=6,vy=(-9)},bColour=Magenta}
			,	{bCenter={x=800,y=140},bRadius=9,bSpeed={vx=(-2),vy=3},bColour=Blue   }
			]
		)

BarrelToPolygon :: !Scale !Barrel -> (!Point2,!Polygon)
BarrelToPolygon scale {bBase,bWalls,bDomain}
	= (scalebase scale bBase bDomain,{polygon_shape=map (scalewall scale) bWalls})
where
	scalebase :: !Scale !Point2 !BarrelDomain -> Point2
	scalebase (kx,ky) base {corner1}
		= {x=toInt (kx*(toReal offset.x)),y=toInt (ky*(toReal offset.y))}
	where
		offset	= base-corner1
	
	scalewall :: !Scale !Wall -> Vector2
	scalewall (kx,ky) ({vx,vy},_)
		= {vx=toInt (kx*(toReal vx)),vy=toInt (ky*(toReal vy))}

splitWallsInBarrel :: !Barrel -> (![SingleWall],![SingleWall])
splitWallsInBarrel {bBase,bWalls}
	= (horizontal,vertical)
where
	(_,horizontal,vertical) = seq (map splitwall bWalls) (bBase,[],[])
	
	splitwall :: !Wall !(!Point2,![SingleWall],![SingleWall]) -> (!Point2,![SingleWall],![SingleWall])
	splitwall wall=:(v,interior) (base,horizontal,vertical)
		| v.vx==0	= (base1,horizontal,[wall1:vertical])
		| otherwise	= (base1,[wall1:horizontal],vertical)
	where
		base1		= movePoint v base
		wall1		= (orientLine (base,base1),interior)
		
		orientLine :: !Line -> Line
		orientLine (a=:{x=aX,y=aY},b=:{x=bX,y=bY})
			| aX==bX
				# (minY,maxY) = minmax aY bY
				= ({a & y=minY},{b & y=maxY})
			| otherwise
				# (minX,maxX) = minmax aX bX
				= ({a & x=minX},{b & x=maxX})


//	Common functions:

between :: !a !a !a -> Bool	| Ord a
between x low high
	| x<=high	= x>=low
	| otherwise	= False

minmax :: !a !a -> (!a,!a)	| Ord a
minmax x y
	| x<y		= (x,y)
	| otherwise	= (y,x)

dist :: !Point2 !Point2 -> Real
dist {x=x1,y=y1} {x=x2,y=y2}
	= sqrt (dX*dX+dY*dY)
where
	dX	= toReal (x2-x1)
	dY	= toReal (y2-y1)

scaleSize :: !Size !Size -> Scale
scaleSize size1 size2
	= ((toReal size1.w)/(toReal size2.w),(toReal size1.h)/(toReal size2.h))
