implementation module bounceDraw


import StdInt, StdReal, StdList, StdFunc
import bounceTypes


drawBarrel :: !UpdateArea !Scale !Barrel !*Picture -> *Picture
drawBarrel updArea scale barrel picture
	# picture		= setPenColour	Grey		picture
	# picture		= seq (map fill updArea)	picture
	# picture		= unfillAt base polygon		picture
	= picture
where
	(base,polygon)	= BarrelToPolygon scale barrel

drawBall :: !Scale !Point2 !Ball !*Picture -> *Picture
drawBall (kx,ky) base {bCenter,bRadius,bColour} picture
	# picture		= setPenColour	bColour		picture
	# picture		= fillAt		center oval	picture
	# picture		= setPenColour	Black		picture
	# picture		= drawAt center oval		picture
	= picture
where
	k				= min kx ky
	r				= (max (toInt (k*(toReal bRadius))) 2)-1
	offset			= bCenter-base
	center			= {x=toInt (kx*(toReal offset.x)),y=toInt (ky*(toReal offset.y))}
	oval			= {oval_rx=r,oval_ry=r}

eraseBall :: !Scale !Point2 !Ball !*Picture -> *Picture
eraseBall (kx,ky) base {bCenter,bRadius} picture
	# picture		= unfillAt center oval picture
	# picture		= undrawAt center oval picture
	= picture
where
	r				= (max (toInt (k*(toReal bRadius))) 2)-1
	k				= min kx ky
	offset			= bCenter-base
	center			= {x=toInt (kx*(toReal offset.x)),y=toInt (ky*(toReal offset.y))}
	oval			= {oval_rx=r,oval_ry=r}
