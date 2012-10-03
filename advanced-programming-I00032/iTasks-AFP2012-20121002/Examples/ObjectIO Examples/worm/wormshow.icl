implementation module wormshow

import	StdInt, StdBool, StdList, StdFunc, StdString
import	StdPicture
import	wormstate

//	The drawing constants.
WormBackGroundColour	:==	RGB {r=MaxRGB,g=MaxRGB,b=MaxRGB*3/4}
WormFontSize			:==	12
PointsPos				:== {x=72, y=15}
LifesPos				:== {x=255,y= 5}
LevelPos				:== {x=465,y=15}
CornerX					:== 15
CornerY					:== 23
SegSize					:== 4
CellSize				:== 10

//	Draw the game.
DrawGame :: !Level !Food !Points !Worm !Lives !*Picture -> *Picture
DrawGame {level,obstacles} food points worm lives pict
	# pict = unfill	{	corner1={x=CornerX-8,y=0}
					,	corner2={x=CornerX+SizeX*CellSize+16,y=CornerY+SizeY*CellSize+16}
					}					pict
	# pict = DrawBorders				pict
	# pict = DrawObstacles	obstacles	pict
	# pict = DrawPoints		points		pict
	# pict = DrawWorm		worm		pict
	# pict = DrawFood		food		pict
	# pict = DrawLevel		level		pict
	# pict = DrawLives		lives		pict
	= pict
where
	DrawObstacles :: ![Obstacle] !*Picture -> *Picture
	DrawObstacles [] pict
		= pict
	DrawObstacles obstacles pict
		# pict	= setPenColour (RGB {r=MaxRGB/2,g=MaxRGB/2,b=0})	pict
		# pict	= seq (map DrawObstacle obstacles)	pict
		# pict	= setPenColour Black				pict
		= pict
	where
		DrawObstacle :: !Obstacle !*Picture -> *Picture
		DrawObstacle {corner1={x=ltx,y=lty},corner2={x=rbx,y=rby}} pict
			= fill {corner1={x=lx,y=ty},corner2={x=rx,y=by}} pict
		where
			lx	= CornerX+CellSize*ltx-2
			ty	= CornerY+CellSize*lty-2
			rx	= CornerX+CellSize*rbx+2
			by	= CornerY+CellSize*rby+2
	
	DrawPoints :: !Points !*Picture -> *Picture
	DrawPoints points pict
		# pict	= setPenColour Magenta						pict
		# pict	= drawAt {PointsPos & x=x-57} "Points: "	pict
		# pict	= setPenColour	Black						pict
		# pict	= DrawNewPoints	points						pict
		= pict
	where
		{x}		= PointsPos
	
	DrawWorm :: !Worm !*Picture -> *Picture
	DrawWorm [head:rest] pict
		# pict	= seq (map (DrawSegment Red) rest)	pict
		# pict	= DrawSegment	Green head			pict
		# pict	= setPenColour	Black				pict
		= pict
	
	DrawLevel :: !Int !*Picture -> *Picture
	DrawLevel level pict
		# pict	= setPenColour Magenta					pict
		# pict	= drawAt {LevelPos & x=x-50} "Level: "	pict
		# pict	= setPenColour Black					pict
		# pict	= unfill {corner1={x=x-1,y=y-12},corner2={x=x+100,y=y+4}}
		  												pict
		# pict	= drawAt LevelPos (toString level)		pict
		= pict
	where
		{x,y}	= LevelPos
	
	DrawLives :: !Lives !*Picture -> *Picture
	DrawLives lives pict
		| lives<>0
			= DrawLittleWorms lives				pict
		| otherwise
			# pict	= setPenColour	Magenta		pict
			# pict	= drawAt {x=lx-63,y=ly+10} "No more worms!"	pict
			# pict	= setPenColour	Black		pict
			= pict
	where
		{x=lx,y=ly}	= LifesPos
		
		DrawLittleWorms :: !Lives !*Picture -> *Picture
		DrawLittleWorms lives pict
			| lives>0
				= DrawLittleWorms (lives-1) (DrawLittleWorm lives pict)
			| otherwise
				# pict	= setPenColour	Magenta	pict
				# pict	= drawAt {x=lx-63,y=ly+10} "Worms:"	pict
				# pict	= setPenColour	Black	pict
				= pict
		where
			{x=lx,y=ly}	= LifesPos
			
			DrawLittleWorm :: !Int !*Picture -> *Picture
			DrawLittleWorm n pict
				# pict		= setPenSize	5				pict		// PA: 5<--(4,5)
				# pict		= setPenColour	Red				pict
				# pict		= setPenPos		{x=x,y=y}		pict
				# pict		= drawLineTo	{x=x+9,y=y}		pict
				# pict		= setPenColour	Green			pict
				# pict		= drawLineTo	{x=x+10,y=y}	pict
				# pict		= setPenSize	1				pict
				# pict		= setPenColour	Black			pict
				= pict
			where
				x			= lx+20*(dec n / 2) 
				y			= ly+ 7*(dec n rem 2) 
				{x=lx,y=ly}	= LifesPos

DrawBorders :: !*Picture -> *Picture
DrawBorders pict
	# pict	= setPenColour	Black	pict
	# pict	= setPenSize	3		pict
	# pict	= draw	{	corner1={x=CornerX-3,y=CornerY-3}
	  				,	corner2={x=CornerX+SizeX*CellSize+11,y=CornerY+SizeY*CellSize+11}}
									pict
	# pict	= setPenSize	1		pict
	= pict

DrawSegment :: !Colour !Segment !*Picture -> *Picture
DrawSegment color {x,y} pict
	# pict	= setPenColour color pict
	# pict	= fillAt {x=CornerX+CellSize*x,y=CornerY+CellSize*y} {oval_rx=SegSize,oval_ry=SegSize} pict
	= pict

EraseSegment :: !Segment !*Picture -> *Picture
EraseSegment segment pict
	= DrawSegment WormBackGroundColour segment pict

DrawFood :: !Food !*Picture -> *Picture
DrawFood {pos} pict
	# pict	= setPenColour	Magenta					pict
	# pict	= fillAt {x=x,y=y} {box_w=6,box_h=6}	pict
	# pict	= setPenColour	Black					pict
	= pict
where
	x		= CornerX+CellSize*pos.x-3
	y		= CornerY+CellSize*pos.y-3
	
EraseFood :: !Food !*Picture -> *Picture
EraseFood {pos} pict
	= unfillAt {x=x,y=y} {box_w=6,box_h=6} pict
where
	x		= CornerX+CellSize*pos.x-3
	y		= CornerY+CellSize*pos.y-3

DrawNewPoints :: !Points !*Picture -> *Picture
DrawNewPoints points pict
	# pict	= unfill {corner1={x=x-1,y=y-12},corner2={x=x+100,y=y+4}}	pict
	# pict	= drawAt PointsPos (toString points)	pict
	= pict
where
	{x,y}	= PointsPos


//	Show a step of the worm.
DrawStep :: !Bool !Food !Food !Points !Segment !Segment !Segment !*Picture -> *Picture
DrawStep scored oldfood newfood points oldh head tail pict
	| not scored
		= DrawMove oldh head tail	pict
	| otherwise
		# pict	= EraseFood		oldfood			pict
		# pict	= DrawFood		newfood			pict
		# pict	= DrawNewPoints	points			pict
		# pict	= DrawMove		oldh head tail	pict
		= pict
where
	DrawMove :: !Segment !Segment !Segment !*Picture -> *Picture
	DrawMove oldh head {x=0,y=0} pict
		# pict	= DrawSegment	Red					 oldh pict
		# pict	= DrawSegment	Green				 head pict
		# pict	= setPenColour	Black					  pict
		= pict
	DrawMove oldh head tail pict
		# pict	= DrawSegment	Red					 oldh pict
		# pict	= DrawSegment	Green				 head pict
		# pict	= DrawSegment	WormBackGroundColour tail pict
		# pict	= setPenColour	Black					  pict
		= pict


//	Close the Playfield between two levels.
DrawAnimation :: !Int !Int !*Picture -> *Picture
DrawAnimation 40 1 pict
	# pict		= setPenColour	White	pict
	# pict		= DrawBorders			pict
	# pict		= setPenColour	Black	pict
	= pict
DrawAnimation n step pict
	| step<0
		# pict	= setPenSize	3 pict
		# pict	= unfill		{corner1={x=l,y=b},corner2={x=x,y=y}} pict
		# pict	= unfill		{corner1={x=r,y=t},corner2={x=x,y=y}} pict
		# pict	= draw			{corner1={x=l,y=t},corner2={x=r,y=b}} pict
		= pict
	| otherwise
		# pict	= setPenSize	3 pict
		# pict	= unfill		{corner1={x=l,y=b},corner2={x=x,  y=y-3}} pict
		# pict	= unfill		{corner1={x=r,y=t},corner2={x=x-3,y=y  }} pict
		# pict	= draw			{corner1={x=l,y=t},corner2={x=r,  y=b  }} pict
		= pict
where
	l			= CornerX-3
	t			= CornerY-3
	r			= l+w*n
	b			= t+h*n 
	x			= r-step*w
	y			= b-step*h 
	w			= (48+SizeX*CellSize)/40
	h			= (48+SizeY*CellSize)/40
