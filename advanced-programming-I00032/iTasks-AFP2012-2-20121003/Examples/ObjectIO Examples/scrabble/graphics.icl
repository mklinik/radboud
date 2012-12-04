implementation module graphics


import	StdInt, StdBool, StdReal, StdChar, StdList, StdFunc, StdEnum, StdArray, StdTuple, StdMisc, StdOrdList
import	StdControl, StdPicture
import	board, language, systemsettings


rbBoardGrey			:== RGB {r=191,g=191,b=191}			// The background colour of the board
rbLighterGrey		:== RGB {r=224,g=224,b=224}
rbBoardRed3			:== RGB {r=255,g=127,b=127}
rbBoardRed2			:== RGB {r=191,g=160,b=160}
rbBoardBlue3		:== RGB {r=127,g=127,b=255}
rbBoardBlue2		:== RGB {r=159,g=159,b=191}
rbSquare			:== RGB {r=255,g=255,b=191}
rbDarkYellow		:== RGB {r=127,g=127,b=0  }

displaywidth		:==	2+250+2
displayheight		:==	2+130+2
boardwidth			:==	391
boardheight			:==	391
squarewidth			::	Int
squarewidth			=:	boardwidth/15
squareheight		::	Int
squareheight		=:	boardheight/15

alphabet			:==	"abcdefghijklmnopqrstuvwxyz"


/*	Mapping 'Amanda-space' to 'Scrabble-space' to 'Pixel-space':
	Amanda-space	: ((-1.0,1.0),(1.0,-1.0))
	Scrabble-space	: ((0.0,0.0), (14.0,14.0))
	Pixel-space		: ((0,0), (width,height))
*/

abs2rel :: !(!Int,!Int) -> (!Int,!Int)
abs2rel (x,y) = (x/squarewidth,y/squareheight)


/*	The drawing operations.	*/

/*	The look of the board.
	It is assumed that the background is set to rbBoardGrey.
*/
boardlook :: !Board Point2 !SelectState !UpdateState !*Picture -> *Picture
boardlook (hor,_) cstate select updState=:{newFrame} picture
	# picture	= setPenColour	White picture
	# picture	= seq [ drawAt {x=squarewidth*i+1,y=0}  {zero & vy=h} \\ i<-is ] picture
	# picture	= seq [ drawAt {x=0,y=squareheight*i+1} {zero & vx=w} \\ i<-is ] picture
	# picture	= setPenColour DarkGrey picture
	# picture	= seq [ drawAt {x=squarewidth*i,y=1}  {zero & vy=h-1} \\ i<-is ] picture
	# picture	= seq [ drawAt {x=1,y=squareheight*i} {zero & vx=w-1} \\ i<-is ] picture
	# picture	= seq (map (drawsquare rbBoardBlue2) doubleletterpositions) picture
	# picture	= seq (map (drawsquare rbBoardBlue3) tripleletterpositions) picture
	# picture	= seq (map (drawsquare rbBoardRed2)  doublewordpositions)   picture
	# picture	= seq (map (drawsquare rbBoardRed3)  triplewordpositions)   picture
	# picture	= drawcenter picture
	# picture	= seq [ drawletter l (i,j) \\ i<-[0..14], j<-[0..14], l<-[(hor!!j)!!i] ] picture
	| enabled select
		= drawfocus True cstate picture
	| otherwise
		= picture
where
	{w,h}		= rectangleSize newFrame
	is			= [0..15]
	
	drawcenter :: *Picture -> *Picture
	drawcenter picture
		# picture	= drawsquare rbBoardGrey (7,7) picture
		# picture	= setPenColour Grey picture
		# picture	= fillAt (absposition (7.5,7.5)) {polygon_shape=shape} picture
		= picture
	where
		h		= (squarewidth-1)/2
		v		= (squareheight-1)/2
		shape	= [{zero & vy=0-v},{vx=h,vy=v},{vx=0-h,vy=v},{vx=0-h,vy=0-v},{vx=h,vy=0-v}]
	
	//	absposition maps a position in 'Scrabble-space' to a position in 'Pixel-space'.
		absposition :: !(!Real,!Real) -> Point2
		absposition (col,row)
			= {x=toInt (col*toReal squarewidth),y=toInt (row*toReal squareheight)}
	
	drawsquare :: !Colour !(!Int,!Int) !*Picture -> *Picture
	drawsquare colour (col,row) picture
		# picture	= setPenColour colour picture
		# picture	= fill {corner1={x=l,y=t},corner2={x=r,y=b}} picture
		= picture
	where
		l =  col    * squarewidth+2
		t =  row    * squareheight+2
		r = (col+1) * squarewidth
		b = (row+1) * squareheight

drawfocus :: !Bool !Point2 !*Picture -> *Picture
drawfocus notErase {x,y} picture
	# picture	= setPenColour	lefttopcolour                picture
	# picture	= setPenPos		{x=l,y=b}                    picture
	# picture	= draw			{vx=0,vy=0-(squareheight-1)} picture
	# picture	= draw			{vx=squarewidth-1,vy=0}      picture
	# picture	= setPenColour  rightbotcolour               picture
	# picture	= draw			{vx=0,vy=squareheight-1}     picture
	# picture	= draw			{vx=0-(squarewidth-1),vy=0}  picture
	= picture
where
	(col,row)						= abs2rel (x,y)
	l								= col*squarewidth+1
	b								= (row+1)*squareheight
	(lefttopcolour,rightbotcolour)	= if notErase (DarkGrey, White)
												  (White, DarkGrey)


drawletter :: !Char !(!Int,!Int) !*Picture -> *Picture
drawletter ' ' _ picture
	= picture
drawletter l (i,j) picture
	# ((_,sfont),picture)	= openFont smallfont picture
	# ((_,lfont),picture)	= openFont letterfont picture
	# (plen,picture)		= getFontStringWidth sfont scoretext										picture
	# picture				= setPenColour	rbSquare													picture
	# picture				= fill	{corner1={x=x+2,y=y+2},corner2={x=x+squarewidth,y=y+squareheight}}	picture
	# picture				= setPenPos		{x=x+2,y=y+squareheight-1}									picture
	# picture				= setPenColour	White														picture
	# picture				= drawLineTo	{x=x+2,y=y+2}												picture
	# picture				= drawLineTo	{x=x+squarewidth-1,y=y+2}									picture
	# picture				= setPenColour	Yellow														picture
	# picture				= drawLineTo	{x=x+squarewidth-1,y=y+squareheight-1}						picture
	# picture				= drawLineTo	{x=x+2,y=y+squareheight-1}									picture
	# picture				= setPenPos		{x=x+squarewidth/4,y=y+h-h/3}								picture
	# picture				= setPenFont	lfont														picture
	# picture				= setPenColour	Black														picture
	# picture				= draw			(toUpper l)													picture
	# picture				= setPenFont	sfont														picture
	# picture				= setPenColour	rbDarkYellow												picture
	# picture				= drawAt		{x=x+squarewidth-2-plen,y=y+h-3} scoretext					picture
	= picture
where
	x						= i*squarewidth
	y						= j*squareheight
	h						= squareheight
	scoretext				= toString (lettervalue l)


redrawboard :: !Id !Board Point2 !(IOSt .l) -> IOSt .l
redrawboard boardId board pos iostate
	= setControlLook boardId True (True,boardlook board pos) iostate

/*	letterboxlook is the look of the set of remaining letters.
	It is assumed that it has the background colour rbBackground.
*/
letterboxlook :: ![Char] SelectState UpdateState !*Picture-> *Picture
letterboxlook letters _ {newFrame} picture
	# picture				= unfill newFrame picture
	# picture				= seq [ drawletter c (0,j) \\ (c,j)<-zip2 leftchars  js ] picture
	# picture				= seq [ drawletter c (2,j) \\ (c,j)<-zip2 rightchars js ] picture
	# ((_,lfont),picture)	= openFont letterfont picture
	# picture				= setPenFont   lfont  picture
	# picture				= setPenColour Black  picture
	# picture				= seq [ drawcount c (1,j) \\ (c,j)<-zip2 leftcounts  js ] picture
	# picture				= seq [ drawcount c (3,j) \\ (c,j)<-zip2 rightcounts js ] picture
	= picture
where
	js						= [0..14]
	counts					= countletters alphabet (sort letters)
	(left,right)			= splitAt 15 counts
	(leftchars, leftcounts)	= unzip left
	(rightchars,rightcounts)= unzip right
	
	drawcount :: !Int !(!Int,!Int) !*Picture -> *Picture
	drawcount count (i,j) picture
		= drawAt {x=x+squarewidth/4,y=y+h-h/3} (toString count) picture
	where
		x = i*squarewidth
		y = j*squareheight
		h = squareheight
	
	countletters :: !String ![Char] -> [(Char,Int)]
	countletters chars letters
		| chars==""
			= []
		| otherwise
			# c					= chars.[0]
			  (count,letters)	= countletter c letters
			= [(c,count):countletters (chars%(1,size chars-1)) letters]
	where
		countletter :: !Char ![Char] -> (Int,![Char])
		countletter c all_letters=:[letter:letters]
			| c<>letter
				= (0,all_letters)
			| otherwise
				# (count,letters)	= countletter c letters
				= (count+1,letters)
		countletter _ _
			= (0,[])

drawletterbox :: !Id ![Char] !(IOSt .l) -> IOSt .l
drawletterbox letterboxId letters iostate
	= setControlLook letterboxId True (True,letterboxlook letters) iostate

drawplayer1letters :: !Id ![Char] !(IOSt .l) -> IOSt .l
drawplayer1letters letters1Id letters iostate
	= setControlLook letters1Id True (True,playerletterslook letters) iostate

drawplayer2letters :: !Id ![Char] !(IOSt .l) -> IOSt .l
drawplayer2letters letters2Id letters iostate
	= setControlLook letters2Id True (True,playerletterslook letters) iostate

playerletterslook :: ![Char] SelectState UpdateState !*Picture -> *Picture
playerletterslook ws _ {newFrame} picture
	= seq [	drawletter c (i,0) \\ c<-ws & i<-[0..] ] (unfill newFrame picture)

drawplayer1score :: !Id !Int !(IOSt .l) -> IOSt .l
drawplayer1score player1scoreId s iostate
	= setControlText player1scoreId (toString s) iostate

drawplayer2score :: !Id !Int !(IOSt .l) -> IOSt .l
drawplayer2score player2scoreId s iostate
	= setControlText player2scoreId (toString s) iostate

drawcommunication :: !Id ![String] !(IOSt .l) -> IOSt .l
drawcommunication displayId text iostate
	= setControlLook displayId True (True,displaylook text) iostate

displaylook :: ![String] SelectState !UpdateState !*Picture -> *Picture		// displaylook assumes PictureDomain
																			// {{-2,-2},{w+2,h+2}}
displaylook text _ {newFrame} picture
	# picture				= unfill       newFrame     picture
	# picture				= drawdisplay  size			picture
	# ((_,font12),picture)	= openFont     (font 12)	picture
	# picture				= setPenFont   font12		picture
	# picture				= setPenColour Red			picture
	# picture				= seq [ drawAt {x=2+w/20,y=2+h*y/10} l \\ (y,l)<-zip2 [2,4..] text ] picture
	= picture
where
	size					= rectangleSize newFrame
	{w,h}					= size

drawprogress :: !Id !Player !Progress !Placing !(IOSt .l) -> IOSt .l
drawprogress displayId player progress placing iostate
	= setControlLook displayId True (True,progresslook player progress placing {w=displaywidth,h=displayheight}) iostate
where
	progresslook :: !Player !Progress !Placing !Size SelectState UpdateState !*Picture -> *Picture
	progresslook player (Letter letter _) placing size=:{w,h} _ {newFrame} picture
		# picture				= unfill newFrame									picture
        # ((_,thefont),picture)	= openFont (font 12)								picture
		# (foundlength,picture)	= getFontStringWidth thefont found_upto_now			picture
		# (at_poslength,picture)= getFontStringWidth thefont at_pos					picture
		# (scorelength,picture)	= getFontStringWidth thefont score_upto_now			picture
		  rtabstop				= tekstindent+foundlength
		  atpos					= rtabstop - at_poslength
          scorepos				= rtabstop - scorelength
		# picture				= drawdisplay	size								picture
		# picture				= setPenFont	thefont								picture
		# picture				= setPenColour	Grey								picture
		# picture				= drawAt		letterspos alphabet					picture
		# picture				= setPenColour	Green								picture
		# picture				= drawAt		letterspos alphabet_l_incl			picture
		# picture				= setPenColour	Red									picture
		# picture				= drawAt		letterspos alphabet_l_excl			picture
		# picture				= setPenColour	Green								picture
		# picture				= drawAt		{x=tekstindent,y=toInt (0.15*h`)} (toString player+++determines_new_word) picture
		# picture				= setPenPos		{x=foundpos,   y=toInt (0.60*h`)}	picture
		# picture				= draw			found_upto_now						picture
		# picture				= movePenPos	{vx=10,vy=0}						picture
		# picture				= draw			placing.word						picture
		# picture				= setPenPos		{x=atpos,      y=toInt (0.75*h`)}	picture
		# picture				= draw			at_pos								picture
		# picture				= movePenPos	{vx=10,vy=0}						picture
		# picture				= draw			placingtext							picture
		# picture				= setPenPos		{x=scorepos,   y=toInt (0.90*h`)}	picture
		# picture				= draw			score_upto_now						picture
		# picture				= movePenPos	{vx=10,vy=0}						picture
		# picture				= draw			(toString placing.score)			picture
		= picture
	where
		(x,y)					= placing.pos
		foundpos				= tekstindent
		
		w`						= toReal w
		h`						= toReal h
		letterspos				= {x=tekstindent,y=toInt (0.35*h`)}
		tekstindent				= toInt (0.05*w`)
		
		alphabet_l_excl			= if (letter=='a') "" (alphabet%(0,l_index-1))
		alphabet_l_incl			= alphabet%(0,l_index)
		l_index					= toInt letter-a_index
		a_index					= toInt 'a'
		
		placingtext				= toString (x,y)+++" "+++toString placing.dir
	progresslook player (Finish _) placing {w,h} _ _ picture
		# picture				= setPenColour	Grey								picture
		# picture				= fill			{corner1=zero,corner2={x=w,y=h}}	picture
		# ((_,font12),picture)	= openFont		(font 12)							picture
		# picture				= setPenFont	font12								picture
		# picture				= setPenColour	Red									picture
		# picture				= drawAt		{x=toInt (0.05*w`),y=toInt (0.95*h`)} (toString player+++determined_new_word) picture
		= picture
	where
		w`						= toReal w
		h`						= toReal h

/*	drawdisplay draws the display. Note that in the 0.8 version, the domain was assumed to be {{-2,-2},{w+2,h+2}}.
	For this reason, at all coordinates {2,2} must be added.
*/
drawdisplay :: !Size !*Picture -> *Picture
drawdisplay {w,h} picture
	# picture	= setPenColour	Grey			picture
	# picture	= setPenPos		{x=1,  y=h-3}	picture
	# picture	= drawLineTo	{x=1,  y=1}		picture
	# picture	= drawLineTo	{x=w-2,y=1}		picture
	# picture	= setPenPos		{x=0,  y=h-2}	picture
	# picture	= drawLineTo	zero			picture
	# picture	= drawLineTo	{x=w-1,y=0}		picture
	# picture	= setPenColour	White			picture
	# picture	= setPenPos		{x=1,  y=h-2}	picture
	# picture	= drawLineTo	{x=w-2,y=h-2}	picture
	# picture	= drawLineTo	{x=w-2,y=0}		picture
	# picture	= setPenColour	rbLighterGrey	picture
	# picture	= setPenPos		{x=0,  y=h-1}	picture
	# picture	= drawLineTo	{x=w-1,y=h-1}	picture
	# picture	= drawLineTo	{x=w-1,y=0}		picture
	= picture
