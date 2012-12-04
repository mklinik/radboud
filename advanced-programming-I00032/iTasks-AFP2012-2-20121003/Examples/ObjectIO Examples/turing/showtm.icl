implementation module showtm

import	StdArray, StdBool, StdClass, StdInt
from	StdString		import instance % {#}
import	StdPicture
import	tm

StatePos	:== {x=10, y=17}
ErrorPos	:== {x=10, y=17}
NamePos		:== {x=130,y=17}
TapeY		:== 40
Room		:== 14
Offset		:== 10
TransY		:== 40
MaxX		:== 29900

/*	Draw a Turing machine: tape, transitions, name and state.
*/
ShowTape :: !Tape !*Picture -> *Picture
ShowTape {content,head} pic
	# pic	= unfill		{zero & corner2={x=MaxX,y=100}}	pic
	# pic	= ShowCont		0 (size content) Offset content	pic
	# pic	= DrawTapeFrame									pic
	# pic	= DrawHeadRect	(HeadPos head) Red				pic
	= pic
where
	ShowCont :: !Int !Int Int !String !*Picture -> *Picture
	ShowCont i l x s pic
		| i==l
			= pic
		| otherwise
			# pic	= setPenPos	{x=x,y=TapeY}		pic
			# pic	= draw		(toString s.[i])	pic
			# pic	= ShowCont	(i+1) l (x+Room) s	pic
			= pic
	
	DrawTapeFrame :: !*Picture -> *Picture
	DrawTapeFrame pic
		# pic	= drawLine {x=x,y=y2} {x=MaxX,y=y2}	pic
		# pic	= drawLine {x=x,y=y1} {x=MaxX,y=y1}	pic
		# pic	= DrawCellBorders x y1 y2			pic
		= pic
	where
		x		= Offset - 4
		y1		= TapeY  - 13
		y2		= TapeY  + 5
		
		DrawCellBorders :: !Int Int Int !*Picture -> *Picture
		DrawCellBorders x y1 y2 pic
			| x>MaxX
				= pic
			| otherwise
				# pic	= drawLine			{x=x,y=y2} {x=x,y=y1}	pic
				# pic	= DrawCellBorders	(x+Room) y1 y2			pic
				= pic

ShowTransitions :: ![Transition] !String !*Picture -> *Picture
ShowTransitions trs state pic
	# pic	= unfill			{zero & corner2={x=MaxX,y=300}}	pic
	# pic	= ShowState			state							pic
	# pic	= ShowTransFrame									pic
	# pic	= DrawTransitions	0 trs							pic
	= pic
where
	ShowState :: !String !*Picture -> *Picture
	ShowState state pic
		# pic	= draw		{corner1={x=x-4,y=y-11},corner2={x=x+101,y=y+4}}	pic
		# pic	= drawAt	StatePos "State:"									pic
		# pic	= ShowNextState	state											pic
		= pic
	where
		{x,y}	= StatePos
	
	ShowTransFrame :: !*Picture -> *Picture
	ShowTransFrame pic
		# pic	= draw {corner1={x=Offset-4,y=y1},corner2={x=limit,y=y2+1}}	pic
		# pic	= ShowTransBorders (Offset+135) limit y1 y2					pic
		= pic
	where
		limit	= MaxX-80
		y1		= TransY-14
		y2		= TransY+201
		
		ShowTransBorders :: !Int !Int Int Int !*Picture -> *Picture
		ShowTransBorders x limit y1 y2 pic
			| x>=limit
				= pic
			| otherwise
				# pic	= drawLine {x=x,y=y2} {x=x,y=y1} pic
				= ShowTransBorders (x+140) limit y1 y2 pic
	
	DrawTransitions :: !Int ![Transition] !*Picture -> *Picture
	DrawTransitions n [transition:transitions] pic
		# pic	= DrawTrans n transition pic
		# pic	= DrawTransitions (n+1) transitions pic
		= pic
	DrawTransitions _ _ pic
		= pic

ShowTransition :: !Int !Int !*Picture -> *Picture
ShowTransition old new pic
	# pic	= DrawTransRect old White pic
	# pic	= DrawTransRect new Red pic
	# pic	= setPenColour Black pic
	= pic
where
	DrawTransRect :: !Int !Colour !*Picture -> *Picture
	DrawTransRect nr colour pic
		# pic	= setPenColour	colour												pic
		# pic	= draw			{corner1={x=x-1,y=y-11},corner2={x=x+133,y=y+4}}	pic
		= pic
	where
		{x,y}	= TransPos nr

DrawTrans :: !Int !Transition !*Picture -> *Picture
DrawTrans n {start,sigma,end,move} pic
	= drawAt {x=x+5,y=y} (start+++","+++toString sigma+++" -> "+++end+++","+++toString move) pic
where
	{x,y}	= TransPos n

ShowTapePart :: !Tape !Int !Int !*Picture -> *Picture
ShowTapePart {content,head} start end pic
	# pic	= drawLine		{x=x,y=y2} {x=MaxX,y=y2}							pic
	# pic	= drawLine		{x=x,y=y1} {x=MaxX,y=y1}							pic
	# pic	= ShowContPart	0 (size content) Offset content (start-30) (end+30)	pic
	# pic	= DrawHeadRect	(HeadPos head) Red									pic
	= pic
where
	x		= Offset - 4
	y1		= TapeY  - 13
	y2		= TapeY  + 5
	
	ShowContPart :: Int Int !Int String Int !Int !*Picture -> *Picture
	ShowContPart i l x s f t pic
		| x>t		= pic
		| x<f		= ShowContPart	(i+1) l (x+Room) s f t	pic
		# pic		= drawLine {x=x-4,y=TapeY+5} {x=x-4,y=TapeY-13}	pic
		| i>=l		= ShowContPart	(i+1) l (x+Room) s f t	pic
		# pic		= drawAt {x=x,y=TapeY} (toString s.[i])			pic
		| otherwise	= ShowContPart	(i+1) l (x+Room) s f t	pic


/*	Make a step of the T.M. (transition) visible on the screen.
*/
ShowNewTape :: !Comm !Int !*Picture -> *Picture
ShowNewTape com pos pic
	= ShowComm com (HeadPos pos) pic
where
	ShowComm :: !Comm !Int !*Picture -> *Picture
	ShowComm Erase pos pic
		# pic	= EraseCell		pos pic
		# pic	= MoveToHeadPos	pos	pic
		# pic	= draw			"#"	pic
		= pic
	ShowComm None pos pic
		= pic
	ShowComm (Write c) pos pic
		# pic	= EraseCell				pos	pic
		# pic	= MoveToHeadPos			pos	pic
		# pic	= draw		(toString c)	pic
		= pic
	ShowComm MoveR1 pos pic
		# pic	= drawAt	{x=newpos+2,y=TapeY} "#"	pic
		# pic	= DrawHeadRect	pos    White			pic
		# pic	= DrawHeadRect	newpos Red				pic
		= pic
	where
		newpos	= pos+Room
	ShowComm MoveR pos pic
		# pic	= DrawHeadRect pos White pic
		# pic	= DrawHeadRect newpos Red pic
		= pic
	where
		newpos	= pos+Room
	ShowComm MoveL pos pic
		# pic	= DrawHeadRect pos White pic
		# pic	= DrawHeadRect newpos Red pic
		= pic
	where
		newpos	= pos-Room
	ShowComm Halt pos pic
		= pic
	ShowComm ErrorL pos pic
		= DrawError "Error: Head went over left edge." pic
	ShowComm ErrorT pos pic
		= DrawError "Error: No Transition applicable." pic
	ShowComm x pos pic
		= DrawError "Fatal Error: Unknown Command." pic

ShowNextState :: !String !*Picture -> *Picture
ShowNextState state pic
	# pic		    = setPenColour			Red														pic
    # (width,pic)	= getPenFontStringWidth	"State: "												pic
	# pic		    = unfill				{corner1={x=x+width,y=y-10},corner2={x=x+100,y=y+3}}	pic
	# pic		    = drawAt				{x=x+width+1,y=y} state									pic
	# pic		    = setPenColour      	Black													pic
	= pic
where
	{x,y}			= StatePos

DrawHeadRect :: !Int !Colour !*Picture -> *Picture
DrawHeadRect pos colour pic
	# pic	= setPenColour	colour														pic
	# pic	= draw			{corner1={x=pos,y=TapeY-11},corner2={x=pos+11,y=TapeY+4}}	pic
	# pic	= setPenColour	Black														pic
	= pic

HeadPos :: !Int -> Int
HeadPos pos
	= Offset+Room*pos-2

TransPos :: !Int -> Point2
TransPos nr
	= {x=Offset+140*(nr/14),y=TransY+15*(nr rem 14)}

MoveToHeadPos :: !Int !*Picture -> *Picture
MoveToHeadPos pos pic
	= setPenPos {x=pos+2,y=TapeY} pic

EraseCell :: !Int !*Picture -> *Picture
EraseCell x pic
	= unfill {corner1={x=x+1,y=TapeY-10},corner2={x=x+10,y=TapeY+3}} pic

DrawError :: !String !*Picture -> *Picture
DrawError mes pic
	# (width,pic)	= getPenFontStringWidth mes											pic
	# pic			= draw		{corner1={x=x-5,y=y-11},corner2={x=x+width+5,y=y+4}}	pic
	# pic			= drawAt	ErrorPos mes											pic
	= pic
where
	{x,y}			= ErrorPos

EraseError :: !*Picture -> *Picture
EraseError pic
	= unfill {corner1={x=x-5,y=y-11},corner2={x=x+299,y=y+4}} pic
where
	{x,y}	= ErrorPos


/*	For the dialogs:
*/
FourCharString :: !String -> String
FourCharString str
	| size str>4	= str%(0,3)
	| otherwise		= str

FirstChar :: !String -> Char
FirstChar str
	| size str==0	= '#'
	| otherwise		= str.[0]


/*	ClickedIn... determines where the mouse clicked: on a tape cell,
	on a transition, on the state or on the name.
*/
ClickedInWindow :: !Point2 -> (!Int,!Bool,!Bool)
ClickedInWindow pos=:{x,y}
	| trans				= (trnr,True,False)
	| state				= (0, False, True )
	| otherwise			= (0, False, False)
where
	trans				= InRectangle pos {corner1={x=Offset,  y=TransY-13},corner2={x=MaxX,     y=TransY+201}}
	state				= InRectangle pos {corner1={x=statex-3,y=statey-10},corner2={x=statex+79,y=statey+3  }}
	trnr				= (x-Offset)/120 * 14 + (y-(TransY-10))/15
	{x=statex,y=statey}	= StatePos

ClickedInTapeWd :: !Point2 -> (!Int,!Bool)
ClickedInTapeWd pos=:{x,y}
	| tape			= (tpos,True)
	| otherwise		= (0,False)
where
	tape			= InRectangle pos {corner1={x=Offset,y=TapeY-11},corner2={x=MaxX,y=TapeY+4}}
	tpos			= (x-Offset+3)/Room

InRectangle :: !Point2 !Rectangle -> Bool
InRectangle {x,y} {corner1={x=lx,y=ly},corner2={x=ux,y=uy}}
	= x>=lx && x<ux && y>ly && y<uy


/*	Functions to show a change of the T.M. when the T.M. is edited.
*/
HiliteTransition :: !Int !Transition !*Picture -> *Picture
HiliteTransition tnr transition pic
	# pic	= setPenColour	Yellow											pic
	# pic	= fill			{corner1={x=x,y=y-9},corner2={x=x+131,y=y+2}}	pic
	# pic	= setPenColour	Black											pic
	# pic	= DrawTrans		tnr transition									pic
	= pic
where
	{x,y}	= TransPos tnr
		   
HiliteState :: !String !*Picture -> *Picture
HiliteState state pic
	# pic	= setPenColour	Yellow											pic
	# pic	= fill			{corner1={x=x+39,y=y-9},corner2={x=x+78,y=y+2}}	pic
	# pic	= drawAt		{x=x+40,y=y} state								pic
	# pic	= setPenColour	Black											pic
	= pic
where
	{x,y}	= StatePos

HiliteCell :: !Int !Char !*Picture -> *Picture
HiliteCell pos cell pic
	# pic	= EraseError															pic
	# pic	= setPenColour	Yellow													pic
	# pic	= fill			{corner1={x=x+1,y=TapeY-10},corner2={x=x+10,y=TapeY+3}}	pic
	# pic	= setPenColour	Black													pic
	# pic	= drawAt		{x=x+2,y=TapeY} (toString cell)							pic
	= pic
where
	x		= HeadPos pos

ShowTrans :: !Int !Transition !*Picture -> *Picture
ShowTrans tnr transition pic
	# pic	= EraseTrans tnr pic
	# pic	= DrawTrans  tnr transition pic
	= pic

EraseTrans :: !Int !*Picture -> *Picture
EraseTrans tnr pic
	= unfill {corner1={x=x,y=y-9},corner2={x=x+131,y=y+2}} pic
where
	{x,y}	= TransPos tnr

DrawTapeCell :: !Int !Char !*Picture -> *Picture
DrawTapeCell pos cell pic
	# pic	= EraseCell	x								pic
	# pic	= drawAt	{x=x+2,y=TapeY} (toString cell)	pic
	= pic
where
	x		= HeadPos pos

ShowHeadMove :: !Tape Int Int Int !*Picture -> *Picture
ShowHeadMove tape=:{head} end left right pic
	# pic	= ShowTapePart tape left right			pic
	# pic	= DrawHeadRect (HeadPos head)	White	pic
	# pic	= DrawHeadRect (HeadPos end)	Red		pic
	= pic

//	Set the font of the Turing machine windows.

SetTuringFont :: !*Picture -> *Picture
SetTuringFont pic
	# ((_,font),pic)	= openFont {fName="Courier",fStyles=[],fSize=10} pic
	= setPenFont font pic
