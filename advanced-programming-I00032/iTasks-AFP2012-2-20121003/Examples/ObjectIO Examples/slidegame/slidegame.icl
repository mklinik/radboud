module slidegame


//	**************************************************************************************************
//
//	A simple slide game that uses bitmaps to show nice pictures.
//	On Macintosh: 
//		* Make sure the application has sufficient 'Extra memory' (Application options)
//		* Select a PICT file;
//	On Windows(9x/NT):
//		* Make sure the application has sufficient 'Heap' (Application options)
//		* Select a BMP file.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************


import StdEnv, StdIO, Random


/*	openSlideGame first attempts to read in the bitmap.
	If successfull, openSlideGame then checks whether the given bitmap has proper dimensions.
	If this is the case then a window is opened that will contain the slide game.
	The initial positions of the slides are determined by shuffling them nr_shuffle times randomly. 
	The local state of the window keeps track of the current position of the hole.
*/
::	WindowState
	=	{	curHole	:: !Coord		// The current position of the hole
		}
::	Coord
	=	{	col		:: !Int			// The zero based column number
		,	row		:: !Int			// The zero based row    number
		}

Start :: *World -> *World
Start world
	# (maybeFile,world)		= selectInputFile world
	| isNothing maybeFile
		= world
	# (maybeBitmap,world)	= openBitmap (fromJust maybeFile) world
	| isNothing maybeBitmap
		= world
	# bitmap				= fromJust maybeBitmap
	  bitmapsize			= getBitmapSize bitmap
	  blocksize				= {w=bitmapsize.w/4,h=bitmapsize.h/4}
	| not (ok_blocksize blocksize)
		= world
	| otherwise
		# (seed,world)		= getNewRandomSeed world
		  (okCoords,hole)	= initlast [{col=col,row=row} \\ row<-[0..3],col<-[0..3]]
		  (_,coords,hole)	= iteraten nr_shuffle shuffle (seed,zip2 okCoords okCoords,hole)
		# (windowId,world)	= openId world
		# (allcids, world)	= openIds   15 world
		# (allr2ids,world)	= openR2Ids 15 world
		= startIO SDI Void (openSlideWindow hole bitmap blocksize windowId allcids allr2ids coords) [ProcessClose closeProcess] world
where
	nr_shuffle				= 200
	openSlideWindow hole bitmap blocksize windowId allcids allr2ids coords pState
		= snd (openWindow {curHole=hole} (window bitmap blocksize windowId allcids allr2ids coords) pState)
	
	ok_blocksize {w,h}
		= isbetween minblocksize.w w maxblocksize.w && isbetween minblocksize.h h maxblocksize.h
	where
		minblocksize		= {w=20,h=20}
		maxblocksize		= {w=maxFixedWindowSize.w/4,h=maxFixedWindowSize.h/4}
	
	shuffle :: (RandomSeed,[(Coord,Coord)],Coord) -> (RandomSeed,[(Coord,Coord)],Coord)
	shuffle (seed,coords,hole)
		# (candidates,others)	= splitFilter (\(okCoord,coord)->distCoord coord hole==1) coords
		  (random_nr,seed)		= random seed
		  (before,[(okCandidate,candidate):after])
		  						= splitAt (random_nr rem (length candidates)) candidates
		= (seed,before++[(okCandidate,hole):after]++others,candidate)


/*	window defines the Window that shows the slide game.
	It contains a list of slide controls.
	Closing the window will terminate the program.
*/
window :: Bitmap Size Id [Id] [SlideR2Id] [(Coord,Coord)]
	-> Window (ListLS (AddLS (:+: CustomButtonControl (Receiver2 SlideMsgIn Bool)))) WindowState (PSt .l)
window bitmap blocksize windowId allcids allr2ids coords
	= Window "SlideGame"
		(	ListLS (map (slideControl bitmap blocksize windowId allr2ids) coord_ids)
		)
		[	WindowClose 	(noLS closeProcess)
		,	WindowId		windowId
		,	WindowItemSpace 0 0
		,	WindowViewSize	{w=4*blocksize.w,h=4*blocksize.h}
		,	WindowPen		[PenBack Grey]
		]
where
	coord_ids	= zip2 coords (zip2 allcids allr2ids)


/*	slideControl defines one slide control of the slide game.
	A slide control consists of two components:
	*	A custom button control:
			This control shows a part of the bitmap image.
			Selecting this control will swap places with the current hole iff it is adjacent to the 
			hole.
			It checks whether all slide controls are at their desired locations, and if so disables 
			the window.
			Note that disabling the window will disable all slide controls. The look of a slide control 
			is such that in disabled state it will not frame its bitmap part, so the complete bitmap 
			will be displayed.
	*	A receiver control:
			This control handles external requests that inform whether the slide control is at its 
			desired position.
*/
::	SlideState						// The local state of a slide control
	=	{	curCoord :: Coord		// The current location of the slide control
		}
::	SlideMsgIn						// The ingoing messages of the slide control
	=	AreYouOk					// Inform whether the control is currently at its desired location
::	SlideMsgOut						// The outgoing messages of the slide control
	:==	Bool						// True iff the control is currently at its desired location
::	SlideR2Id						// Shorthand for the receiver id of a slide control
	:==	R2Id SlideMsgIn SlideMsgOut
::	SlideControl ls pst				// Shorthand for the slide control constructor type
	:==	AddLS (:+: CustomButtonControl (Receiver2 SlideMsgIn SlideMsgOut)) ls pst

slideControl :: Bitmap Size Id [SlideR2Id] ((Coord,Coord),(Id,SlideR2Id))
	-> SlideControl WindowState (PSt .l)
slideControl bitmap size windowId allr2ids ((okCoord,initCoord),(cid,r2id))
	= {	addLS	= { curCoord=initCoord }
	  ,	addDef	= custombutton :+: receiver2
	  }
where
	others			= removeMember r2id allr2ids
	
	custombutton	= CustomButtonControl size slideLook
						[	ControlPos		(LeftTop,OffsetVector (offset initCoord))
						,	ControlFunction slideMove
						,	ControlId		cid
						]
	slideLook select {newFrame} picture
		# picture			= drawAt {x=0-okCoord.col*size.w,y=0-okCoord.row*size.h} bitmap picture
		| enabled select	= draw newFrame picture
		| otherwise			= picture
	offset {col,row}= {vx=size.w*col,vy=size.h*row}
	
	slideMove :: (*(SlideState,WindowState),PSt .l) -> (*(SlideState,WindowState),PSt .l)
	slideMove ((slide=:{curCoord},ls=:{curHole}),pst)
		| distCoord curCoord curHole<>1
			= ((slide,ls),pst)
		# slide				= {slide & curCoord=curHole }
		  ls				= {ls    & curHole =curCoord}
		# (_,pst)			= accPIO (setControlPos windowId [(cid,(LeftTop,OffsetVector (offset curHole)))]) pst
		# i_am_ok			= curHole==okCoord
		| not i_am_ok
			= ((slide,ls),pst)
		# (others_ok,pst)	= seqList (map areYouOk others) pst
		| and others_ok
			= ((slide,ls),appPIO (disableWindow windowId) pst)
		| otherwise
			= ((slide,ls),pst)
	
	areYouOk :: SlideR2Id (PSt .l) -> (Bool,PSt .l)
	areYouOk r2id pst
		# (response,pst)	= syncSend2 r2id AreYouOk pst
		= (fromJust (snd response),pst)
	
	receiver2	= Receiver2 r2id receive2 []
	
	receive2 :: SlideMsgIn *(*(SlideState,.ls),PSt .l) -> (SlideMsgOut,*(*(SlideState,.ls),PSt .l))
	receive2 AreYouOk (slide=:({curCoord},_),pst)
		= (okCoord==curCoord,(slide,pst))

//	The distance between two Coords:
distCoord :: !Coord !Coord -> Int
distCoord coord1 coord2
	= abs (coord1.col-coord2.col) + abs (coord1.row-coord2.row)

instance zero Coord where
	zero = {col=0,row=0}
instance == Coord where
	(==) coord1 coord2 = distCoord coord1 coord2==0

//	Generally useful functions:
initlast :: ![.a] -> ([.a],.a)							// (init xs,last xs)
initlast [a]
	= ([],a)
initlast [a:as]
	= ([a:init],last)
where
	(init,last)	= initlast as
initlast []
	= abort "initlast of []"

splitFilter :: (a -> .Bool) !.[a] -> (.[a],.[a])		// (filter cond xs,filter (not o cond) xs)
splitFilter f [a:as]
	| f a		= ([a:yes],no)
	| otherwise	= (yes,[a:no])
where
	(yes,no)	= splitFilter f as
splitFilter f []
	= ([],[])

iteraten :: !Int (a -> a) a -> a
iteraten n f x
	| n<=0		= x
	| otherwise	= iteraten (n-1) f (f x)

isbetween :: !a !a a -> Bool	| Ord a
isbetween low x up
	= low<=x && x<=up
