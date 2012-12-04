implementation module StdPrint

import StdArray, StdBool, StdEnum, StdInt, StdList, StdTuple
import commondef, StdMaybe, StdPicture
import osprint

::	PageDimensions
	=	{	page		::	!Size
		,	margins		::	!Rectangle
		,	resolution	::	!(!Int,!Int)
		}

defaultPrintSetup	::	!*env -> (!PrintSetup, !*env) | FileEnv env
defaultPrintSetup env
	= os_defaultprintsetup env

printSetupDialog	::	!PrintSetup !*printEnv -> (!PrintSetup, !*printEnv)
					|	PrintEnvironments printEnv
printSetupDialog printSetup env
	= os_printsetupdialog printSetup env

getPageDimensions	::	!PrintSetup !Bool	->	PageDimensions
getPageDimensions printSetup emulateScreenRes
	# ((w,h),((x1,y1),(x2,y2)),resolution)	= os_getpagedimensions printSetup emulateScreenRes
	= {page={w=w,h=h}, margins={corner1={x=x1,y=y1},corner2={x=x2,y=y2}}, resolution=resolution}

instance == PageDimensions
  where
	(==) {page=page1,margins=margins1,resolution=resolution1}
		 {page=page2,margins=margins2,resolution=resolution2}
		= page1==page2 && margins1==margins2 && resolution1==resolution2


fwritePrintSetup	::	!PrintSetup !*File -> *File
fwritePrintSetup printSetup file
	#!	string	= os_printsetuptostring printSetup
		hexChars	= [ nibbleToChar (if low (lowNibble string.[i]) (highNibble string.[i]))
						\\ i<-[0..(size string)-1], low<-[True, False] ]
	= fwrites (toString hexChars+++" ") file
	where
		lowNibble ch	= (toInt ch) bitand 0xF
		highNibble ch	= ((toInt ch)>>4) bitand 0xF
		nibbleToChar nibble
			|	10<=nibble && nibble<=15	= toChar (nibble-10+(toInt 'A'))
			|	 0<=nibble && nibble<=9		= toChar (nibble+(toInt '0'))
			

freadPrintSetup		::	!*File !*env -> (!Bool, !PrintSetup, !*File, !*env)	| FileEnv env
freadPrintSetup file env
	#!	(hexChList, file)	= readline [] file
		chList				= map hexToChar (pair hexChList)
		printSetup			= os_stringtoprintsetup (toString chList)
		(valid, env)		= os_printsetupvalid printSetup env
	|	not valid
		#!	(defaultPS, env)= os_defaultprintsetup env
		= (False, defaultPS, file ,env)
	= (True, printSetup, file, env)
  where
	readline akku file
		#!	(ok, ch, file)	= freadc file
		|	ok && isMember ch (['0'..'9']++['A'..'F'])
			= readline [ch:akku] file
		= (reverse akku, file)
	pair [] = []
	pair [x] = [(x,'0')]
	pair [x,y:rest] = [(x,y): pair rest]	
	hexToChar (lowNibble,highNibble)
		= (nibbleToInt lowNibble)+16*(nibbleToInt highNibble)
	nibbleToInt ch
		|	'A'<=ch && ch<='F'	= (toInt ch) - (toInt 'A') + 10
		|	'0'<=ch && ch<='9'	= digitToInt ch


print :: !Bool !Bool
		 .(PrintInfo *Picture -> ([IdFun *Picture],*Picture))
         !PrintSetup !*printEnv 
      -> (!PrintSetup, !*printEnv)
      | PrintEnvironments printEnv
print doDialog emulateScreen prFun printSetup printEnv
	# (finalState,printEnv) = os_printpageperpage doDialog emulateScreen 0 initFun stateTransition printSetup printEnv
	= case finalState of
		Cancelled _						->	(printSetup,printEnv)
		StartedPrinting (_,printSetup2)	->	(printSetup2,printEnv)
  where
  	initFun _ printInfo=:{printSetup} picture
  		# (drawFuns,picture) = prFun printInfo picture
  		= ((isEmpty drawFuns,zero), ((drawFuns,printSetup),picture))
	stateTransition (([drawFun:rest],printSetup),picture)
  		=((isEmpty rest,zero), ((rest,printSetup), drawFun picture))

printUpdateFunction
		:: 	!Bool (UpdateState -> *Picture -> *Picture) [Rectangle]
			!PrintSetup !*printEnv 
		->	(!PrintSetup, !*printEnv)
		| PrintEnvironments printEnv
printUpdateFunction doDialog updateFunc rectangles printSetup printEnv
	# (result, printEnv) = os_printpageperpage doDialog True Nothing initState pageTrans printSetup printEnv
	  outPrintSetup = case result of
				StartedPrinting (outPrintSetup,_)	-> outPrintSetup
				Cancelled s							-> printSetup
	= (outPrintSetup, printEnv)
  where
	initState s printInfo=:{ printSetup, jobInfo={range=(first,last), copies} } picture
		= (	( isEmpty printedClips,(hd printedClips).corner1),
			( (printSetup, printedClips), picture )
		  )
		where
			{page={w=wP,h=hP}}	= getPageDimensions printSetup True
			printedClips = flatten (repeatn copies (allClips % (first-1,last-1)))
			allClips = flatten (map clipsOfOneRectangle rectangles)
			clipsOfOneRectangle rectangle
				= clipRectangles
				where
					{rleft=x1,rtop=y1,rright=x2,rbottom=y2} = rectangleToRect rectangle
					wR = x2-x1+1
					hR = y2-y1+1
					columns = [0..(ceilOfRatio wR wP)-1]
					rows = [0..(ceilOfRatio hR hP)-1]
					clipRectangles = [ { corner1 = { x=c*wP+x1, y=r*hP+y1 },
										 corner2 = { x=min ((c+1)*wP+x1) x2,
										   			 y=min ((r+1)*hP+y1) y2
										 }} \\ r<-rows,c<-columns]
					ceilOfRatio num denum 		// ceil (num/denom)
						| num rem denum == 0
							= num/denum
						= num/denum + 1
					min x y
						| x>y = y
						=x
	pageTrans ((printSetup, [clipRect:rest]), picture)
		# drawFunction = updateFunc (rectangleToUpdateState clipRect)
		= ( (isEmpty rest,if (isEmpty rest) zero (hd rest).corner1),
		 	((printSetup,rest), appClipPicture (toRegion clipRect) drawFunction picture)
		  )

printPagePerPage ::	!Bool !Bool 
					.x
					.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
					((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
					!PrintSetup !*printEnv 
				-> 	(Alternative .x .state,!*printEnv)
		        | PrintEnvironments printEnv
printPagePerPage doDialog emulateScreen x initFun transFun printSetup printEnv
	= os_printpageperpage doDialog emulateScreen x initFun transFun printSetup printEnv

instance PrintEnvironments World
  where
	os_printpageperpage p1 p2 p3 p4 p5 p6 world
		= accFiles (os_printpageperpage p1 p2 p3 p4 p5 p6) world
	os_printsetupdialog p1 world
		= accFiles (os_printsetupdialog p1) world
