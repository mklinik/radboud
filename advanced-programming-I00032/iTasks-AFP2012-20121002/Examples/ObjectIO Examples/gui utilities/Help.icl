implementation module Help

//	**************************************************************************************************
//
//	General utility for handling information about the application and present help.
//
//	This module has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import	StdArray, StdBool, StdFile, StdFunc, StdInt, StdList, StdTuple, StdMisc
import	StdId, StdProcess, StdPSt, StdPStClass, StdSystem, StdWindow

::	NoState		=	NoState
::	InfoDef		:== (Size,[InfoLine])
::	InfoLine	:== (InfoFontDef,Int,Int,String)
::	InfoFontDef	=	InfoFont Font	Centered
				|	NoFont			Centered
::	Centered	:== Bool
::	Fonts
	=	{	normal		:: Font
		,	large		:: Font
		,	bold		:: Font
		,	large_bold	:: Font
		}
::	Heights		:== (Int,Int)

InfoFontName1	:== SansSerifFontDef.fName
InfoFontName2	:== SerifFontDef.fName
NormalSize1		:== 9
NormalSize2		:== 12
LargeSize1		:== 12
LargeSize2		:== 14
Margin			:== 8
AboutBegin		:== "\\About"
AboutEnd		:== "\\EndAbout"
HelpBegin		:== "\\Help"
HelpEnd			:== "\\EndHelp"
About			:== False
Help			:== True


/*	showAbout opens a window:
	-	it has the title of the application name (String argument 1),
	-	it displays the about information of the application (found in the helpfile, name argument 2),
	-	it has an Ok button that closes this window, 
	-	it has a Help button that displays the help information (see showHelp).
*/
showAbout :: String String (PSt .l) -> PSt .l
showAbout appname helpfile pState
	# (okId, pState)		= accPIO openId pState
	# (fonts,pState)		= accPIO (accScreenPicture infoFonts) pState
	  {normal}				= fonts
	# ((size,text),pState)	= readInfo About fonts AboutBegin AboutEnd helpfile pState
	  about					= Dialog ("About "+++appname)
	  							(	CustomControl size (look normal text)
  															[ ControlPos		(Center,NoOffset)]
	  							:+:	ButtonControl "Ok"		[ ControlId			okId
  															, ControlFunction	(noLS closeActiveWindow)
  															, ControlPos		(Center,NoOffset)
  															]
	  							:+:	ButtonControl "Help"	[ ControlFunction	(noLS (showHelp helpfile o closeActiveWindow))]
	  							)
	  							[	WindowOk okId	]
	= snd (openDialog undef about pState)


/*	showHelp opens a SDI process that displays the help information found in the helpfile.
*/
showHelp :: String (PSt .l) -> PSt .l
showHelp helpfile pState
	# (fonts,pState)		= accPIO (accScreenPicture infoFonts) pState
	# ((size,text),pState)	= readInfo Help fonts HelpBegin HelpEnd helpfile pState
	= openProcesses (Process SDI NoState (initHelp fonts size text) [ProcessClose closeProcess]) pState
where
	initHelp :: !Fonts !Size ![InfoLine] (PSt .l) -> PSt .l
	initHelp fonts size text pState
		= snd (openWindow undef window pState)
	where
		window				= Window "Help" NilLS
								[	WindowViewSize		size
								,	WindowLook			True (look fonts.normal text)
								,	WindowHScroll		(stdScrollFunction Horizontal 10)
								,	WindowVScroll		(stdScrollFunction Vertical   10)
								,	WindowClose			(noLS closeProcess)
								,	WindowViewDomain	{zero & corner2={x=size.w,y=size.h}}
								]

look :: Font [InfoLine] SelectState UpdateState *Picture -> *Picture
look font lines _ {updArea} picture
	# picture	= setPenFont font picture
	# picture	= seq (map (\r -> drawInfo font (r.corner1.y-1) (r.corner2.y+40) lines o (unfill r)) updArea) picture
	= picture


//	Try to open a prefered set of fonts to display the help and about information:

infoFonts :: !*Picture -> (!Fonts,!*Picture)
infoFonts picture
	# (normal,    picture) = selectfont [(InfoFontName1,NormalSize1),(InfoFontName2,NormalSize2)] [] picture
	# (large,     picture) = selectfont [(InfoFontName1,LargeSize1 ),(InfoFontName2,LargeSize2 )] [] picture
	# (bold,      picture) = selectfont [(InfoFontName1,NormalSize1),(InfoFontName2,NormalSize2)] [BoldStyle] picture
	# (large_bold,picture) = selectfont [(InfoFontName1,LargeSize1 ),(InfoFontName2,LargeSize2 )] [BoldStyle] picture
	= ({normal=normal,large=large,bold=bold,large_bold=large_bold},picture)
where
	selectfont :: ![(String,Int)] ![FontStyle] !*Picture -> (!Font,!*Picture)
	selectfont [(fontname,size):preffonts] style picture
		# ((found,font),picture)	= openFont {fName=fontname,fStyles=style,fSize=size} picture
		| found						= (font,picture)
		| otherwise					= selectfont preffonts style picture
	selectfont _ style picture
		= openDefaultFont picture


//	Determine the line height and leading of a given font:

getFontHeightAndAscent :: Fonts (PSt .l) -> (((Int,Int),(Int,Int)),PSt .l)
getFontHeightAndAscent fonts pState
	# ((normal,large),pState)	= accPIO (accScreenPicture (getmetrics fonts)) pState
	= (((fontLineHeight normal,normal.fAscent), (fontLineHeight large,large.fAscent)),pState)
where
	getmetrics :: Fonts *Picture -> ((FontMetrics,FontMetrics),*Picture)
	getmetrics {normal,large} picture
		# (normal,picture)	= getFontMetrics normal picture
		# (large, picture)	= getFontMetrics large  picture
		= ((normal,large),picture)


//	Reading and pre-processing of the file containing the about- and help-info. */

readInfo :: Bool Fonts String String String (PSt .l) -> ((Size,[InfoLine]),PSt .l)
readInfo help fonts begin end filename pState
	# (metrics,    pState)	= getFontHeightAndAscent fonts pState
	# (succes,file,pState)	= fopen (applicationpath filename) FReadText pState
	| not succes && help
		= processInfoStrings fonts metrics [errpref+++"could not be found."] pState
	| not succes
		= processInfoStrings fonts metrics ["\\DThis is a Clean program."] pState
	# (found,info,file)		= readInfoFile begin end file
	# (_,pState)			= fclose file pState
	| not found && help
		= processInfoStrings fonts metrics [errpref+++"does not contain help information."] pState
	| not found
		= processInfoStrings fonts metrics ["\\DThis is a Clean program."] pState
	| otherwise
		= processInfoStrings fonts metrics info pState
where
	errpref						= "The help file \'"+++filename+++"\' " 
	
	processInfoStrings :: Fonts ((Int,Int),(Int,Int)) [String] (PSt .l) -> (InfoDef,PSt .l)
	processInfoStrings fonts ((normalHeight,normalAscent),(largeHeight,largeAscent)) lines pState
		# ((size,lines),pState)	= addFontToInfoLines fonts (normalHeight,largeHeight) 0 (Margin+largeAscent) lines pState
		  width					= Margin+size.w+Margin
		# (lines,pState)		= seqList (map (centerInfoLine fonts.normal width) lines) pState
		= (({w=width,h=size.h+Margin-largeAscent},lines),pState)
	where
		addFontToInfoLines :: Fonts Heights Int Int [String] (PSt .l) -> (InfoDef,PSt .l)
		addFontToInfoLines fonts heights maxx maxy [line:rest] pState
			# ((font,wid,hgt,line),pState)	= parseInfoLine fonts heights line pState
			# ((size,rest),        pState)	= addFontToInfoLines fonts heights (max maxx wid) (maxy+hgt) rest pState
			= ((size,[(font,Margin,maxy,line):rest]),pState)
		where
		/*	parseInfoLine determines the font that should be used to draw the line.
			If line == '\{L,b,B,c,C,d,D}'+++line1 then a special font is used, otherwise the default font is used.
			parseInfoLine also calculates the width and height of the line.
		*/
			parseInfoLine :: Fonts Heights String (PSt .l) -> ((InfoFontDef,Int,Int,String),PSt .l)
			parseInfoLine fonts=:{normal,large,bold,large_bold} heights=:(nhgt,lhgt) line pState
				# linelen	= size line
				| linelen<2 || line.[0]<>'\\'
					# (width,pState)	= accPIO (accScreenPicture (getFontStringWidth normal line)) pState
					= ((NoFont False,width,nhgt,line),pState)
				| otherwise
					# (infofont,font,height)= case (line.[1]) of
												'L' -> (InfoFont large      False, large,      lhgt)
												'b' -> (InfoFont bold       False, bold,       nhgt)
												'B' -> (InfoFont large_bold False, large_bold, lhgt)
												'c' -> (NoFont   True,             normal,     nhgt)
												'C' -> (InfoFont large      True , large,      lhgt)
												'd' -> (InfoFont bold       True , bold,       nhgt)
												'D' -> (InfoFont large_bold True , large_bold, lhgt)
												_   -> (NoFont   False,            normal,     nhgt)
					  line					= line%(2,linelen-1)
					# (width,pState)		= accPIO (accScreenPicture (getFontStringWidth font line)) pState
					= ((infofont,width,height,line),pState)
		addFontToInfoLines _ _ maxx maxy _ pState
			= (({w=maxx,h=maxy},[]),pState)
		
		centerInfoLine :: Font Int InfoLine (PSt .l) -> (InfoLine,PSt .l)
		centerInfoLine nft maxx info=:(inft=:(NoFont centered),x,y,line) pState
			| not centered
				= (info,pState)
			| otherwise
				# (width,pState)	= accPIO (accScreenPicture (getFontStringWidth nft line)) pState
				= ((inft,(maxx-width)/2,y,line),pState)
		centerInfoLine nft maxx info=:(inft=:(InfoFont font centered),x,y,line) pState
			| not centered
				= (info,pState)
			| otherwise
				# (width,pState)	= accPIO (accScreenPicture (getFontStringWidth font line)) pState
				= ((inft,(maxx-width)/2,y,line),pState)
	
	readInfoFile :: String String *File -> (Bool,[String],*File)
	readInfoFile begin end file
		# (begin_found,file)	= findInfoBegin begin file
		| not begin_found
			= (False,[],file)
		| otherwise
			# (lines,file)		= readInfoUntil end file
			= (True,lines,file)
	where
		findInfoBegin :: String *File -> (Bool,*File)
		findInfoBegin begin file
			# (endOfFile,file)		= fend file
			| endOfFile				= (False,file)
			# (line,file)			= freadline file
			| isPrefixOf begin line	= (True,file)
			| otherwise				= findInfoBegin begin file
		
		readInfoUntil :: String *File -> ([String],*File)
		readInfoUntil end file
			# (endOfFile,file)		= fend file
			| endOfFile
				= ([],file)
			# (line,file)			= freadline file
			| isPrefixOf end line
				= ([],file)
			| otherwise
				# (lines,file)		= readInfoUntil end file
				= ([stripNewline line:lines],file)
		where
			stripNewline :: String -> String
			stripNewline string
				| string==""			= string
				| string.[last]<>'\n'	= string
				| otherwise				= string%(0,last-1)
			where
				last					= size string-1
		
		isPrefixOf :: String String -> Bool
		isPrefixOf prefix string
			| prefixlen>size string		= False
			| otherwise					= prefix==string%(0,prefixlen-1) 
		where
			prefixlen					= size prefix


/*	The drawing of the about/help info. */

drawInfo :: Font Int Int [InfoLine] *Picture -> *Picture
drawInfo defaultfont top bot [(InfoFont font c,x,y,line):rest] pic
	| y>bot		= pic
	| y<top		= drawInfo defaultfont top bot rest pic
	| otherwise
		# pic	= setPenFont font pic
		# pic	= drawAt {x=x,y=y} line pic
		# pic	= setPenFont defaultfont pic
		= drawInfo defaultfont top bot rest pic
drawInfo defaultfont top bot [(NoFont c,x,y,line):rest] pic
	| y>bot		= pic
	| y<top		= drawInfo defaultfont top bot rest pic
	| otherwise	= drawInfo defaultfont top bot rest (drawAt {x=x,y=y} line pic)
drawInfo _ _ _ _ pic
	= pic
