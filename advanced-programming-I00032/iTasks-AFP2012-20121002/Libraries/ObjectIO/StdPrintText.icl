implementation module StdPrintText
 
import StdEnv, StdMaybe, StdPrint, StdPicture
 

emulateScr	:== False
NL13 		:== '\xD'	// carriage return
NL10 		:== '\xA'	// linefeed

/////////////// the CharStreams class /////////////////////////

:: *FileCharStream = { textFile :: !*File , pos :: !Int}

fileToCharStream :: !*File -> *FileCharStream
fileToCharStream file
	= { textFile=file, pos=0 }

charStreamToFile :: !*FileCharStream -> *File
charStreamToFile {textFile}
	= textFile

class CharStreams cs
where
	getChar		:: !*cs -> (!Bool,!Char,!*cs)
	savePos		:: !*cs -> *cs	
	restorePos	:: !*cs -> *cs
	eos			:: !*cs -> (!Bool,!*cs)

instance CharStreams FileCharStream
where
	getChar tf=:{textFile=f} 
		# (ok,ch,f) = freadc f
		= (ok,ch,{tf & textFile=f })
	savePos tf=:{textFile=f} 
		# (pos,f) = fposition f
		= { textFile=f, pos=pos }
	restorePos tf=:{textFile=f, pos}
		# (ok,f) = fseek f pos FSeekSet
		| not ok
			= abort "StdPrintText: fatal error: restorePos not succesful"
		= {tf & textFile=f }
	eos tf=:{textFile=f} 
		# (end,f) = fend f
		= (end,{tf & textFile=f })

/////////////// tabbed Strings /////////////////////////


// Each element of a tabbed String is a substring and an x-coordinate, where the substring has to be printed
// e.g. the string "Hello\tworld" could be as a tabbed string [("Hello",0),("world",10)]

:: TabbedString :== [(!String, !Int)]

//////////////// The State /////////////////////////////

:: *State userInfo charStream = 
  {	file 			:: !charStream,		 
    buffer 			:: !String,		// the next line to print (but never longer than rO.noOfPix, see lastCh)	
    wasWrapped		:: !Bool,		// whether contens of buffer is a line, which was wrapped before
    lastCh			:: !Char,		// contains the last character, that was read via freadLineP or evtlSkipLine
    pageNo			:: !Int,		// actual page number
    noDoneCopies	:: !Int,		// # copies done yet
	firstPageState	:: Maybe FirstPageState,
    rO				:: !ReadOnly userInfo	
    								// these values won't be altered 
  }

// lastChar contains either NL10, NL13, '\f' or ' ' (space). If it is a space, this means, that the line
// was so long, that it couldn't be read to the end by freadLineP. The other values are used to handle
// the different newline conventions on different platforms:
// 	unix: 10 (decimal); dos: 13,10; mac: 13
// For dos formats, freadLineP will only read to the first delimiter (13). If the next line begins with 10,
// this character will be skipped. 

// firstPageState will be initialized, when the first page in the range of pages will be printed.
// After the first copy (= all pages in the range) will have been generated, the file, buffer, 
// wasWrapped, lastCh fields will be reset to the values in this record, so that the next copy is ready
// to be printed.

:: FirstPageState =					
  {	buffer`			:: !String,
	wasWrapped`		:: !Bool,
	lastCh`			:: !Char
  }
  
// ReadOnly fields are not altered

::ReadOnly userInfo =
  {
    printableSet	:: !{#Bool},	// this array represents the set of all printable characters in the font
    topBsLn			:: !Int,		// first baseline of a page
    maxBsLn			:: !Int,		// text won't be drawn below this baseline 
    eachPageDrawFunc:: userInfo Int *Picture -> *Picture,
	wrapMode		:: !WrapMode,	// eachPageDrawFunc, wrapMode and spacePerTab are given as parameters
	spacePerTab		:: !Int,
    tabWidth		:: Real,		// width of tabulator in Pixels, annotating this strict won't work
    noOfPix			:: !Int,		// =inc (fst jobInfo.range) (horizontal width of page)
    maxCharWidth	:: !Int,		// the maximum length of a character in the given font
    font			:: !Font,		// the choosen font
	lineHeight		:: !Int,		// height of one line
    printInfo		:: !PrintInfo,	// ...
	userInfo		:: userInfo
  }

////////////////////////////////////////////////////////////////////////////////////////////////////////////

:: WrapMode :== Int
NoWrap :== 0
LeftJustify :== 1
RightJustify :== 2





printText1 :: !Bool !WrapMode !FontDef !Int !*charStream !PrintSetup !*printEnv
		 ->   (!(!*charStream,!PrintSetup),!*printEnv)
		 | CharStreams charStream & PrintEnvironments printEnv
printText1 doDialog wrapMode fontDef spacesPerTab file printSetup printEnv
  = printText3 	doDialog wrapMode fontDef spacesPerTab textRangeFunc eachPageDrawFunc
				file printSetup printEnv
where
  textRangeFunc {printSetup} picture
	# { page={h} } = getPageDimensions printSetup emulateScr
	= (Nothing, (0,h-1), picture)
  eachPageDrawFunc _ _ picture = picture


printText2 :: !String !String !Bool !WrapMode !FontDef !Int !*charStream !PrintSetup !*printEnv
		 ->  (!(!*charStream,!PrintSetup),!*printEnv)
		 | CharStreams charStream & PrintEnvironments printEnv
 /*
printText2 title page doDialog wrapMode fontDef=:(f1,f2,f3) spacesPerTab file printSetup printEnv
  = printText3 doDialog wrapMode fontDef spacesPerTab textRange eachPageDraw file printSetup printEnv
where
	textRange {printSetup} picture
		#	{page=(width,height)}= getPageDimensions printSetup emulateScr
			(_,font)			= SelectFont f1 (f2++["Bold"]) f3 
			picture				= SetFont font picture
			(metrics=:(fAscent,fDescent,fMaxWidth,fLeading),picture)
								= PictureFontMetrics picture
			lineHeight			= fDescent + fAscent + fLeading
			beamHeight			= 3*lineHeight/2
			ascDesc				= fAscent + fDescent
			baseLine			= fAscent + (beamHeight-ascDesc)/2
		=	( (beamHeight,baseLine,fMaxWidth,font,width,height),
		      (2*lineHeight,height-1),picture)
	eachPageDraw (beamHeight,baseLine,maxwidth,font,width,height) pageNr picture
		#	picture	= SetFont font picture
	    	pageStr = page+++(toString pageNr)
			(pageStrWidth, picture) 
					= PictureStringWidth pageStr picture
		= seq [	SetPenColour	(RGB 0.75 0.75 0.75),
				FillRectangle	((0,0),(width,beamHeight)),
				SetPenColour BlackColour,
				MovePenTo (maxwidth,baseLine),
				DrawString title,
				MovePenTo (width-pageStrWidth-maxwidth,baseLine),
				DrawString pageStr
			  ] 
			  picture
 */
printText2 title page doDialog wrapMode fontDef spacesPerTab file printSetup printEnv
  = printText3 doDialog wrapMode fontDef spacesPerTab textRange eachPageDraw file printSetup printEnv
where
	textRange {printSetup} picture
		#	{page={w=width,h=height}}		= getPageDimensions printSetup emulateScr
			((_,font),picture)	= openFont { fontDef & fStyles=fontDef.fStyles++[BoldStyle]} picture 
			picture				= setPenFont font picture
			(metrics=:{fAscent,fDescent,fMaxWidth},picture)
								= getFontMetrics font picture
			lineHeight			= fontLineHeight metrics
			beamHeight			= 3*lineHeight/2
			ascDesc				= fAscent + fDescent
			baseLine			= fAscent + (beamHeight-ascDesc)/2
		=	( (beamHeight,baseLine,fMaxWidth,font,width,height),
		      (2*lineHeight,height-1),picture)
	eachPageDraw (beamHeight,baseLine,maxwidth,font,width,height) pageNr picture
		#	picture	= setPenFont font picture
	    	pageStr = page+++(toString pageNr)
			(pageStrWidth, picture) 
					= getFontStringWidth font pageStr picture
		= seq [	setPenColour LightGrey,
				fill { zero & corner2={ x=width, y=beamHeight} },
				setPenColour Black,
				drawAt { x=maxwidth, y=baseLine} title,
				drawAt { x=width-pageStrWidth-maxwidth, y=baseLine} pageStr
			  ] 
			  picture




printText3 ::!Bool !WrapMode !FontDef !Int 
			 .(PrintInfo *Picture -> (state, (Int,Int), *Picture))
			 (state Int *Picture -> *Picture)
			 !*charStream
			 !PrintSetup !*printEnv
		 ->  (!(!*charStream,!PrintSetup),!*printEnv)
		 | CharStreams charStream & PrintEnvironments printEnv
printText3 doDialog wrapMode fontDef spacesPerTab textRangeFunc eachPageDrawFunc file printSetup printEnv
	# (alt, printEnv) = printPagePerPage 
						doDialog emulateScr
						( wrapMode, fontDef, spacesPerTab,
						  textRangeFunc, eachPageDrawFunc, file
						)
						initState
						pageTransition
						printSetup
						printEnv
	  result = case alt of
				Cancelled (_,_,_,_,_,file) 							-> (file,printSetup)
				StartedPrinting { file, rO={printInfo={printSetup}}}-> (file,printSetup)
	= (result,printEnv)
						



///////////////  initState //////////////////////////////////////////////////////////////////////

//import StdDebug,dodebug
//import print

initState ::	!*(	.Int,
					FontDef,
					.Int,
					!.(PrintInfo -> .(*Picture -> *(userInfo,(.Int,.Int),*Picture))),
					(userInfo -> Int -> *Picture -> *Picture),
					*charStream
				)
				!.PrintInfo
				*Picture
			 -> *((.Bool,Point2),*(*State userInfo *charStream,*Picture))
			 | CharStreams charStream
initState (wrapMode, fontDef, spacePerTab, textRangeFunc, eachPageDrawFunc, file) 
		  printInfo=:{ printSetup, jobInfo={range=(from`,to)} } picture
  # {page={w=width,h=height}}		= getPageDimensions printSetup emulateScr
   	(userInfo,(top, bot), picture)	= textRangeFunc printInfo picture

  # ((_,font),picture)				= openFont fontDef picture
    ({fAscent, fDescent, fMaxWidth, fLeading}, picture)
									= getFontMetrics font picture
  # lineHeight						= fAscent + fDescent + fLeading
  
  // Check top and bot
    
  | top<0 || bot>height || top>bot 
  		= abort ("\nStdPrintText: wrong top and bottom values returned by function, which is the\n"
  		         +++ "fourth parameter of printText.\n")
  | bot-top+1<lineHeight
  		= abort ("\nStdPrintText: wrong top and bottom values returned by function, which is the\n"
  		         +++ "fourth parameter of printText: range for text is too small.\n")
  
  // Check some assumptions, this module relies on
  | ('\n'<>NL10 && '\n'<>NL13) || width<0
  		= abort "StdPrintText.icl: I have a bug"
  		
  # spacePerTab` = if (spacePerTab<0) 0 spacePerTab
  // ok
  
  #	(space100Width, picture) 
        = getFontStringWidth font (toString (spaces 100)) picture
    (charWidths, picture) 
    	= getFontCharWidths font [toChar i \\ i<-[0..255]] picture
    printableSet 
    	= { { width>0 \\ width<-charWidths } & [toInt '\t']=True } 
    				// '\t' is also "printable"

	// the space character has to be printable with this font
  | not printableSet.[toInt ' ']
		= abort "\nStdPrintText: error: the ASCII space character is not printable with this font.\n" 

  #	noOfPix 
		= (width*995)/1000		// print only in 99.5 % of the page, because some drivers clip a small part 
									// of the page in landscape mode
	
    // fill buffer with first line
	
  	(buffer, lastCh, file) 
		= freadLineP file printableSet ' ' noOfPix spacePerTab
	// eventually rest of first line has to be skipped 
	skipToEoln = wrapMode==NoWrap && (not (isEoln lastCh))
	(lastCh2,(eof,file)) = evtlSkipLine skipToEoln file 

	state 
		= { file=file, buffer=buffer, wasWrapped=False, 
			lastCh=if skipToEoln lastCh2 lastCh,
			pageNo=1, noDoneCopies=0,
			firstPageState=Nothing,
			rO =	{ 	printableSet = printableSet,
			  			topBsLn=top+fLeading+fAscent-1, maxBsLn=bot-fDescent,
			  			eachPageDrawFunc=eachPageDrawFunc,
						wrapMode=wrapMode,
						spacePerTab=spacePerTab`,
		 				tabWidth=(toReal spacePerTab`)*((toReal space100Width)/100.0),
		  				noOfPix=noOfPix, maxCharWidth=fMaxWidth, 
		  				font=font, lineHeight=lineHeight, printInfo=printInfo,
		  				userInfo=userInfo
		  			}
			} 
  | (size buffer==0 && eof) || fMaxWidth>=noOfPix // check, whether the file is empty or font is too big
	  = ((True,zeroOrigin),(state,picture)) 

  | from`==1 && (size buffer>0 || not eof) // optimisation for the case, that it is shure,
	  =	( (False,zeroOrigin), // that there is a page to print
	      (state ,picture)
	    )
	    
  // otherwise this has to be checked out first
  # ((_,_),(state=:{firstPageState}, picture)) = pageTransition (state,picture)
  | isNothing firstPageState 
  		= ((True,zeroOrigin),(state,picture))
  # state = restore_position state			// set current position back to beginning of first page to print
  = ((False,zeroOrigin),(state,picture))

/////////////////// pageTransition ///////////////////////////////////////////////////////////////////



pageTransition ::	*(*State userInfo *charStream,*Picture)
				->	*((.Bool,Point2),*(*State userInfo *charStream,*Picture))
				|	CharStreams charStream
pageTransition (st=:{ pageNo, noDoneCopies, rO } ,picture)
  // it is assumed, that the buffer contains the next line to print
  #	(from`,to) 
  		= rO.printInfo.jobInfo.range
  	
  	// if this is the first page, that will be printed in this job, then save the actual "state",
  	// so this position in the text can be recovered, when the first pages of the following copies
  	// are to be printed (important, e.g. if user chooses to print pages 400-402 twenty times)
  	
  	st 	= evtlUpdateFirstPageState (pageNo==from` && noDoneCopies==0) st 
  	
  	pageInRange 
  		= from`<=pageNo && pageNo<=to
  
  // eventually apply the drawfunction for the header (and trailer)
  
  | pageInRange
	  	#	picture = rO.eachPageDrawFunc rO.userInfo pageNo picture
	  	= continuation st picture from` to pageInRange
  
  =continuation st picture from` to pageInRange
  where
	continuation st picture from` to pageInRange
	  #	picture = seq [setPenColour Black, setPenFont rO.font] picture

	  // draw the text of this page (eventually)
	  
		(eof, st, picture) = drawLines st pageInRange rO.topBsLn picture

		eoCopy = eof || pageNo==to
	  | eoCopy
		  // another finished copy. Quit printing, if this was the last
		  | st.noDoneCopies+1==rO.printInfo.jobInfo.copies = ((True,zeroOrigin), (st,picture))
		  
		  // there are other copies left to do. Set position in file back to first page in range
		  // (stored in firstPageState)
		  # st = restore_position st
		  = ((False,zeroOrigin), ({ st & noDoneCopies=inc st.noDoneCopies },picture))

	  // this was not the last page of the copy

	  | pageInRange
		  = ((False,zeroOrigin), ({ st & pageNo=inc st.pageNo }, picture))
	  = pageTransition ({ st & pageNo=inc st.pageNo }, picture)



evtlUpdateFirstPageState :: .Bool *(State a *b) -> *State a *b
							| CharStreams b
evtlUpdateFirstPageState False st = st
evtlUpdateFirstPageState True st=:{ file, buffer, wasWrapped, lastCh }
	# file = savePos file
	= { st & file=file, 
	         firstPageState=Just { buffer`=buffer, wasWrapped`=wasWrapped, lastCh`=lastCh } }

restore_position :: *(State a *b) -> *State a *b
					| CharStreams b
restore_position st
	# { buffer`, wasWrapped`, lastCh`} = fromJust st.firstPageState
	  file = restorePos st.file	
	= { st & file=file, buffer=buffer`, wasWrapped=wasWrapped`, lastCh=lastCh`,
			 pageNo=fst st.rO.printInfo.jobInfo.range  }
		  	


///////////////////////  drawLines ///////////////////////////////////////



drawLines :: *(State a *b) Bool Int *Picture 
		-> 	*(.Bool,*State a *b,*Picture)
		| 	CharStreams b;
drawLines st=:{ lastCh, rO } reallyDraw topBsLn picture
  // it is assumed, that the buffer contains the next line to print
  // further on, this function will return a state, in which the buffer contains the next line to print 
  // drawlines draws the further text into one page between the y-coordinates top an state.rO.bot. It
  // returns a state, that reflects
  // this change. It further returns, whether the last line of the file was printed.
  
  // print next line, depending on wrapMode
  
  | rO.wrapMode==NoWrap
		= continuation (drawLineNoWrap st reallyDraw topBsLn picture)
		= continuation (drawLineWrap st reallyDraw topBsLn picture)
  where
	continuation (st=:{ file, buffer }, picture)
	
		// check for end of file
		# (eof, file) = eos file
		  st = { st & file=file }
		| size buffer==0 && eof
			= (True, st, picture)

		// check, whether the drawn line ended with a form feed
		| lastCh=='\f'
			= (False, st, picture) 
			
		// check, whether the page is full
		# nextBsLn = topBsLn+rO.lineHeight
  		| nextBsLn>rO.maxBsLn
			= (False, st, picture)
		
  		// there are still lines to be printed on this page, so do it
		= drawLines st reallyDraw nextBsLn picture




///////////////////////  drawLineWrap ///////////////////////////////////////



drawLineWrap :: *(State a *b) .Bool .Int *Picture 
			-> *(*State a *b,*Picture) 
			| CharStreams b
drawLineWrap st=:{ buffer, wasWrapped, rO } reallyDraw y picture
  // it is assumed, that the buffer contains the next line to print
  // further on, this function will return a state, in which the buffer contains the next line to print 
  // This function prints the current line, if the current page is in the print range (indicated by reallyDraw)

  // get the number of characters of buffer, which maximally will fit into the current line
  
  # (fittingLength, picture) 
    	= splitString 	buffer rO.noOfPix rO.maxCharWidth 
    					rO.tabWidth 
						rO.font
    					picture

  // print this substring, if the current page is in the print range

  | reallyDraw
	  	
	  	// split (buffer % (0,fittingLength-1) into parts, which are delimited by tabs 
	  	# ((width,tString), picture) = splitInTabs buffer fittingLength rO.tabWidth 
	  												rO.font picture

		  offset = if (wasWrapped && rO.wrapMode==RightJustify) (rO.noOfPix-width-1) 0

		// draw !
		  picture = drawTabbedString tString offset y picture 
		= continuation fittingLength y picture

  // don't draw anything
  = continuation fittingLength y picture

where

  continuation fittingLength y picture
	// fill the buffer with the next line to draw
	| fittingLength<size st.buffer 
		// the line was wrapped, so some part of the buffer will appear in the next line
		# smallerBuffer = buffer % (fittingLength,(size buffer)-1)
		| isEoln st.lastCh
			= ( { st & buffer=smallerBuffer, wasWrapped=True }, picture)
		# (newContens, lastCh, file) = freadLineP st.file rO.printableSet st.lastCh 
													(rO.noOfPix-(size smallerBuffer)) rO.spacePerTab
		= ( { st & file=file, buffer=smallerBuffer+++newContens, wasWrapped=True, lastCh=lastCh }, picture)
						
	// the line was not wrapped, the whole buffer contens was drawn. get new line
	# (newBuffer, lastCh, file) = freadLineP st.file rO.printableSet st.lastCh rO.noOfPix rO.spacePerTab
	= ( { st & file=file, buffer=newBuffer, wasWrapped=False, lastCh=lastCh }, picture)



///////////////////////  drawLineNoWrap  ///////////////////////////////////////



drawLineNoWrap :: *(State a *b) .Bool .Int *Picture
				-> *(*State a *b,*Picture)
				| CharStreams b
drawLineNoWrap st=:{ buffer, rO } reallyDraw y picture
  // it is assumed, that the buffer contains the next line to print
  // further on, this function will return a state, in which the buffer contains the next line to print 
  // This function prints the current line, if the current page is in the print range (indicated by reallyDraw)

  | reallyDraw
	  	
	  	// split buffer into parts, which are delimited by tabs 
	  	# ((_,tString), picture) = splitInTabs buffer (size buffer) rO.tabWidth rO.font picture
		// draw !
		  picture = drawTabbedString tString 0 y picture 
		= continuation y picture

  // don't draw anything
  = continuation y picture

where

  continuation y picture
	// fill the buffer with the next line to draw
	# (newBuffer, lastCh, file) = freadLineP st.file rO.printableSet st.lastCh rO.noOfPix rO.spacePerTab
	  skipToEoln = not (isEoln lastCh)
	  (lastCh2,(_,file)) = evtlSkipLine skipToEoln file
	= ({ st & file=file, buffer=newBuffer, wasWrapped=False, lastCh=if skipToEoln lastCh2 lastCh }, picture)


evtlSkipLine :: !Bool !*charStream -> (!Char, (!Bool, !*charStream)) | CharStreams charStream
// iff first parameter True, then skip characters until next newline, give back last character & eof
evtlSkipLine False file = (' ',eos file)
evtlSkipLine _ file
	= skipLine file
	where
		skipLine file
			# (ok, char, file) = getChar file
			| not ok
				= (NL13, (True,file))
			| isEoln char
				= (char, (False,file))
			= skipLine file

			
////////////// freadLineP ///////////////////////////////////////////////////

		
freadLineP :: !*charStream !{#Bool} !Char !Int !Int -> (!String, !Char, !*charStream)
			| CharStreams charStream
freadLineP file printableSet lastCh max spacePerTab
// reads the next line from the file, but not more than max characters. A line is at it's end,
// when NL10,NL13, or '\f' occurs. The returned String won't contain this character, it will be
// returned in the Char field instead. If the
// line is longer than max, then the returned String will contain only max characters, and the
// returned Char will be a space.
// This function skips trailing spaces of a line, and replaces some leading spaces with the right
// number of tabs (because the Mac has difficulties in printing spaces with big fonts)
	# (ok,ch,file) = getChar file
	| not ok
		= ("",NL13,file)
	| lastCh==NL13 && ch==NL10
		= freadLineP file printableSet ' ' max spacePerTab	// this is DOS format. Skip first character
	| isEoln ch
		= ("",ch,file)	// the line is empty 
	# (charList,lastCh2,file) = readA file (dec max) []
	  charList2 = if (isEoln lastCh2) 
	  						   (dropWhile (\ch -> ch==' ' || ch=='\t') charList) // skip trailing spaces
	  						   charList
	  charList3	= replaceLeadingSpaces [ch:reverse charList2]
	= ({ ch \\ ch<-charList3 }, lastCh2, file)
  where
	readA file 0 akku = (akku,' ',file)
	readA file i akku
		# (ok,ch,file) = getChar file
		| not ok 				// an eof will terminate the line
			= (akku,NL13,file)
		| isEoln ch
			= (akku,ch,file)
		= readA file (dec i) [changeUnprintable ch:akku]
	changeUnprintable ch
	 	| printableSet.[toInt ch] = ch
	 	= ' '
	replaceLeadingSpaces l
		#!	(leadingChars, rest)	= span (\ch-> ch==' ') l
			nrLeadingChars			= length leadingChars
		= (repeatn (nrLeadingChars/spacePerTab) '\t')++(repeatn (nrLeadingChars rem spacePerTab) ' ')++rest
		
//////////// splitString /////////////////////////////////////////////////////


 /*
splitString :: !String !Int !Int !Real !*Picture -> (!Int, !*Picture)
splitString str noOfPix maxCharWidth tabWidth pict
// returns the maximum number of characters of str that fit into the picture of width noOfPix
// contains optimisation: checks, whether the line is that short,
// that it doesn't depend on the characters, whether it fits into the picture.
// str` is a string made out of str by replacing all tabs width the maximal number of spaces 
  #	noOfTabs = length [ tab \\ tab <-: str | tab=='\t'] 
	strWidth = ((size str) - noOfTabs)*maxCharWidth  + noOfTabs*(ceil tabWidth)
  | strWidth<=noOfPix = (size str, pict)
  # ((width,_), pict) = splitInTabs str (size str) tabWidth pict
  | width<=noOfPix = (size str, pict)
  #	fitGuaranteed = maxList [0, (noOfPix - noOfTabs*(ceil tabWidth))/maxCharWidth]	
    				// a save estimation: this number of characters will savely fit into the range
  = splitLoop str noOfPix fitGuaranteed (size str) tabWidth pict
    	
splitLoop :: !String !Int !Int !Int !Real !*Picture -> (!Int, !*Picture)
splitLoop str noOfPix fit doesntFit tabWidth pict
// (str % (0,fit-1)) fits into the picture, but (str % (0,doesntFit-1)) doesn't.
// returns the maximum number of characters of str that fit into the picture of width noOfPix.
  | (inc fit)==doesntFit = (fit, pict)
  # middle = (fit+doesntFit)/2
	((width,_), pict) = splitInTabs str middle tabWidth pict
  | width>noOfPix = splitLoop str noOfPix fit    middle    tabWidth pict
				  = splitLoop str noOfPix middle doesntFit tabWidth pict
 */
splitString :: !String !Int !Int  !Real !Font !*Picture -> (!Int, !*Picture)
splitString str noOfPix maxCharWidth tabWidth font pict
// returns the maximum number of characters of str that fit into the picture of width noOfPix
// contains optimisation: checks, whether the line is that short,
// that it doesn't depend on the characters, whether it fits into the picture.
// str` is a string made out of str by replacing all tabs width the maximal number of spaces 
  #	noOfTabs = length [ tab \\ tab <-: str | tab=='\t'] 
	strWidth = ((size str) - noOfTabs)*maxCharWidth  + noOfTabs*(ceil tabWidth)
  | strWidth<=noOfPix = (size str, pict)
  # ((width,_), pict) = splitInTabs str (size str) tabWidth font pict
  | width<=noOfPix = (size str, pict)
  #	fitGuaranteed = maxList [0, (noOfPix - noOfTabs*(ceil tabWidth))/maxCharWidth]	
    				// a save estimation: this number of characters will savely fit into the range
  = splitLoop str noOfPix fitGuaranteed (size str) tabWidth font pict
    	
splitLoop :: !String !Int !Int !Int !Real !Font !*Picture -> (!Int, !*Picture)
splitLoop str noOfPix fit doesntFit tabWidth font pict
// (str % (0,fit-1)) fits into the picture, but (str % (0,doesntFit-1)) doesn't.
// returns the maximum number of characters of str that fit into the picture of width noOfPix.
  | (inc fit)==doesntFit = (fit, pict)
  # middle = (fit+doesntFit)/2
	((width,_), pict) = splitInTabs str middle tabWidth font pict
  | width>noOfPix = splitLoop str noOfPix fit    middle    tabWidth font pict
				  = splitLoop str noOfPix middle doesntFit tabWidth font pict


////////////////// tabbed Strings ////////////////////////////////////////////////

  
splitInTabs :: !String !Int !Real !Font !*Picture -> ( (Int, TabbedString), !*Picture)
splitInTabs str length tabWidth font picture
// returns the width of the substring (str % (0,length-1)) and that substring in tabbed form
	# strList  = parts str 0 (length-1) []	// contains at least 1 element
	  (widths, picture) = getFontStringWidths font strList picture
	  (sumWidth,beginPos) = getBeginningPos widths 0 []
	=( (sumWidth, zip (strList, beginPos)), picture)
  where
  	parts str min max akku
  		# nextTabPos = searchTab str min max
  		| nextTabPos== max+1 = reverse [ str % (min,max) : akku]
  		= parts str (inc nextTabPos) max [ str % (min,nextTabPos-1) :akku]
  	searchTab str pos max 
  		| pos == max+1 = pos
  		| str.[pos]=='\t' = pos
  		= searchTab str (inc pos) max
	getBeginningPos :: ![Int] !Int ![Int] -> (!Int, ![Int])
	getBeginningPos [width] sum akku= (width+sum,reverse [sum:akku])
	getBeginningPos [width:rest] sum akku
		# nextTabNr = ceil (  (	  (toReal (sum+(if (width<=0) 1 width)))
							 	/ tabWidth)
							+ roundErrorCorrection)
		  nextTabPos = toInt ((toReal nextTabNr)*tabWidth)
		= getBeginningPos rest nextTabPos [sum:akku]
		where
			roundErrorCorrection = 0.05
			// this value is introduced, because otherwise the CleanIDE Editor and this printing
			// routine would generate different output. The following could happen without this
			// correction: width=499, and tabWidth=100 -> the inserted tab would be one pixel
			// wide -> it looks, as if it was "forgotten" to print this tab.   

drawTabbedString :: !TabbedString !Int !Int !*Picture -> *Picture
drawTabbedString [] offset y picture = picture
drawTabbedString [(subStr,x):rest] offset y picture
	= drawTabbedString rest offset y (drawAt { x=x+offset,y=y } subStr picture)


////////////////// misc //////////////////////////////////////////

ceil :: !Real -> Int
ceil r					// = ~ (entier (~r)) doesn't work, e.g.: ceil 3.0 = 4 !!! 
	# round = toInt r
	  diff = r - (toReal round)
	| diff > 0.0 = inc round
	= round
	  
// a form feed counts as an ended line

isEoln ch
  :== ch==NL10 || ch==NL13 || ch=='\f'
  
zeroOrigin :== { x=0, y=0 }

