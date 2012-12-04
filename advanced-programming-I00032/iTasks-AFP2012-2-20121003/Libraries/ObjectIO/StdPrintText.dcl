definition module StdPrintText

//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdPrintText specifies functions to print text.
//	********************************************************************************

import	StdString
from	StdPictureDef	import :: FontDef
import	StdPrint


/* 	printText1 and printText2 both call printText3, which is the most general function.
 *	printText3 calls the printPagePerPage function from the StdPrint module.
 *	Therefore printText3 has to pass two functions to printPagePerPage: a function that
 *	calculates a initial state, and a function that draws one page and alters the state
 *	accordingly. These two functions are "initState" and "pageTransition".
 *	
 *	Further: pageTransition calls drawLines. drawLines should draw the text of one page.
 *	Depending on the wrapMode, drawLines calls either drawLineWrap or drawLineNoWrap
 *	to draw a single line and then recursively calls itself to draw the remaining lines.
 *	The state contains always the next line to print.
 */


::	WrapMode	:== Int

NoWrap 			:== 0
LeftJustify 	:== 1
RightJustify 	:== 2

class CharStreams cs where
	getChar		:: !*cs -> (!Bool,!Char,!*cs)
				// getChar returns the next character of the stream. The Boolean
				// result indicates whether this operation was successful.
	savePos		:: !*cs -> *cs
				// savePos saves actual position of charstream to enable the 
				// restorePos function to restore it.
	restorePos	:: !*cs -> *cs
	eos			:: !*cs -> (!Bool,!*cs)
				// eos checks for end of stream.

instance CharStreams FileCharStream

::	*FileCharStream

fileToCharStream :: !*File -> *FileCharStream
charStreamToFile :: !*FileCharStream -> *File


printText1 :: !Bool !WrapMode !FontDef !Int
			  !*charStream !PrintSetup  !*env
		-> (!(!*charStream,!PrintSetup),!*env)
		|  CharStreams charStream & PrintEnvironments env
/* 	printText1 doDialog wrapMode font spacesPerTab
			   charStream printSetup env
	prints a CharStream:
	doDialog:
		identical to print (StdPrint)
	wrapMode:
		controls word wrapping in case lines do not fit. NoWrap suppresses wrapping.
		LeftJustify and RightJustify wrap text to the left and right respectively.
	font:
		the text will be printed in this font.
	spacesPerTab:
		the number of spaces a tab symbol represents.
	charStream:
		the charStream to be printed.
	printSetup, env:
		identical to print (StdPrint)
*/

printText2 :: !String !String
			  !Bool !WrapMode !FontDef !Int
			  !*charStream !PrintSetup  !*env
		-> (!(!*charStream,!PrintSetup),!*env)
		|  CharStreams charStream & PrintEnvironments env

/*	printText2 titleStr pageStr
			   doDialog wrapMode fontParams spacesPerTab
			   charStream printSetup env
	prints a charStream with a header on each page.
	titleStr:
		this String will be printed on each page at the left corner of the header
	pageStr:
		this String and the actual page number are printed on the right corner of 
		the header
	The other parameters are identical to printText1.
*/

printText3 :: !Bool !WrapMode !FontDef !Int
			  .(PrintInfo *Picture -> (state, (Int,Int), *Picture))
			   (state Int *Picture -> *Picture)
			  !*charStream !PrintSetup  !*env
		-> (!(!*charStream,!PrintSetup),!*env)
		|  CharStreams charStream & PrintEnvironments env
/*	printText3 doDialog wrapMode font spacesPerTab
			   textRange
			   eachPageDraw
			   charStream printSetup env
	prints a charStream with a header and trailer on each page. 
	textRange:
		this function takes a PrintInfo record and the printer Picture on which the 
		text will be printed. It returns a triple (state,range,picture):
		state:
			a value of arbitrary type that can be used to pass data to the page 
			printing function pages. 
		range:
			a pair (top,bottom), where top<bottom. The printed text will appear 
			within these y-coordinates only, so a header and a trailer can be 
			printed for each page.
	eachPageDraw:
		this function draws the header and/or trailer for the current page. Its 
		arguments are the data produced by textRange, the actual page number, and 
		an initial printer Picture. This function is applied by printText3 before 
		each new page receives its text. 
	The other parameters are identical to printText1.
*/

/*	If a file is openend with FReadData, then all possible newline conventions
	(unix,mac,dos) will be recognized. All these printing functions will replace
	nonprintable characters of the font with ASCII spaces. Exceptions are: newline,
	formfeed and tab. So the ASCII space has to be a printable character in the used
	font. A form feed character will cause a form feed, and it will also end a line.
*/
