implementation module language


import	StdBool, StdList, StdArray, _SystemArray, StdEnum
import	StdIOCommon, StdSystem, StdTime
import	types


/*	This module contains macro's to make the scrabble application language customisable.
*/

//	The filename of the lexicon:

lexiconfilename	:==	"English"+++toString dirseparator+++"English_lexicon"

//	The filename of the help file:

helpfilename	:==	"English"+++toString dirseparator+++"ScrabbleHelp"


//	The set of letters used in the scrabble game.

letterbox :: [Char]
letterbox 
	=:	repeatn 7  'a' ++ repeatn 2  'b' ++ repeatn 1 'c' ++ repeatn 4 'd' ++
		repeatn 10 'e' ++ repeatn 2  'f' ++ repeatn 3 'g' ++ repeatn 3 'h' ++
		repeatn 5  'i' ++ repeatn 1  'j' ++ repeatn 1 'k' ++ repeatn 4 'l' ++
		repeatn 2  'm' ++ repeatn 4  'n' ++ repeatn 7 'o' ++ repeatn 2 'p' ++
		repeatn 1  'q' ++ repeatn 4  'r' ++ repeatn 5 's' ++ repeatn 5 't' ++
		repeatn 3  'u' ++ repeatn 1  'v' ++ repeatn 2 'w' ++ repeatn 1 'x' ++
		repeatn 2  'y' ++ repeatn 1  'z'

lettervalue :: !Char -> Int
lettervalue l
|	isMember l ['a','e','i','l','n','o','r','s','t','u']	= 1
|	l=='d' || l=='g'										= 2
|	l=='b' || l=='m' || l=='p' || l=='w'					= 3
|	l=='c' || l=='f' || l=='h' || l=='y'					= 4
|	l=='v'													= 5
|	l=='j' || l=='k'										= 7
|	l=='x'													= 8
|	l=='q' || l=='z'										= 10
|	otherwise												= 0		// for the blank letters


//	String conversion functions:

instance toString Player where
	toString :: !Player -> {#Char}
	toString Player1			 = "Player 1"
	toString Player2			 = "Player 2"
instance toString Playerkind where
	toString :: !Playerkind -> {#Char}
	toString Computer			= "Computer"
	toString Person				= "Person"
instance toString Strength where
	toString :: !Strength -> {#Char}
	toString Maximum			= "Maximum"
	toString First				= "First"
	toString MediumStrength		= "Medium"
	toString EasyStrength		= "Easy"
	toString VeryEasyStrength	= "Very Easy"
instance toString (a,b) | toString a & toString b where
	toString :: !(a,b) -> {#Char} | toString a & toString b
	toString (a,b) = "("+++toString a+++","+++toString b+++")"
DirectionToString :: !Direction -> {#Char}
DirectionToString Horizontal	= "Across"
DirectionToString Vertical		= "Down"


//	Text used to communicate with the user in the display control.

exchanges_letters
	:==	" exchanges letters."
placement_error word pos
	:==	[	" by placing the word '"+++word+++"'"
		,	" at "+++toString pos
		,	" it will not adjoin any present word."
		]
no_letters_used_error
	:==	[	"you should letters of your own."	]
anonymous_placement_error
	:==	[	"word can not be place at this position." ]
missing_letters_error cs
	:==	[	"you can not form this word because you do not have the letter(s): "
		,	"   "+++{c\\c<-cs}+++"."
		]
nr_new_words_placed nr words
	:==	if (nr==0)
			["No new words placed.",wait (2*ticksPerSecond) ""]
	   (if (nr==1)
	   		["New word placed:",hd words,wait (2*ticksPerSecond) ""]
			[toString nr+++" New words placed" : words++[wait (2*ticksPerSecond) ""]]
	   )
has_won				:==	" has won."
is_a_draw			:==	"It is a draw."
is_move				:==	" is playing."

determines_new_word	:==	" determines a new word"
determined_new_word	:==	" has determined a new word."
found_upto_now		:==	"found word up to now:"
score_upto_now		:==	"score up to now:"
at_pos				:== "at:"


//	Text used in the GUI definition.

//	The Scrabble menu:
scrabblemenutitle		:==	"Scrabble"
playersmenutitle		:==	"Players"
newgametitle			:==	"New"
quitgametitle			:==	"Quit"

//	The Strength menu:
strengthmenutitle		:==	"Strength"

//	The Scrabble dialog:
scrabbledialogtitle		:==	"Scrabble"
scrabbledialogscore		:==	"Score"
scrabbledialogword		:==	"Player word"
scrabbledialogdirection	:==	"Direction"
scrabbledialogplaceword	:==	"Place Word"
scrabbledialoginittext lexicon
	:==	[	"Number of read words: "
		,	toString (sizeDictionary lexicon)
		,	wait (2*ticksPerSecond) ""
		]

//	The Add Words dialog:
addwordsheading nr
	:==	if (nr==1)	("Word does not occur.","Would you like to add it?")
					("Words do not occur.", "Would you like to add them?")
addwordstitle			:==	"Add New"
addwords_yes			:==	"Yes"
addwords_no				:==	"No"

//	The Save notice:
save_notice_text
	:==	[	"Save added words to lexicon?"
		]
save_notice_yes			:==	"Yes"
save_notice_no			:==	"No"
