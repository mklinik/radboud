definition module language


import	StdList, StdArray, _SystemArray, StdEnum
import	StdIOCommon, StdSystem, StdTime
import	board


/*	This module contains macro's to make the scrabble application language customisable.
*/

//	The filename of the lexicon:

lexiconfilename	:==	"English"+++toString dirseparator+++"English_lexicon"

//	The filename of the help file:

helpfilename	:==	"English"+++toString dirseparator+++"ScrabbleHelp"


//	The set of letters used in the scrabble game.

letterbox	:: [Char]
lettervalue	:: !Char -> Int


//	String conversion functions:

instance toString Player
instance toString Playerkind
instance toString Strength
instance toString (a,b) | toString a & toString b
DirectionToString :: !Direction -> {#Char}


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
addwordstitle			:==	"Add New"
addwordsheading nr
	:==	if (nr==1)	("Word does not occur.",	"Would you like to add it?")
					("Words do not occur.","Would you like to add them?")
addwords_yes			:==	"Yes"
addwords_no				:==	"No"

//	The Save notice:
save_notice_text
	:==	[	"Save added words to lexicon?"
		]
save_notice_yes			:==	"Yes"
save_notice_no			:==	"No"

