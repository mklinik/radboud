definition module language


import	StdList, StdArray, /*_SystemArray,*/ StdEnum
import	StdIOCommon, StdSystem, StdTime
import	board


/*	This module contains macro's to make the scrabble application language customisable.
*/

//	The filename of the lexicon:

lexiconfilename	:==	"Nederlands"+++toString dirseparator+++"Nederlands_lexicon"

//	The filename of the help file:

helpfilename	:==	"Nederlands"+++toString dirseparator+++"ScrabbleHelp"


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
	:==	" ruilt letters in."
placement_error word pos
	:==	[	" door het woord '"+++word+++"'"
		,	" op "+++toString pos+++" te plaatsen"
		,	" grenst het nergens aan de liggende letters."
		]
no_letters_used_error
	:==	[	"geen eigen letters gebruikt."	]
anonymous_placement_error
	:==	[	"woord kan daar niet geplaatst worden."	]
missing_letters_error cs
	:==	[	"je kunt het woord niet vormen want je mist de letter(s): "
		,	"   "+++{c\\c<-cs}+++"."
		]
nr_new_words_placed nr words
	:==	if (nr==0)
			["Geen nieuwe woorden geplaatst.",wait (2*ticksPerSecond) ""]
	   (if (nr==1)
	   		["Nieuw woord geplaatst: ",hd words,wait (2*ticksPerSecond) ""]
			[toString nr+++" Nieuwe woorden geplaatst: " : words++[wait (2*ticksPerSecond) ""]]
	   )
has_won				:==	" heeft gewonnen."
is_a_draw			:==	"Gelijk spel."
is_move				:==	" is aan zet."

determines_new_word	:==	" bepaalt een nieuw woord"
determined_new_word	:==	" heeft een nieuw woord bepaald."
found_upto_now		:==	"gevonden woord tot nu toe:"
score_upto_now		:==	"score tot nu toe:"
at_pos				:== "op:"


//	Text used in the GUI definition.

//	The Scrabble menu:
scrabblemenutitle		:==	"Scrabble"
playersmenutitle		:==	"Spelers"
newgametitle			:==	"Nieuw"
quitgametitle			:==	"Quit"

//	The Strength menu:
strengthmenutitle		:==	"Speelsterkte"

//	The Scrabble dialog:
scrabbledialogtitle		:==	"Scrabble"
scrabbledialogscore		:==	"Score"
scrabbledialogword		:==	"Speler woord"
scrabbledialogdirection	:==	"Richtingkeuze"
scrabbledialogplaceword	:==	"Leg woord"
scrabbledialoginittext lexicon
	:==	[	"Aantal ingelezen woorden: "
		,	toString (sizeDictionary lexicon)
		,	wait (2*ticksPerSecond) ""
		]

//	The Add Words dialog:
addwordsheading nr
	:==	if (nr==1)	("Woord komt niet voor.",	"Wil je het woord toevoegen?")
					("Woorden komen niet voor.","Wil je de woorden toevoegen?")
addwordstitle			:==	"Toevoegen"
addwords_yes			:==	"Ja"
addwords_no				:==	"Nee"

//	The Save notice:
save_notice_text
	:==	[	"Bewaar toegevoegde woorden"
		,	"van woordenboek?"
		]
save_notice_yes			:==	"Ja"
save_notice_no			:==	"Nee"
