implementation module language


import	StdBool, StdList, StdArray, /*_SystemArray,*/ StdEnum
import	StdIOCommon, StdSystem, StdTime
import	types


/*	This module contains macro's to make the scrabble application language customisable.
*/

//	The filename of the lexicon:

lexiconfilename	:==	"Nederlands"+++toString dirseparator+++"Nederlands_lexicon"

//	The filename of the help file:

helpfilename	:==	"Nederlands"+++toString dirseparator+++"ScrabbleHelp"


letterbox :: [Char]
letterbox 
	=:	repeatn 6  'a' ++ repeatn 2  'b' ++ repeatn 2 'c' ++ repeatn 5 'd' ++
		repeatn 18 'e' ++ repeatn 1  'f' ++ repeatn 3 'g' ++ repeatn 2 'h' ++
		repeatn 4  'i' ++ repeatn 2  'j' ++ repeatn 3 'k' ++ repeatn 3 'l' ++
		repeatn 3  'm' ++ repeatn 10 'n' ++ repeatn 6 'o' ++ repeatn 2 'p' ++
		repeatn 1  'q' ++ repeatn 5  'r' ++ repeatn 4 's' ++ repeatn 5 't' ++
		repeatn 3  'u' ++ repeatn 2  'v' ++ repeatn 2 'w' ++ repeatn 1 'x' ++
		repeatn 1  'y' ++ repeatn 2  'z'

lettervalue :: !Char -> Int
lettervalue l
	| l=='a' || l=='e' || l=='i' || l=='n' || l=='o'			= 1
	| l=='d' || l=='r' || l=='s' || l=='t'						= 2
	| l=='b' || l=='g' || l=='k' || l=='l' || l=='m' || l=='p'	= 3
	| l=='f' || l=='h' || l=='j' || l=='u' || l=='v' || l=='z'	= 4
	| l=='c' || l=='w'											= 5
	| l=='x' || l=='y'											= 8
	| l=='q'													= 10
	| otherwise													= 0		// for the blank letters


//	String conversion functions:

instance toString Player where
	toString :: !Player -> {#Char}
	toString Player1			 = "Speler 1"
	toString Player2			 = "Speler 2"
instance toString Playerkind where
	toString :: !Playerkind -> {#Char}
	toString Computer			= "Computer"
	toString Person				= "Persoon"
instance toString Strength where
	toString :: !Strength -> {#Char}
	toString Maximum			= "Maximaal"
	toString First				= "Eerste Beste"
	toString MediumStrength		= "Gemiddeld"
	toString EasyStrength		= "Makkelijk"
	toString VeryEasyStrength	= "Heel Makkelijk"
instance toString (a,b) | toString a & toString b where
	toString :: !(a,b) -> {#Char} | toString a & toString b
	toString (a,b) = "("+++toString a+++","+++toString b+++")"
DirectionToString :: !Direction -> {#Char}
DirectionToString Horizontal	= "Horizontaal"
DirectionToString Vertical		= "Vertikaal"


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
