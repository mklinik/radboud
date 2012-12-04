definition module board


import	StdString
import	StdClass, StdFile
import	types


::	Board				:==	(![[Char]],![[Char]])

initboard				:: Board
getplacedletters		:: !Board -> [Char]

doubleletterpositions	:: [Position]
tripleletterpositions	:: [Position]
doublewordpositions		:: [Position]
triplewordpositions		:: [Position]

grab					:: ![Char] !Int [Int]	-> (![Char],![Char],[Int])
lettervalueat			:: !Char	!Position	-> Int
wordvalueat				::			!Position	-> Int

/***************************************************************************************************************
	getfreehorpositions board letter determines the positions on board that are valid horizontal starting 
	positions for a word starting with letter. 
	This function is useful only for letters that are not available on the letter bar of the computer player.
****************************************************************************************************************/
getfreehorpositions		:: !Board !Char -> [Position]


/***************************************************************************************************************
	getfreeverpositions board letter determines the positions on board that are valid vertical starting 
	positions for a word starting with letter.
	This function is useful only for letters that are not available on the letter bar of the computer player.
****************************************************************************************************************/
getfreeverpositions		:: !Board !Char -> [Position]


/***************************************************************************************************************
	tryaddword board word position direction adds word at position in direction to board. 
	The Board	result is the new board.
	The Boolean	result reports whether the word could be placed.
	The [Char]	result are the letters that have been used.
	The Int		result is the score by placing this word.
	The Words	result are the possibly new formed words.
	
	After tryaddword it should be verified if the new formed words are legal. 
	After tryaddword it also should be verified if a bonus should be added to the score in case all letters have 
	been used.
****************************************************************************************************************/
tryaddword			:: !Board !Word !Position !Direction -> (!Board,!Bool,[Char],Int,[Word])


/***************************************************************************************************************
	newmaximumplacings _ lexicon letterbar _ (Letter l _) _ _ determines all valid words from lexicon that start 
	with l and are member of letterbar.
****************************************************************************************************************/
newmaximumplacings	:: !Board Tree [Char] !(!Int,!Int,!Int,!Int) !Progress !Strength Bool -> Placing


/***************************************************************************************************************
	newmaximumplacing board lexicon letterbar (hor,ver) (Letter l _) _ _ determines all valid words from lexicon 
	that start with l and are not member of letterbar. The positions hor++ver are assumed to be valid free 
	positions on board starting with l.
****************************************************************************************************************/
newmaximumplacing	:: !Board Tree [Char] ([Position],[Position]) !Progress !Strength Bool -> Placing
