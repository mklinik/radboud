module loco

import iTasks, Random

derive bimap	(,), Maybe
derive class	iTask	Loco, Player, Color, LocoTable, ColorItem

:: Loco		=	{ table 	:: LocoTable
				, players	:: [Player]
				}
:: LocoTable=	{ turn 		:: Int
				, stack		:: Cards
				, chips 	:: Chips
				}
:: Player	=	{ cards		:: Cards
				, chips		:: Chips
				}
:: Color	=	Red
			|	Green
			|	Blue
			|	Black
			|	Purple
:: Cards	:== ColorItem [Int]
:: Chips	:== ColorItem Int
:: ColorItem a	
			= 	{ red 		:: a
				, green 	:: a
				, blue		:: a
				, black 	:: a
				, purple 	:: a
				}


numberOfPlayers	=	4
cardsPerPlayer	=	7
numberOfCards	= 	numberOfPlayers * cardsPerPlayer + 2 
numberOfColors	=	length allColors
allColors		=	[Red, Green, Blue, Black, Purple]
maxCardValue	=	6
cardValues		=	[1..maxCardValue]

initLocoGame :: Int -> Loco
initLocoGame seed = initLoco (mapToColor (shakeCards [1..numberOfCards] [] (genRandInt seed)))
where
	initLoco :: [(Color,Int)] -> Loco
	initLoco cards	
		=	{ table		= initTable
			, players	= [initPlayers (cards%(cardsPerPlayer*(i-1),cardsPerPlayer*i-1)) \\ i <- [1..numberOfPlayers]]
			}
	initTable :: LocoTable
	initTable
		=	{ turn 		= 1
			, stack		= initColorItem (repeat [])
			, chips 	= initColorItem (repeat 6)
			}
	initPlayers :: [(Color,Int)] -> Player
	initPlayers cards
		=	{ cards		= convert cards (initColorItem (repeat []))
			, chips		= initColorItem (repeat 0)
			}
	where
		convert [] deck 			= deck
		convert [(Red,i):cs] deck	= convert cs {deck & red = deck.red ++ [i]}		
		convert [(Green,i):cs] deck	= convert cs {deck & green = deck.red ++ [i]}		
		convert [(Blue,i):cs] deck	= convert cs {deck & blue = deck.red ++ [i]}		
		convert [(Black,i):cs] deck	= convert cs {deck & black = deck.red ++ [i]}		
		convert [(Purple,i):cs] deck	= convert cs {deck & purple = deck.red ++ [i]}		

	mapToColor :: [Int] -> [(Color,Int)]
	mapToColor cards = [(allColors !! detColor card maxCardValue 0,1 + card rem maxCardValue) \\ card <- cards]
	where
		detColor card val n
			| card <= val = n
			= detColor card (val+maxCardValue) (n+1)
	
	shakeCards :: [Int] [Int] [Int] -> [Int]
	shakeCards [] accu _ = accu
	shakeCards cards accu [s:ss]
	# index = (abs s) rem (length cards)
	= shakeCards (removeAt index cards) [cards!!index:accu] ss

	initColorItem as
		= 	{ red 		= as!!0
			, green 	= as!!1
			, blue		= as!!2
			, black 	= as!!3
			, purple 	= as!!4
			}

instance toString Color
where
	toString Red 	= "red"
	toString Green 	= "green"
	toString Blue 	= "blue"
	toString Black 	= "black"
	toString Purple = "purple"

showColVal color label  = toHtmlDisplay [SpanTag [StyleAttr ("color:" +++ (toString color))] [Text (toString label)]]

// -------------------

Start w = startEngine [workflow "test" "test loco" locoWorkflow] w

normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

locoWorkflow 
	= 					readShared sharedCurrentTime
		>>= \time ->	parallel "Loco Player" (initLocoGame (time.hour + time.min + time.sec)) noResult
							[DetachedTask  (normalTask RootUser) noMenu (player i) \\ i <- [1..numberOfPlayers]]
where
	noResult _ _ = Void

	player i s os
		=				monitor "Table" toViewTable (\_ -> False) False s
						||-
						updateSharedInformationA ("Player " <+++ i) (toViewPlayer,fromViewPlayer)  [] s
	where
		toViewTable s  = Display s.table

		toViewPlayer s = Display (s.players!!(i-1))
		fromViewPlayer v s = s

