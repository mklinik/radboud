definition module graphics

import	StdPicture, StdPSt
import	board

rbBoardGrey			:== RGB {r=191,g=191,b=191}			// The background colour of the board

displaywidth		:==	2+250+2		//250
displayheight		:==	2+130+2		//130
boardwidth			:==	391
boardheight			:==	391
squarewidth			::	Int
squareheight		::	Int

abs2rel				:: !(!Int,!Int) -> (!Int,!Int)

boardlook			:: !Board Point2 !SelectState !UpdateState	!*Picture	-> *Picture
redrawboard			:: !Id !Board Point2						!(IOSt .l)	-> IOSt .l
drawfocus			:: !Bool !Point2							!*Picture	-> *Picture

letterboxlook		:: ![Char] SelectState UpdateState			!*Picture	-> *Picture
drawletterbox		:: !Id ![Char]								!(IOSt .l)	-> IOSt .l

drawplayer1letters	:: !Id ![Char]								!(IOSt .l)	-> IOSt .l
drawplayer2letters	:: !Id ![Char]								!(IOSt .l)	-> IOSt .l
playerletterslook	:: ![Char] SelectState UpdateState			!*Picture	-> *Picture

drawplayer1score	:: !Id !Int									!(IOSt .l)	-> IOSt .l
drawplayer2score	:: !Id !Int									!(IOSt .l)	-> IOSt .l

drawcommunication	:: !Id ![String]							!(IOSt .l)	-> IOSt .l
displaylook			:: ![String] SelectState !UpdateState		!*Picture	-> *Picture
drawprogress		:: !Id !Player !Progress !Placing			!(IOSt .l)	-> IOSt .l
