definition module bounceDraw


import bounceTypes


drawBarrel	:: !UpdateArea !Scale !Barrel	!*Picture -> *Picture
drawBall	:: !Scale !Point2 !Ball			!*Picture -> *Picture
eraseBall	:: !Scale !Point2 !Ball			!*Picture -> *Picture
