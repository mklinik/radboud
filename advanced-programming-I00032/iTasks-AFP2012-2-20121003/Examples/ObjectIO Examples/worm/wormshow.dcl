definition module wormshow

import	StdPicture
import	wormstate

WormBackGroundColour	:==	RGB {r=MaxRGB,g=MaxRGB,b=MaxRGB*3/4}
WormFontSize			:==	12

DrawGame				:: !Level !Food !Points !Worm !Lives					!*Picture -> *Picture
DrawStep				:: !Bool !Food !Food !Points !Segment !Segment !Segment	!*Picture -> *Picture
EraseSegment			:: !Segment												!*Picture -> *Picture
DrawAnimation			:: !Int !Int											!*Picture -> *Picture
