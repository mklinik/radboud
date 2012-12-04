definition module Life

import	StdPicture

::	Generation
::	LifeCell
::	CellSize	:==	Int
::	ClickPoint	:== Point2

makeGeneration	:: Generation
makeLifeCell	:: !ClickPoint !CellSize -> LifeCell

drawCells		:: !(LifeCell -> *Picture -> *Picture) !Generation !*Picture -> *Picture
drawCell		:: !CellSize !LifeCell !*Picture -> *Picture
eraseCell		:: !CellSize !LifeCell !*Picture -> *Picture

insertCell		:: !LifeCell !Generation -> Generation
removeCell		:: !LifeCell !Generation -> Generation

lifeGame		:: !Generation -> (!Generation,!Generation)
