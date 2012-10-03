definition module showtm

from	StdPicture	import :: Point2, :: Picture
import	tm

MaxX	:== 29900

ShowTape		:: !Tape						!*Picture -> *Picture
ShowTransitions	:: ![Transition] !String		!*Picture -> *Picture
ShowTransition	:: !Int  !Int					!*Picture -> *Picture
ShowTapePart	:: !Tape !Int !Int				!*Picture -> *Picture
ShowNewTape		:: !Comm !Int					!*Picture -> *Picture
ShowNextState	:: !String						!*Picture -> *Picture

ClickedInWindow	:: !Point2 -> (!Int,!Bool,!Bool)
ClickedInTapeWd	:: !Point2 -> (!Int,!Bool)

HiliteTransition:: !Int !Transition				!*Picture -> *Picture
HiliteState		:: !String						!*Picture -> *Picture
HiliteCell		:: !Int !Char					!*Picture -> *Picture
ShowTrans		:: !Int !Transition				!*Picture -> *Picture
EraseTrans		:: !Int							!*Picture -> *Picture
DrawTapeCell	:: !Int !Char					!*Picture -> *Picture
ShowHeadMove	:: !Tape Int Int Int			!*Picture -> *Picture
EraseError		::								!*Picture -> *Picture

SetTuringFont	::								!*Picture -> *Picture

FourCharString	:: !String -> String
FirstChar		:: !String -> Char
