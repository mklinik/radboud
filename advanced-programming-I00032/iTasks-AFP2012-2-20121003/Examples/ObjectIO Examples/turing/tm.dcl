definition module tm

import StdString

::	TmState
	=	{	turing		:: !Turing
		,	transition	:: !TrNr
		,	command		:: !Comm
		}
::	Turing
	=	{	transitions	:: ![Transition]
		,	tape		:: !Tape
		,	state		:: !State
		}
::	Transition
	=	{	start		:: !State
		,	sigma		:: !Head
		,	end			:: !State
		,	move		:: !Char
		}
::	Tape
	=	{	content		:: !String
		,	head		:: !Int
		}
::	State	:== String
::	Head	:== Char
::	TrNr	:== Int
::	Comm	=	Erase | None | MoveR1 | MoveR | MoveL | Halt | ErrorL | ErrorT
			|	Write Char

Step				:: !TmState							-> TmState
CellContents		:: !Int !Tape						-> Char
ChangeCellContents	:: !Int !Char !Tape					-> Tape
MoveHead			:: !Int !Tape						-> Tape
GetTransition		:: Int				![Transition]	-> Transition
ChangeTransition	:: Int Transition	![Transition]	-> [Transition]
RemoveTransition	:: Int				![Transition]	-> [Transition]
NrOfTransitions		::					![Transition]	-> Int
NrOfCells			:: !String							-> Int
