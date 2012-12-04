implementation module tm

import	StdArray, StdBool, StdChar, StdInt, StdList, StdString

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


//	Perform one step of a Turing machine.
Step :: !TmState -> TmState
Step {turing}
	= {turing={turing & tape=newtape,state=newstate},transition=transition_nr,command=newcommand}
where
	tape							= turing.tape
	head							= tape.content.[tape.head]
	(transition_nr,transition)		= SelectTransition 0 head turing.state turing.transitions
	(newtape,newstate,newcommand)	= ApplyTransition transition tape
	
	SelectTransition :: !Int !Head !State ![Transition] -> (!TrNr,!Transition)
	SelectTransition n head state [transition=:{start,sigma}:transitions]
		| head==sigma && state==start	= (n,transition)
		| otherwise						= SelectTransition (n+1) head state transitions
	SelectTransition _ _ _ _
		= (0,{start="",sigma='_',end="error",move='_'})
	
	ApplyTransition :: !Transition !Tape -> (!Tape,!State,!Comm)
	ApplyTransition {end,move} tape
		| end=="error"	= (tape,end,ErrorT)
		| move=='L'		= left tape end
						with
							left :: !Tape !State -> (!Tape,!State,!Comm)
							left tape end
								| tape.head==0			= (tape,"error",ErrorL)
								| otherwise				= ({tape & head=tape.head-1},end,MoveL)
		| move=='R'		= right tape end
						with
							right :: !Tape !State -> (!Tape,!State,!Comm)
							right tape=:{content,head} end
								| pos>=size content		= ({content=content+++"#",head=pos},end,MoveR1)
								| otherwise				= ({tape & head=pos},end,MoveR)
							where
								pos						= head+1
		| otherwise		= write tape move end
						with
							write :: !Tape !Char State -> (!Tape,!State,!Comm)
							write tape=:{content,head} move end
								| move=='#'				= ({tape & content=content:=(head,'#')},end,Erase)
								| move==content.[head]	= (tape,end,None)
								| otherwise				= ({tape & content=content:=(head,move)},end,Write move)


//	Functions to inspect and change the tape.
CellContents :: !Int !Tape -> Char
CellContents pos {content,head}
	| pos>=NrOfCells content	= '#'
	| otherwise					= content.[head]

ChangeCellContents :: !Int !Char !Tape -> Tape
ChangeCellContents pos cell tape=:{content,head}
	| pos>=NrOfCells content	= {tape & content=content+++toString cell}
	| otherwise					= {tape & content=content:=(head,cell)}

MoveHead :: !Int !Tape -> Tape
MoveHead pos tape=:{content,head}
	| pos>=length				= {tape & content=ExtendContents content head length}
	| otherwise					= tape
where
	length						= NrOfCells content
	
	ExtendContents :: !String !Int !Int -> String
	ExtendContents content max pos
		| pos>max				= content
		| otherwise				= ExtendContents (content+++"#") max (pos+1)

NrOfCells :: !String -> Int
NrOfCells cont = size cont


//	Functions to inspect and change the transitions.
GetTransition :: Int ![Transition] -> Transition
GetTransition n trs
	| isEmpty trs	= {start="",sigma=' ',end="",move=' '}
	| n==0			= hd trs
	| otherwise		= GetTransition (n-1) (tl trs)

ChangeTransition :: Int Transition ![Transition] -> [Transition]
ChangeTransition n t trs
	| isEmpty trs	= [t]
	| n==0			= [t:tl trs]
	| otherwise		= [hd trs:ChangeTransition (n-1) t (tl trs)]

RemoveTransition :: Int ![Transition] -> [Transition]
RemoveTransition n trs
	| isEmpty trs	= trs
	| n==0			= tl trs
	| otherwise		= [hd trs:RemoveTransition (n-1) (tl trs)]

NrOfTransitions :: ![Transition] -> Int
NrOfTransitions	trs
	= length trs
