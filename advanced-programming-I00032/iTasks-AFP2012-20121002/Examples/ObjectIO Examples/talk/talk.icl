module talk

//	**************************************************************************************************
//
//	This program creates two interactive processes that communicate via message passing.
//	In a future distributed version this program can be used as a graphical talk application.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import	StdEnv, StdIO

//	The message type of talk processes:
::	Message
 =	NewLine String	// Transmit a line of text
 |	Quit			// Request termination

//	Start creates two talk processes A and B that communicate by means of message passing.
Start :: *World -> *World
Start world
	# (a,    world)	= openRId world
	# (b,    world)	= openRId world
	# (talkA,world)	= talk "A" a b world
	# (talkB,world)	= talk "B" b a world
	= startProcesses [talkA,talkB] world

/*	talk name me you
	defines a talk process named name, to which messages can be sent of type Message
	via me, and that sends messages of type Message to a receiver you.
*/
talk :: String (RId Message) (RId Message) *World -> (Process,*World)
talk name me you world
	# (wId,  world)	= openId world
	# (outId,world)	= openId world
	# (inId, world)	= openId world
	= (	Process SDI	Void (initialise wId outId inId) [ProcessClose (quit you)]
	  ,	world
	  )
where
	initialise wId outId inId pst
		# (size,pst)	= controlSize (input wId inId you :+: output outId inId) True Nothing Nothing Nothing pst
		  talkwindow	= Window ("Talk "+++name) (input wId inId you :+: output outId inId)
							[	WindowId		wId
							,	WindowViewSize	size
							]
		  menu			= Menu ("&Talk "+++name)
							(	MenuItem "&Quit" [MenuShortKey 'q',MenuFunction (noLS (quit you))]
							)	[]
		  receiver		= Receiver me (noLS1 (receive wId outId)) []
		# (_,pst)		= openWindow   undef talkwindow pst
		# (_,pst)		= openMenu     undef menu       pst
		# (_,pst)		= openReceiver undef receiver   pst
		= pst		
	
	input wId inId you	= EditControl	"" (PixelWidth (hmm 50.0)) 5
							[	ControlId		inId
							,	ControlKeyboard	inputfilter Able (noLS1 (keyinput wId inId you))
							,	ControlResize	editResize
							,	ControlTip		"Type your message here"
							]
	output outId inId	= EditControl	"" (PixelWidth (hmm 50.0)) 5
							[	ControlId		outId
							,	ControlPos		(Below inId,NoOffset)
							,	ControlSelectState Unable
							,	ControlResize	editResize
							,	ControlTip		"Received messages appear here"
							]

/*	editResize handles the resize of the two input fields. 
*/
editResize :: Size Size Size -> Size
editResize _ _ newWindowSize=:{h}
	= {newWindowSize & h=h/2}

/*	keyinput handles keyboard input in the input EditControl: 
	for every KeyDown keyboard input that has been accepted by the input EditControl, input sends the 
	current content text of the input EditControl to the other talk process with (NewLine text).
*/
inputfilter :: KeyboardState -> Bool
inputfilter keystate
	= getKeyboardStateKeyState keystate<>KeyUp

keyinput :: Id Id (RId Message) KeyboardState (PSt .l) -> PSt .l
keyinput wId inId you _ pst
	# (Just window,pst)	= accPIO (getWindow wId) pst
	  text				= fromJust (snd (getControlText inId window))
	= snd (asyncSend you (NewLine text) pst)
	
/*	The message passing protocol of a talk process.
	On receipt of:
	(1)	NewLine text:set the new text to the output control field of the talk dialog.
	(2) Quit:	     this is always the last message of the other talk process when termination is 
		              requested. The process should terminate itself.
*/
receive :: Id Id Message (PSt .l) -> PSt .l
receive wId outId (NewLine text) pst=:{io}
	= {pst & io=setEditControlCursor outId (size text) (setControlText outId text io)}
receive _ _ Quit pst
	= closeProcess pst

/*	The quit command first sends the Quit message to the other talk process and then quits itself.
*/	
quit :: (RId Message) (PSt .l) -> PSt .l
quit you pst
	= closeProcess (snd (syncSend you Quit pst))
