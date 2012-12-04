module chatClient

//	**************************************************************************************************
//
//	This chat program runs together with the "chatServer" program
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//
//	**************************************************************************************************

import	StdEnv, StdIO, StdTCP

chatPort	:== 2000
remote		:== "Please enter IP adress of chat server"
CR			:== '\xD'					// carriage return

::	*LS									// the ls part of the PSt
	=	{	sndChan		::	TCP_SChannel
		,	nickname	::	String
		}
::	*PState	:==	PSt LS

Start :: *World -> *World
Start world
	= startIO SDI { sndChan=undef, nickname=""} initialize [ProcessWindowSize zero] world

initialize ::  PState -> PState
initialize ps
	# (dialogId, ps)	= accPIO openId ps
	  (nicknameId, ps)	= accPIO openId ps
	  (rmtsiteId, ps)	= accPIO openId ps
	  (buttonId, ps)	= accPIO openId ps

	  // prompt for chat parameters nickname and server address
	  dDef	= Dialog "Enter Chat Parameters" 
				(   TextControl "Type in your nickname and the internet address of the server."
								[]
				 :+:TextControl ("If there is no server running on the specified machine then this program aborts.") [ControlPos (BelowPrev, zero)]
				 :+:EditControl "" (PixelWidth 400) 1 [ControlId nicknameId, ControlPos (Right, zero)]
				 :+:TextControl "Nickname:" [ControlPos (LeftOfPrev, zero)]
				 :+:EditControl remote (PixelWidth 400) 1 [ControlId rmtsiteId, ControlPos (Right, zero)]
				 :+:TextControl "Chat Server:" [ControlPos (LeftOfPrev, zero)]
				 :+:ButtonControl "OK" 
				 				  [ControlId buttonId,
				 				   ControlFunction (noLS1 ok (dialogId, nicknameId, rmtsiteId)),
				 				   ControlPos (Right, zero)]
				) [WindowId dialogId, WindowOk buttonId]
	# ((errReport, _), ps) = openModalDialog Void dDef ps
	| errReport<>NoError
		= abort "can't open modal dialog"
	# (_, ps) = openWindow Void (Window "dummy" NilLS [WindowViewSize {w=100,h=30}]) ps
	= ps
  where
	ok (dialogId, nicknameId, rmtsiteId) ps
		// this function is called, when the promt dialog's ok button is pressed
		// when input parameters are fine, then it tries to connect with the server, which
		// will call "continuation" 
		# (Just dialog,ps)	= accPIO (getWindow dialogId) ps
		  controlTexts		= map (fromJust o snd) (getControlTexts [nicknameId,rmtsiteId] dialog)
		  nickname			= controlTexts !! 0
		  remoteSite		= controlTexts !! 1
		| nickname=="" || remoteSite==""
			=ps
		# ps				= lookupAndConnect remoteSite (continuation nickname remoteSite) ps
		= closeWindow dialogId ps
	continuation :: !String !String (Maybe TCP_DuplexChannel) PState -> PState
	continuation _ remoteSite Nothing ps
		= abort ("ABORT: CAN'T CONNECT with "+++remoteSite)
	continuation nickname _ (Just { sChannel, rChannel }) ps
		// connection with server has been established.
		# (dialogId, ps)= accPIO openId ps
		  (inId, ps)	= accPIO openId ps
		  (outId, ps)	= accPIO openId ps

		// build chat window & menu
		# dDef = Dialog "Chat"
						(	EditControl	"" (PixelWidth (hmm 150.0)) 5
									[	ControlId		inId
									,	ControlKeyboard	inputfilter Able (input dialogId inId)
									]
						:+:	EditControl	"" (PixelWidth (hmm 150.0)) 20
									[	ControlId		outId
									,	ControlPos		(BelowPrev,zero)
									]
						)
						[	WindowId	dialogId
						]
		  (errReport,ps)	= openDialog "" dDef ps
		| errReport<>NoError
			=	abort "chatClient could not open dialog."
		# menu		= Menu "Chat"
							(	MenuItem "Quit" [	MenuShortKey 'q'
												,	MenuFunction (noLS quit)
												]
							)	[]
		# (errReport,ps)	= openMenu Void menu ps
		| errReport<>NoError
			=	abort "chat could not open menu."

		// first send own nickname to server, so that he can broadcast my appearance !
		# (sChannel, ps)	= send (toByteSeq nickname) sChannel ps 

		// open send notifier to eventually flush the send channels buffer 
		# (errReport, sChannel, ps)
		  				= openSendNotifier Void
		  							(SendNotifier sChannel (noLS1 sReceiver) []) ps
		| errReport<>NoError
			=	abort "chat could not open receiver."
		
		// open receiver, which will receive the messages of other chatting people
		# (rcvId, ps)	= accPIO openId ps
		  (errReport, ps)
		  				= openReceiver (dialogId, outId)
		  					(TCP_Receiver rcvId rChannel rReceiver []) ps
		| errReport<>NoError
			=	abort "chat could not open receiver."

		= { ps & ls={ ps.ls & sndChan=sChannel } }
		
lookupAndConnect :: !String ((Maybe TCP_DuplexChannel) -> *(PState -> PState)) PState -> PState
// lookup a host via DNS, connect with that host (if possible) and call the callback function
// (which is the second parameter)
lookupAndConnect inetAddr callback ps
	= lookupIPAddress_async inetAddr lookupCBF ps
  where
    lookupCBF Nothing ps
    	= callback Nothing ps
	lookupCBF (Just inetHost) ps
		= connectTCP_async (inetHost, chatPort) connectCBF ps
	connectCBF Nothing ps
    	= callback Nothing ps
	connectCBF x=:(Just tcpDuplexChan) ps
		= callback x ps

inputfilter :: KeyboardState -> Bool
inputfilter (CharKey char (KeyDown False)  )
	= char==CR
inputfilter (SpecialKey key _ _)
	= key==enterKey
inputfilter _
	= False
	
input dialogId inId _ (l,ps)
	= (l, setControlTextInDialog f dialogId inId ps)
  where
	f text ps=:{ls=ls=:{sndChan}, io}
		#!	(sndChan, io)	= send_NB (toByteSeq (withoutNewlines text)) sndChan io
		= ("", { ps & ls={ls & sndChan=sndChan}, io=io })
	withoutNewlines :: String -> String
	withoutNewlines str
		= toString [ ch \\ ch<-:str | isPrint ch]

sReceiver :: SendEvent PState -> PState
// the function for the receive channel's receiver
sReceiver Sendable ps=:{ls=ls=:{sndChan}, io}
	# (sndChan, io)			= flushBuffer_NB sndChan io
	= { ps & ls={ ls & sndChan=sndChan}, io=io }
sReceiver Disconnected ps
	= abort "ABORT: CONNECTION DISRUPTED "
		
rReceiver :: !(ReceiveMsg ByteSeq) (*(Id,Id),PState) -> (*(Id,Id),PState)
// the function for the send channel's send notifier
// the local state of type (Id,Id) contains the id of the chat window and the id of the
// text control for the output
rReceiver (Received byteSeq) (ls=:(dialogId, outId), ps)
	= (ls, setControlTextInDialog f dialogId outId ps)
  where
	f oldText ps
		= (garbageCollect 1500 (oldText+++(toString byteSeq+++newlineChars)), ps)
	garbageCollect max str	// takes care, that the text in the lower edit control doesn't
		# sz = size str		// get to huge (in case of long chat sessions)
		| sz > max
			= str % (sz-max, sz-1)
		= str

rReceiver EOM _
	= abort "ABORT: CONNECTION DISRUPTED "

quit ps=:{ls=ls=:{sndChan}, io}
	# io	= closeChannel sndChan io
	= closeProcess { ps & ls={ls & sndChan=undef}, io=io }

setControlTextInDialog :: (String PState -> (String, PState)) Id Id PState -> PState
// performs a state transition on the text of an edit control with id <controlId> in window <dialogId>
setControlTextInDialog f dialogId controlId ps
	# (Just wState, ps) = accPIO (getWindow dialogId) ps
	  l					= getControlTexts [controlId] wState
	  oldText			= hd (map (fromJust o snd) l)
	  (newText, ps)		= f oldText ps
	  ps				= appPIO (setControlText controlId newText) ps
	= appPIO (setEditControlCursor controlId (size newText)) ps
	
