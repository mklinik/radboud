module echoServer

//  ********************************************************************************
//  Clean tutorial example program.
//  
//  This program demonstrates the usage of functions for event driven TCP.
//	It listens on port 7, accepts a connection and echoes the input
//
//  ********************************************************************************

import  StdEnv, StdTCP, StdIO

echoPort	:== 7

::	*LS							// the ls part of the PSt
	=	{	chan	::	TCP_DuplexChannel
		,	eom		::	Bool	//	The Boolean value stores, whether EOM happened on the receive channel.
		}
::	*PState	:==	PSt LS

Start world
	# (ok, mbListener, world)	= openTCP_Listener echoPort world
	| not ok
		= abort ("chatServer: can't listen on port "+++toString echoPort)
	#!(console, world)		= stdio world
	  console	= fwrites "This server program waits until a client program " console
	  console	= fwrites "tries to connect.\n" console
	  (_,world)	= fclose console world
	  ((_,duplexChan), listener, world)
	  							= receive (fromJust mbListener) world
	  world						= closeRChannel listener world
	= startIO NDI {chan=duplexChan,eom=False} initialise [] world

////////////////////////////////////////////////////////////
////	initialise - the function to initialise the PSt ////
////////////////////////////////////////////////////////////

::	ReceiveSt
	=	{ rcvId :: Id
		}

initialise	:: PState -> PState
initialise pSt=:{ ls=ls=:{chan={rChannel,sChannel}}, io }
	# (tcpRcvId, io)	= openId io
	  pSt 				= { pSt & ls = { ls & chan = { rChannel=undef, sChannel=undef } }, io=io }

  // open a receiver for the receive channel

	  (errReport1, pSt)	= openReceiver {rcvId=tcpRcvId}
	  								  (TCP_Receiver tcpRcvId rChannel rcvFun []) pSt

  // open a receiver for the send channel

	  (errReport2, sChannel, pSt)
						= openSendNotifier {rcvId=tcpRcvId} 
										   (SendNotifier sChannel sndFun []) pSt
	| errReport1<>NoError || errReport2<>NoError
		= abort "error: can't open receiver"
	= { pSt & ls = { ls & chan = { rChannel=undef, sChannel=sChannel } } }

/////////////////////////////////////////////////////////////////////////////
////	rcvFun - the callback function for the receive channels receiver ////
/////////////////////////////////////////////////////////////////////////////

rcvFun :: (ReceiveMsg ByteSeq) (*ReceiveSt,PState) -> (*ReceiveSt,PState)
rcvFun (Received byteSeq) (rst=:{rcvId=tcpRcvId}, pSt=:{ ls=ls=:{chan=chan=:{sChannel}}, io})
	# (sChannel, io)		= send_NB byteSeq sChannel io
	  (buffSize,sChannel)	= bufferSize sChannel

  //  disable this receiver, if the send channel is full

	  io = case buffSize of
	  		0 -> io
	  		_ -> disableReceivers [tcpRcvId] io
	= (rst, { pSt & ls={ ls & chan={chan & sChannel=sChannel}}, io=io })
rcvFun EOM (tcpRcvId, pSt=:{ ls=ls=:{chan=chan=:{sChannel}}, io})
	# (buffSize,sChannel)	= bufferSize sChannel
	  pSt	= { pSt & ls  = { ls & chan={chan & sChannel=sChannel}, eom=True}, io=io }

  //  close program only, if all data in the send channels ineternal buffer has been
  //  sent

	  pSt = case buffSize of
	  			0 	-> closeProcess (close pSt)
				_	-> pSt
	= (tcpRcvId, pSt)

//////////////////////////////////////////////////////////////////////////
////	sndFun - the callback function for the send channels receiver ////
//////////////////////////////////////////////////////////////////////////

sndFun :: SendEvent (*ReceiveSt,PState) -> (*ReceiveSt,PState)
sndFun Sendable (rst=:{rcvId=tcpRcvId}, pSt=:{ ls=ls=:{chan=chan=:{sChannel},eom=eomHappened}, io})
	# (sChannel, io)		= flushBuffer_NB sChannel io
	  (buffSize,sChannel)	= bufferSize sChannel
	  pSt	= { pSt & ls  = { ls & chan={chan & sChannel=sChannel}}, io=io }

  //  enable the receive channel's receiver again, if the send channel is still
  //  sendable

	  pSt = case (buffSize,eomHappened) of
	  			(0, False)	-> { pSt & io = enableReceivers [tcpRcvId] pSt.io }
	  			(0, True )	-> close pSt
				_			-> pSt
	= (rst, pSt)
sndFun Disconnected (rst, pSt)
	= (rst, closeProcess pSt)

close	::	PState -> PState
close pSt=:{ls=ls=:{chan=chan=:{sChannel}}, io}
	#!	io				= closeChannel sChannel io
	= { pSt & ls={ ls & chan={chan & sChannel=undef}}, io=io }
