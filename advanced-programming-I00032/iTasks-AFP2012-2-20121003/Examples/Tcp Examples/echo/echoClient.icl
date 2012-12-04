module echoClient

//	**************************************************************************************************
//  Clean tutorial example program.
//
//	A program to use together with echoServer
//
//	**************************************************************************************************

import	StdEnv, TCPIP, StdMaybe
from	StdSystem import ticksPerSecond

echoPort	:== 7

sendMessage	:==	"HELLO"

Start world
	#	(console, world)			= stdio world

  // get the IP address of the server
		console						= fwrites "enter address of echo server:" console
		(line, console)				= freadline console
		server						= line % (0, (size line)-2)
		(mbIPAddr, world)			= lookupIPAddress server world
	|	isNothing mbIPAddr
		#	console					= fwrites (server+++" not found\n") console
		= end console world

  // connect

	#	(tReport, mbDuplexChan, world)
									= connectTCP_MT (Just (15*ticksPerSecond))
												    (fromJust mbIPAddr, echoPort) world
	|	tReport<>TR_Success
		#!	console 				= fwrites (   server+++" does not respond on port "
											   +++toString echoPort+++"\n") console
		= end console world
	#!	console 					= fwrites	(server+++" responded on port "+++toString echoPort)
												console
		{ sChannel=sc, rChannel=rc }= fromJust mbDuplexChan

  // send something

		(sc, world)					= send (toByteSeq sendMessage) sc world

  // receive answer

		(bs, rc, world)				= receive rc world
		console						= fwrites (" with \""+++toString bs+++"\".\n") console

  // close

		world						= closeRChannel rc world
		world						= closeChannel sc world
	= end console world

end :: !*File !*World -> *World
end console world
	#!	console		= fwrites "press return to exit program" console
		(_, console)= freadline console
		(_,world)	= fclose console world
	= world