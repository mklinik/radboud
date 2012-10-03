module downloadHTTP

//	**************************************************************************************************
//
//	A program that uses HTTP to download the beginning of a HTML page
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import	StdEnv, TCPIP, StdMaybe
from	StdSystem import ticksPerSecond

server	:== "www.cs.kun.nl"
path	:== "/~clean/"		// the Clean homepage

httpCommand	= "GET "+++path+++" HTTP/1.0\xD\xA\xD\xA"
port	:== 80

Start world
	#	(console, world)			= stdio world

  // get the IP address of the server

		(mbIPAddr, world)			= lookupIPAddress server world
	|	isNothing mbIPAddr
		#	console					= fwrites (server+++" not found\n") console
		= fclose console world

  // connect

	#	(tReport, mbDuplexChan, world)
									= connectTCP_MT (Just (15*ticksPerSecond))
									                (fromJust mbIPAddr, port) world
	|	tReport<>TR_Success
		#!	console 				= fwrites (   server+++" does not respond on port "
											   +++toString port+++"\n") console
		= fclose console world
	#!	console 					= fwrites (server+++" responded on port "+++toString port+++"\n")
											  console
		{ sChannel=sc, rChannel=rc }= fromJust mbDuplexChan

  // send http command

		(sc, world)					= send (toByteSeq httpCommand) sc world

  // receive answer

		(tReport, mbBs, rc, world)	= receive_MT (Just (20*ticksPerSecond)) rc world
		console	= case tReport of
					TR_Success		-> fwrites (toString (fromJust mbBs)) console
					_				-> fwrites (server+++" does not send anything (timeout expired)")
											   console

  // close

		world						= closeRChannel rc world
		world						= closeChannel sc world
	= fclose console world
