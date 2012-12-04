module chatServer

//  ********************************************************************************
//  Clean tutorial example program.
//  
//  This program demonstrates the usage of the selectChannel function
//
//  ********************************************************************************

import  StdEnv, StdTCPChannels, StdMaybe

chatPort	:==	2000

::	*ChanInfo
	=	{	sndChan		::	TCP_SChannel
		,	rcvChan		::	TCP_RChannel
		,	nickname	::	String
		}

Start world
	# (ok, mbListener, world)	= openTCP_Listener chatPort world
	| not ok
		= abort ("chatServer: can't listen on port "+++toString chatPort)
	# (console, world)		= stdio world
	  console	= fwrites "This server program waits until a client program " console
	  console	= fwrites "tries to connect.\n" console
	= loop (fromJust mbListener) [] console world


loop :: !TCP_Listener ![ChanInfo] !*File !*World -> *World
loop listener channels console world
	# (sChans, rChans, nicknames)
				= unzip3 channels
	  glue		= (TCP_Listeners [listener]) :^: (TCP_RChannels rChans)
	  ([(who,what):_],glue, _, world)
				= selectChannel_MT Nothing glue Void world
	  ((TCP_Listeners [listener:_]) :^: (TCP_RChannels rChans))
	  			= glue
	  channels	= zip3 sChans rChans nicknames
		
	// case 1: someone wants to join the chatroom
		
	| who==0
		# (tReport, mbNewMember, listener, world)
							= receive_MT (Just 0) listener world
		| tReport<>TR_Success            // the potential new member changed it's mind
			= loop listener channels console world
		# (_,{sChannel,rChannel})	= fromJust mbNewMember
		  (byteSeq, rChannel, world)
		  					= receive rChannel world
		  nickname			= toString byteSeq
		  message			= "*** "+++nickname+++" joined the group."
		  console			= fwrites (message+++"\n") console
		  channel			= { sndChan=sChannel, rcvChan=rChannel, nickname=nickname }
		  channels			= [channel:channels]
		  (channels, world)	= broadcastString message channels [] world
		| nickname % (0,3)=="quit"
			= quit listener channels world
		= loop listener channels console world

	// case 2: somebody has something to say

	| what==SR_Available
		# (channel=:{rcvChan, nickname}, channels)
		  						= selectList (who-1) channels
		  (byteSeq, rcvChan, world)
		  						= receive rcvChan world
		  message				= toString byteSeq
		  channels				= channels++[{channel & rcvChan=rcvChan}]
		  (channels, world)		= broadcastString	(nickname+++": "+++message)
		  											channels [] world
		= loop listener channels (fwrites message console)  world

	// case 3: somebody leaves the group
	
	| what==SR_EOM
	# ({sndChan, rcvChan, nickname}, channels)
		  						= selectList (who-1) channels
	  message					= "*** "+++nickname+++" left the group"
	  console					= fwrites (message+++"\n") console
	  (channels, world)			= broadcastString message channels [] world
	  world						= seq [closeChannel sndChan, closeRChannel rcvChan] world
	= loop listener channels console  world

broadcastString :: !String ![ChanInfo] ![ChanInfo] !*World -> ([ChanInfo],!*World)
broadcastString string [] akku world
	= (u_reverse akku, world)
broadcastString string [channel=:{sndChan}:channels] akku world
	# (sndChan, world)		= send (toByteSeq string) sndChan world
	= broadcastString string channels [{channel & sndChan=sndChan}:akku] world

//selectList :: !Int [.a] -> (!.a,![.a])
selectList n l
	# (left, [element:right])	= splitAt n l
	= (element, left++right)

quit listener channels world
	# world = closeRChannel listener world
	= closeChannels channels world

closeChannels [] world
	= world
closeChannels [{sndChan, rcvChan}: channels] world
	# world	= seq [closeChannel sndChan, closeRChannel rcvChan] world
	= closeChannels channels world

unzip3 :: ![ChanInfo] -> (![TCP_SChannel], ![TCP_RChannel], ![String])
unzip3 [] = ([],[],[])
unzip3 [{sndChan, rcvChan, nickname}:t]
	# (a,b,c) = unzip3 t
	= ([sndChan:a], [rcvChan:b], [nickname:c])

zip3 :: ![TCP_SChannel] ![TCP_RChannel] ![String] -> [ChanInfo]
zip3 [] [] [] = []
zip3 [sndChan:a] [rcvChan:b] [nickname:c]
	= [{sndChan=sndChan, rcvChan=rcvChan, nickname=nickname} : zip3 a b c]

u_reverse list = reverse_ list []
where 
	reverse_ [hd:tl] list	= reverse_ tl [hd:list]
	reverse_ [] list		= list
