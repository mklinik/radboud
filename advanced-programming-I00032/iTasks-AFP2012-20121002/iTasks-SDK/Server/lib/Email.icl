implementation module Email

import TCPChannels
import StdMaybe
import StdTuple, StdInt, StdList

// Send function which sends the e-mail out
sendEmail :: [EmailOption] Email *World -> (Bool,*World)
sendEmail options email world
	// Lookup ip address of SMTP server
	# (mbIp, world)					= lookupIPAddress (getSMTPServerOpt options) world
	| isNothing mbIp				= (False,world)
	// Connect to the SMTP server
	# (tReport, mbDuplex, world)	= connectTCP_MT Nothing ((fromJust mbIp),25) world
	| tReport <> TR_Success			= (False,world)
	# {sChannel, rChannel}			= fromJust mbDuplex
	// Read welcome message
	# (msg, rChannel, world)		= receive rChannel world
	| statusCode msg <> 220			= (False,world)
	// Send HELO command
	# (sChannel, world)				= send (toByteSeq mkSMTPHelo) sChannel world
	# (msg, rChannel, world)		= receive rChannel world
	// Send FROM command
	# (sChannel, world)				= send (toByteSeq (mkSMTPFrom email)) sChannel world
	# (msg, rChannel, world)		= receive rChannel world
	| statusCode msg <> 250			= (False,world)
	// Send TO command
	# (sChannel, world)				= send (toByteSeq (mkSMTPTo email)) sChannel world
	# (msg, rChannel, world)		= receive rChannel world
	| statusCode msg <> 250			= (False,world)
	// Send DATA command
	# (sChannel, world)				= send (toByteSeq mkSMTPData) sChannel world
	# (msg, rChannel, world)		= receive rChannel world
	| statusCode msg <> 354			= (False,world)
	// Send body
	# (sChannel, world)				= send (toByteSeq (mkSMTPBody email)) sChannel world
	# (msg, rChannel, world)		= receive rChannel world
	| statusCode msg <> 250			= (False,world)	
	// Send QUIT command
	# (sChannel, world)				= send (toByteSeq mkSMTPQuit) sChannel world
	# (msg, rChannel, world)		= receive rChannel world //Ignore this reply, we are done anyway
	// Disconnect (the server should have already done this)
	# world							= closeChannel sChannel world
	# world							= closeRChannel rChannel world
	= (True,world)

// SMTP Protocol handling functions

// Make the SMTP Helo command
mkSMTPHelo :: String
mkSMTPHelo = "HELO localhost\r\n"

// Make the SMTP From command
mkSMTPFrom :: Email -> String
mkSMTPFrom email = "MAIL FROM:<" +++ (cleanupEmailString email.email_from) +++ ">\r\n"

// Make the SMTP To command
mkSMTPTo :: Email -> String
mkSMTPTo email = "RCPT TO:<" +++ (cleanupEmailString email.email_to) +++ ">\r\n"

// Make the SMTP Data command
mkSMTPData :: String
mkSMTPData = "DATA\r\n"

// Make the SMTP message body that is sent after the message 
mkSMTPBody :: Email -> String
mkSMTPBody email	=	"From: " +++ (cleanupEmailString email.email_from) +++ "\r\n"
					+++	"To: " +++ (cleanupEmailString email.email_to) +++ "\r\n"
					+++	"Subject: " +++ (cleanupEmailString email.email_subject) +++ "\r\n"
					+++ "\r\n" +++ email.email_body +++ "\r\n.\r\n"

mkSMTPQuit :: String
mkSMTPQuit = "QUIT\r\n"

//Utility functions
getSMTPServerOpt :: [EmailOption] -> String
getSMTPServerOpt [] 						= "localhost" //Default is localhost
getSMTPServerOpt [EmailOptSMTPServer s:xs]	= s
getSMTPServerOpt [x:xs] 					= getSMTPServerOpt xs

//Parse the reply of the server into a status code
statusCode :: ByteSeq -> Int
statusCode msg = toInt ((toString msg) % (0,2))

//Strip any newline chars and tabs from a string.
cleanupEmailString :: String -> String
cleanupEmailString s = toString (filter (\x -> not (isMember x ['\r\n\t'])) (fromString s))




