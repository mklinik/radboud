definition module monitor


import	StdReceiver


::	MonitorMessage					// The message type of the monitor process
	=	BeginSession				// Begin a typist session
	|	KeyHit Char					// Register key stroke
	|	EndSession					// End a typist session
	|	Quit						// Close the monitor process


openMonitor :: ItemPos (RId MonitorMessage) (PSt .l) -> PSt .l
/*	openMonitor creates the monitor process that will keep track of keyboard input.
	
	It expects a stream MonitorMessages in the following order:
	-	BeginSession:	start registering key hits
	-	(KeyHit _)* :	register a key hit
	-	EndSession  :	end registering key hits
	
	The monitor process displays the key hits in a window and presents the average 
	key hits per second.
*/
