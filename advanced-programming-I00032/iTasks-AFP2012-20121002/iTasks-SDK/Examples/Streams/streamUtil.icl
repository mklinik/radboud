implementation module streamUtil

import iTasks


// spawn an asyncronous process

spawnP :: String (Task a) -> Task (Task a) | iTask a					// spawn process, return task fetching its result
spawnP name ta	
	= 					spawnProcess RootUser True (name @>> ta)		// spawn of asynchronous process
		>>= \pid ->		return (waitFor name pid)						// return task which waits for its result upon evaluation
where
	waitFor :: String (ProcessRef a) -> Task a | iTask a
	waitFor name pid 
	=	//				prompt name 									// prompt that we are waiting 
		//				||- 
						waitForProcess pid 								// wait for process to complete
		>>= \mbVal -> //	deleteProcess pid								// delete from process table
/*		>>| 	*/		return (fromJust mbVal)							// and return its value
	
	prompt name			= showStickyMessage ("Waiting for process " +++ name) 

// wait for such a process

waitP :: (a -> Task  b) (Task a) -> Task b  | iTask a & iTask b
waitP fun str = str >>= fun




