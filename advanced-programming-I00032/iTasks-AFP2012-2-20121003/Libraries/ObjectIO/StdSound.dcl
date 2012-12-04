definition module StdSound


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdSound specifies sound playing functions.
//	NOTE: This is an experimental extension of the Object I/O library.
//	********************************************************************************

import	StdString


class playSoundFile env :: !String !*env -> (!Bool,!*env)

/*	playSoundFile filename 
		opens the sound file at filename and plays it synchronously. 
		The Boolean result indicates whether the sound file could be succesfully 
		played.
*/

instance playSoundFile World
