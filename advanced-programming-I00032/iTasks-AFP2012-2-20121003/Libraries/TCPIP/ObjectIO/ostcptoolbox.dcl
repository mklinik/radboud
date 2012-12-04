definition module ostcptoolbox

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	Author: Martin Wierich
//	Modified: 15 October 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdMaybe
import	TCPDef
from	TCPChannelClass import :: Timeout
import	tcp
import	ostoolbox


OSinstallTCP				:: !*OSToolbox -> *OSToolbox
