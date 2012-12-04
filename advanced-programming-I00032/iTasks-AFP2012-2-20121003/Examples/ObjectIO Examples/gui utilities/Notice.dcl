definition module Notice

//	**************************************************************************************************
//
//	A new instance of the Dialogs type constructor class to easily create simple notice dialogues.
//
//	This module has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import StdWindow

::	Notice ls pst
 =	Notice [String] (NoticeButton *(ls,pst)) [NoticeButton *(ls,pst)]
::	NoticeButton st
 =	NoticeButton String (IdFun st)

instance Dialogs Notice

openNotice  :: !(Notice .ls (PSt .l)) !(PSt .l) -> PSt .l
/*	openNotice can be used to create a Notice without having to bother about the ErrorReport result.
*/
