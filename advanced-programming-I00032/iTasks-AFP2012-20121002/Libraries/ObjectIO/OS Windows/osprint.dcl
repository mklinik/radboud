definition module osprint

//	Clean Standard Object I/O library, version 1.2

import StdFile, StdPicture, iostate

::	PrintSetup
::	JobInfo
	=	{	range	::	!(!Int,!Int)	// First and last page as typed in by the 
										// user. If the user chooses "ALL", then the
										// first page will be one, and the last page 
										// will be a "huge" number. 
		,	copies	::	!Int			// Number of copies. This will not 
										// necessarily be equal to the number of 
										// copies, as specified in the print dialog.
										// Some printer drivers take themselves care
										// of producing the appropriate number of 
										// copies => printInfo.copies==1.
		}
::	PrintInfo
	=	{	printSetup	::	PrintSetup	// PC:	the print setup, which was chosen by
										//		the user via the print dialog
										// Mac:	the value will be identical to the
										//		actual PrintSetup argument, that was
										//		passed to one of the printing
										//		functions
		,	jobInfo		::	JobInfo
		}
::	Alternative x y
	=	Cancelled x
	|	StartedPrinting y

os_getpagedimensions	:: !PrintSetup !Bool -> (!(!Int,!Int),!(!(!Int,!Int),!(!Int,!Int)),!(!Int,!Int))
os_defaultprintsetup	::             !*env -> (!PrintSetup, !*env)
os_printsetupvalid		:: !PrintSetup !*env -> (!Bool,       !*env)
						
class PrintEnvironments printEnv where
	os_printpageperpage :: !Bool !Bool 
						   !.x
						   .(.x -> .(PrintInfo -> .(*Picture -> *((.Bool,Point2),*(.state,*Picture)))))
						   (*(.state,*Picture) -> *((.Bool,Point2),*(.state,*Picture)))
						   !PrintSetup !*printEnv
						-> (Alternative .x .state,!*printEnv)
	os_printsetupdialog	::  !PrintSetup !*printEnv
						-> (!PrintSetup,!*printEnv)
		
instance PrintEnvironments Files
instance PrintEnvironments (PSt .l)

os_printsetuptostring	:: !PrintSetup -> String
os_stringtoprintsetup	:: !String     -> PrintSetup
