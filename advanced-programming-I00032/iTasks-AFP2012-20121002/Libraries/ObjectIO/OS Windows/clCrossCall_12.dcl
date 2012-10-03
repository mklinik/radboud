definition module clCrossCall_12

//	Clean Object I/O library, version 1.2

import	StdOverloaded, StdString
import	ostoolbox

  //----------------------------------------------//
 //    Crosscall infrastructure                  //
//----------------------------------------------//

//	CrossCallInfo is the basic record that is passed between the Clean thread and the OS thread:
::	CrossCallInfo
	=	{	ccMsg	:: !Int		// The message nr: Clean->OS use ccRq...; OS->Clean use ccWm...
		,	p1		:: !Int
		,	p2		:: !Int
		,	p3		:: !Int
		,	p4		:: !Int
		,	p5		:: !Int
		,	p6		:: !Int
		}

call_back_clean_object_io :: !Int !Int !Int !Int !Int !Int !Int !*OSToolbox -> *(!Int,!Int,!Int,!Int,!Int,!Int,!Int,!*OSToolbox);

//	Crosscall with state parameter:
errorCallback :: !String !CrossCallInfo !.s !*OSToolbox -> (!CrossCallInfo,!.s,!*OSToolbox)
issueCleanRequest :: !(CrossCallInfo -> .(.s -> .(*OSToolbox -> *(CrossCallInfo,.s,*OSToolbox))))
					 !CrossCallInfo
					 !.s
					 !*OSToolbox
				  -> (!CrossCallInfo,!.s,!*OSToolbox)

//	Crosscall without state parameter:
errorCallback2 :: !String !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
issueCleanRequest2 :: !(CrossCallInfo -> .(*OSToolbox -> *(CrossCallInfo,*OSToolbox)))
					 !CrossCallInfo
					 !*OSToolbox
				  -> (!CrossCallInfo,!*OSToolbox)

consolePrint	:: !{#Char} !*OSToolbox ->  *OSToolbox
iprint			:: !String !.a -> .a
iprint`			:: !String !.a -> .a

Rq0Cci msg :== {ccMsg=msg,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0}
Rq1Cci msg v1 :== {ccMsg=msg,p1=v1,p2=0,p3=0,p4=0,p5=0,p6=0}
Rq2Cci msg v1 v2 :== {ccMsg=msg,p1=v1,p2=v2,p3=0,p4=0,p5=0,p6=0}
Rq3Cci msg v1 v2 v3 :== {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=0,p5=0,p6=0}
Rq4Cci msg v1 v2 v3 v4 :== {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=0,p6=0}
Rq5Cci msg v1 v2 v3 v4 v5 :== {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=v5,p6=0}
Rq6Cci msg v1 v2 v3 v4 v5 v6 :== {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=v5,p6=v6}

return0Cci   ::				                     CrossCallInfo
return1Cci   :: !Int			              -> CrossCallInfo
return2Cci   :: !Int !Int		              -> CrossCallInfo
return3Cci   :: !Int !Int !Int                -> CrossCallInfo
return4Cci   :: !Int !Int !Int !Int           -> CrossCallInfo
return5Cci   :: !Int !Int !Int !Int !Int      -> CrossCallInfo
return6Cci   :: !Int !Int !Int !Int !Int !Int -> CrossCallInfo


  //---------------------------------------------------------------------//
 //  Synchronisation operations between the Clean thread and OS thread  //
//---------------------------------------------------------------------//
winEndOs			:: !*OSToolbox -> *OSToolbox
winBeginOs			:: !*OSToolbox -> *OSToolbox

winCloseOs				:: !*OSToolbox -> Bool
winInitOs				:: (!Bool,!*OSToolbox)


  //------------------------------------------------------------------------//
 //  The message numbers for communication from Clean to OS (ccMsg field)  //
//------------------------------------------------------------------------//
// Mike //
CcRqUSERGAMEEVENT			:== 1905
CcRqCREATEGAMEOBJECT		:== 1904
CcRqPLAYSOUNDSAMPLE			:== 1903

CcRqRUNGAME					:== 1901
CcRqCREATEGAMEWINDOW		:== 1900
///
// MW...
CcRqDO_PRINT_SETUP			:== 1828 // MW11++
CcRqDO_HTML_HELP			:==	1827

CcRqGET_PRINTER_DC			:== 1824
CcRqDISPATCH_MESSAGES_WHILE_PRINTING
							:== 1823
CcRqENDDOC					:== 1822
CcRqSTARTDOC				:== 1821
// ... MW
CcRqCREATETCPWINDOW			:==	1820		/* create TCP window */

CcRqDESTROYMDIDOCWINDOW 	:== 1817		// PA: added to destroy MDI document window
CcRqCREATESDIDOCWINDOW		:==	1816		// PA: added to create SDI document window
CcRqCREATEMDIDOCWINDOW		:== 1815		// PA: added to create MDI document window
CcRqCREATEMDIFRAMEWINDOW	:== 1814		// PA: added to create MDI frame window
CcRqCREATESDIFRAMEWINDOW	:== 1813		// PA: added to create SDI frame window
CcRqCLIPBOARDHASTEXT		:== 1812
CcRqGETCLIPBOARDTEXT		:== 1811
CcRqSETCLIPBOARDTEXT		:== 1810
CcRqGETCLIPBOARDCOUNT		:== 1809		/* PA: added to retrieve clipboard count. */

CcRqDIRECTORYDIALOG			:==	1802		/* PA: added to create directory selector dialog. */
CcRqFILESAVEDIALOG			:== 1801
CcRqFILEOPENDIALOG			:== 1800

CcRqSHOWCONTROL				:== 1755		/* PA: added */
CcRqSELECTPOPUPITEM			:== 1754
CcRqENABLEPOPUPITEM			:== 1753
CcRqADDTOPOPUP				:== 1752
CcRqSETITEMCHECK			:== 1751
CcRqENABLECONTROL			:== 1750

CcRqCREATECOMPOUND			:== 1729		/* PA: added */
CcRqCREATESCROLLBAR			:== 1728		/* PA: added */
CcRqCREATECUSTOM			:== 1727
CcRqCREATEICONBUT			:== 1726
CcRqCREATEPOPUP				:== 1725
CcRqCREATECHECKBOX			:== 1724
CcRqCREATERADIOBUT			:== 1723
CcRqCREATEEDITTXT			:== 1722
CcRqCREATESTATICTXT			:== 1721
CcRqCREATEBUTTON			:== 1720

CcRqCREATEMODALDIALOG		:== 1701		/* PA: added to create modal dialog. */
CcRqCREATEDIALOG			:== 1700

CcRqCREATETOOLBARSEPARATOR	:==	1603		/* PA: added to create a toolbar separator item. */
CcRqCREATETOOLBARITEM		:== 1602		/* PA: added to create a toolbar bitmap item. */
CcRqCREATEMDITOOLBAR		:== 1601		/* PA: added to create a toolbar for a MDI process. */
CcRqCREATESDITOOLBAR		:==	1600		/* PA: added to create a toolbar. */

CcCbFONTSIZE				:== 1530

CcCbFONTNAME				:== 1520

CcRqGETFONTSIZES			:== 1510

CcRqGETFONTNAMES			:== 1500

CcRqSETCLIENTSIZE			:==	1438		/* PA: added to set client size. */
CcRqDELCONTROLTIP			:==	1437		/* PA: added to remove controls from tooltip areas. */
CcRqADDCONTROLTIP			:==	1436		/* PA: added to add controls to tooltip areas. */
CcRqGETWINDOWSIZE			:==	1435		/* PA: added to retrieve bounding size of windows. */
CcRqRESTACKWINDOW			:==	1434		/* PA: added to restack windows. */
CcRqSHOWWINDOW				:==	1433		/* PA: added to (hide/show) windows. */
CcRqSETWINDOWSIZE			:==	1432		/* PA: added to resize windows/controls. */
CcRqSETSELECTWINDOW			:== 1431		/* PA: added to (en/dis)able windows. */
CcRqSETWINDOWPOS			:== 1430		/* PA: added to move windows/controls. */

CcRqSETEDITSELECTION		:== 1428		/* PA: added for handling edit control selections. */
CcRqSETSCROLLSIZE			:==	1427		/* PA: added for setting thumb size of scrollbar. */
CcRqSETSCROLLPOS			:== 1426		/* PA: added for setting thumb of scrollbar. */
CcRqSETSCROLLRANGE			:== 1425		/* PA: added for setting range of scrollbar. */
CcRqRESETCURSOR				:== 1424
CcRqSETGLOBALCURSOR			:== 1423
CcRqOBSCURECURSOR			:== 1422
CcRqCHANGEWINDOWCURSOR		:== 1421
CcRqACTIVATEWINDOW			:==	1420		/* PA: added for activating window. */
CcRqACTIVATECONTROL			:== 1419		/* PA: added for activating controls. */

CcRqGETWINDOWPOS			:== 1416
CcRqGETCLIENTSIZE			:== 1415

CcRqUPDATEWINDOWRECT		:== 1412		/* PA: added for updating rect part of a window/control. */
CcRqGETWINDOWTEXT			:== 1411
CcRqSETWINDOWTITLE			:== 1410

CcRqFAKEPAINT				:==	1405		/* PA: added combination of BeginPaint; EndPaint; InvalidateRect; */
CcRqENDPAINT				:== 1404
CcRqBEGINPAINT				:== 1403
CcRqDESTROYWINDOW			:== 1402
CcRqDESTROYMODALDIALOG		:==	1401		/* PA: added to destroy modal dialog. */

CcRqDRAWMBAR				:== 1265

CcRqTRACKPOPMENU			:==	1256		/* PA: added for handling pop up menu. */
CcRqCREATEPOPMENU			:== 1255

CcRqINSERTSEPARATOR			:== 1245

CcRqMENUENABLE				:== 1235

CcRqMODIFYMENU				:== 1230

CcRqINSERTMENU				:==	1226		// PA: new constant for inserting a new menu into the menu bar

CcRqITEMENABLE				:== 1220

CcRqREMOVEMENUSHORTKEY		:==	1217		// PA: new constant for removing a shortkey of a menu item
CcRqADDMENUSHORTKEY			:== 1216		// PA: new constant for adding a shortkey of a menu item
CcRqMODIFYMENUITEM			:== 1215
CcRqDESTROYMENU				:== 1214		// PA: new constant for destroying a menu 'physically'
CcRqDELETEMENU				:==	1213		// PA: new constant for deleting a menu logically
CcRqREMOVEMENUITEM			:== 1212

CcRqCHECKMENUITEM			:== 1210

CcRqINSERTMENUITEM			:== 1205

CcRqDOMESSAGE				:== 1100

  //------------------------------------------------------------------------//
 //  The message numbers for communication from OS to Clean (CcMsg field)  //
//------------------------------------------------------------------------//
CcWINMESSmax				:== 999

// Mike: Convention for OS to Clean requests: 500-599 //
CcWmCHECKQUIT				:== 513     /* Mike: check user's quit function */
CcWmUSEREVENT				:== 512     /* Mike: user defined event */
CcWmSTATISTICS				:== 511     /* Mike: request for statistics */
CcWmOBJECTKEYUP				:== 510     /* Mike: key released */
CcWmOBJECTKEYDOWN			:== 509     /* Mike: key pressed for object */
CcWmOBJECTTIMER				:== 508     /* Mike: framecounter reached 0 */
CcWmANIMATION				:== 507     /* Mike: animation sequence ended */
CcWmCOLLISION				:== 506     /* Mike: collision of two objects */
CcWmTOUCHBOUND				:== 505     /* Mike: object touches bound or code */
CcWmOBJECTDONE				:== 504     /* Mike: object is destroyed */
CcWmMOVEOBJECT				:== 503     /* Mike: move object */
CcWmINITOBJECT				:== 502     /* Mike: initialize new object */
CcWmSCROLL					:== 501     /* Mike: calculate layer position */
CcWmGAMEKEYBOARD			:== 500     /* Mike: keyboard input for game */
///
CcWmINETEVENT				:==	140		/* MW11 */

CcWmZEROTIMER				:==	136		/* PA: new constant for sequence of zero timer events (generated only by Clean). */
CcWmLOSTKEY					:== 135		/* PA: new constant for loosing keyboard input (generated only by Clean). */
CcWmLOSTMOUSE				:== 134		/* PA: new constant for loosing mouse input    (generated only by Clean). */
CcWmSPECIALBUTTON			:==	133		/* PA: new constant for info about OK/CANCEL button selected. */
CcWmPROCESSDROPFILES		:== 132		/* PA: new constant for requesting opening of files. */
CcWmGETTOOLBARTIPTEXT		:==	131		/* PA: new constant for getting tooltip text. */
CcWmSETFOCUS				:==	130		/* PA: new constant for notifying obtaining keyboard input focus. */
CcWmKILLFOCUS				:==	129		/* PA: new constant for notifying loss of keyboard input focus. */

CcWmPROCESSCLOSE			:==	127		/* PA: new constant for requesting closing of process. */
CcWmDRAWCLIPBOARD			:==	126		/* PA: new constant for clipboard handling. Copied from Ronny. */
CcWmGETSCROLLBARINFO		:==	125		/* PA: new constant for info about scrollbars. */
CcWmSCROLLBARACTION			:== 124		/* PA: new constant for scrollbar handling. */
CcWmDDEEXECUTE				:== 123
CcWmMOUSEWHEEL				:== 122		/* PA: new constant for mouse wheel event. */
CcWmIDLEDIALOG				:== 121		/* PA: old constant reused for initialising modal dialogues. */
CcWmDRAWCONTROL				:== 120
CcWmCOMBOSELECT				:== 119
CcWmBUTTONCLICKED			:== 118
CcWmINITDIALOG				:== 117
CcWmIDLETIMER				:== 116
CcWmTIMER					:== 115
CcWmNEWVTHUMB				:== 114
CcWmNEWHTHUMB				:== 113
CcWmGETVSCROLLVAL			:== 112
CcWmGETHSCROLLVAL			:== 111
CcWmSIZE					:== 110		/* PA: old constant reused for passing resize information. */
CcWmMOUSE					:== 109
CcWmKEYBOARD				:== 108
CcWmDEACTIVATE				:== 107
CcWmACTIVATE				:== 106
CcWmCLOSE					:== 105
CcWmCOMMAND					:== 103
CcWmCHAR					:== 102
CcWmCREATE					:== 101
CcWmPAINT					:== 100

CcWINMESSmin				:== 100

CcWmNOTIFY					:== 78

CcRETURNmax					:== 19

CcRETURN6					:== 16
CcRETURN5					:== 15
CcRETURN4					:== 14
CcRETURN3					:== 13
CcRETURN2					:== 12
CcRETURN1					:== 11
CcRETURN0					:== 10

CcRETURNmin					:== 10

CcWASQUIT					:== 1

instance toInt Bool
