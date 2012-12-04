/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	TCP event handling is done in the dedicated window class TCPWindowClassName instead of 
	the ghMainWindow in cCrossCall_121.c. The handle to this window  instance is ghTCPWindow.
	TCP events are sent by the routines in cTCP_121.c to ghMainWindow. The window procedure
	of ghMainWindow passes these events on to ghTCPWindow.
********************************************************************************************/
#include "Clean.h"
#include "cCrossCallTCP_121.h"
#include "cCrossCall_121.h"

extern  HWND    *ghMainWindow_p_for_tcp;
static int PM_SOCKET_EVENT_for_tcp;

//	Global data with external references:
DNSInfo *DNSInfoList = NULL;

/*	Registered Windows tcp window class name.
*/
static char TCPWindowClassName[]  = "__CleanTCPWindow";		/* Class for TCP child window of ghMainWindow.  */


/*	This function is used for looking up an element with a matching dnsHdl-field in
	the DNSInfoList. This element will then be removed from the list.
*/
static void lookUpAndRemove(WPARAM dnsHdl,DNSInfo **listPtr,DNSInfo **elPtr)
{
	if ((WPARAM)(*listPtr)->dnsHdl==dnsHdl)
		{	// the object to look up has been found, so remove it from the list
			// and give it back via elPtr.
			*elPtr = *listPtr;
			*listPtr = (*listPtr)->next;
		}
	  else
		lookUpAndRemove(dnsHdl, &(*listPtr)->next, elPtr);	
}


/*	The callback routine for a TCP window (extracted from MainWindowProcedure).
	It handles only PM_SOCKET_EVENT and PM_DNS_EVENT.
*/
static LRESULT CALLBACK TCPWindowProcedure (HWND hWin, UINT uMess, WPARAM wPara, LPARAM lPara)
{
	printMessage ("TCP Window", hWin, uMess, wPara, lPara);
	switch (uMess)
	{
		case PM_SOCKET_EVENT:
			{
				// wPara is the socket handle
				// LOWORD(lPara) is the message
				// HIWORD(lPara) is an error code
				switch (LOWORD(lPara))
				{	case FD_OOB:
					case FD_READ:	SendMessage3ToClean(CcWmINETEVENT,IE_RECEIVED, wPara,
														RChanReceiver);
									break;
					case FD_WRITE:	SendMessage3ToClean(CcWmINETEVENT,IE_SENDABLE, wPara,
														SChanReceiver);
									break;
					case FD_ACCEPT:	SendMessage3ToClean(CcWmINETEVENT,IE_CONNECTREQUEST, wPara,
														ListenerReceiver);
									break;
					case FD_CONNECT:SendMessage3ToClean(
										CcWmINETEVENT,
										HIWORD(lPara)==0 ?	IE_ASYNCCONNECTCOMPLETE :
															IE_ASYNCCONNECTFAILED,
										wPara,
										ConnectReceiver);
									break;
					case FD_CLOSE:	{
									dictitem	*pDictitem;
									pDictitem		= lookup(wPara);
									if (pDictitem) {
										if (pDictitem->hasReceiveNotifier)
											SendMessage3ToClean(CcWmINETEVENT,IE_EOM, wPara,
																RChanReceiver);
									
										if (pDictitem->hasSendableNotifier && HIWORD(lPara)!=0)
											SendMessage3ToClean(CcWmINETEVENT,IE_DISCONNECTED, wPara,
																SChanReceiver);
										};
									};
									break;
				};
				return 0;
			};
			break;
		case PM_DNS_EVENT:
			{ // wPara contains the DNS handle (the handle created by WSAAsyncGetHostByName
			  // The IP-adress of the looked up host will have been written into the
			  // corresponding element of the DNSInfoList. Look it up:

			  struct DNSInfo	*elPtr;
			  int				errCode;

			  errCode = HIWORD(lPara);

			  lookUpAndRemove(wPara,&DNSInfoList,&elPtr);
			  
			  // *elPtr contains the info

			  SendMessage4ToClean(	CcWmINETEVENT,
									errCode ?	IE_IPADDRESSNOTFOUND :
												IE_IPADDRESSFOUND,							
									elPtr->dnsHdl,
									DNSReceiver,
									errCode ?
										0 :
										ntohl(((int*)(*(elPtr->junion.Hostent.h_addr_list)))[0])
								 );

			  // deallocate unused memory
			  LocalFree(elPtr);
			  return 0;
			};
			break;
		default:
			return DefWindowProc (hWin, uMess, wPara, lPara);
			break;
	}
	return 0;
}	/*	TCPWindowProcedure */


/*	Register the TCPWindow class:
*/
void InitialiseCrossCallTCP (void)
{
	WNDCLASSEX wclass;

	/* register tcp window class */
	wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = CS_NOCLOSE;
	wclass.lpfnWndProc   = (WNDPROC) TCPWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = NULL;
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = TCPWindowClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);
}


/*	Cross call procedure implementations.
	Eval<nr> corresponds with a CrossCallEntry generated by NewCrossCallEntry (nr,Eval<nr>).
*/
void EvalCcRqCREATETCPWINDOW (CrossCallInfo *pcci)		/* No cross call args; no result. */
{
	if (!ghTCPWindow)
		ghTCPWindow	= CreateWindow (TCPWindowClassName,		/* Class name					 */
									"",					 	/* Window title 				 */
									WS_POPUP,				/* style flags					 */
									0, 0,					/* x, y 						 */
									0, 0,					/* width, height 				 */
									NULL,					/* Parent window				 */
									(HMENU) NULL,			/* menu handle					 */
									(HANDLE) ghInst,		/* Instance that owns the window */
									0
								);
	MakeReturn0Cci (pcci);
}

int InstallCrossCallTCP (int ios)
{
	if (!ghTCPWindow)
	{
		CrossCallProcedureTable newTable;

		InitialiseCrossCallTCP ();

		newTable = EmptyCrossCallProcedureTable ();
		AddCrossCallEntry (newTable, CcRqCREATETCPWINDOW,EvalCcRqCREATETCPWINDOW);
		AddCrossCallEntries (gCrossCallProcedureTable, newTable);
	}
	return ios;
}

void lookupHost_asyncC (CleanString inetAddr, int *errCode, HANDLE *endpointRef)
// errCode: 0 ok, 1 not ok
{
	DNSInfo		*newPtr;
	HANDLE		dnsHdl;

	StartUp (TRUE);

	*errCode	= 1;
	newPtr = (DNSInfo*) LocalAlloc (LMEM_FIXED,sizeof(DNSInfo));
	if (newPtr==NULL){
		*errCode	= 1;
		return;
	}

	newPtr->next = DNSInfoList;
	DNSInfoList = newPtr;

	if (ghMainWindow_p_for_tcp==NULL){
		ghMainWindow_p_for_tcp = &ghMainWindow;
		PM_SOCKET_EVENT_for_tcp = PM_SOCKET_EVENT;
	}

	// and fill the fields and initiate DNS lookup.
	dnsHdl = WSAAsyncGetHostByName (ghMainWindow,PM_DNS_EVENT,CleanStringCharacters(inetAddr),
									DNSInfoList->junion.freeSpace,
									MAXGETHOSTSTRUCT);
	// this will cause the sending of a PM_DNS_EVENT message to the main window.
	// The wParam value of that message will be equal to dnsHdl so that
	// the ipAdress of the lookedup host can be retrieved then.
	// The element of the List should be deallocated then
	if (dnsHdl==0)
		return;

	DNSInfoList->dnsHdl = dnsHdl;
	*errCode			= 0;
	*endpointRef		= dnsHdl;
}

void os_connectTCP_asyncC (int onlyForMac, int ipAddr, int portnum,
						   int *errCodeP, size_t *endpointRefP)
// errCode: 0 ok;	1 not ok
{
	SOCKET client;
	SOCKADDR_IN	srvAdr,clientAdr;
	int err, tru;

	*errCodeP = 1;
		
	client = socket(PF_INET, SOCK_STREAM, 0);
	if (client==INVALID_SOCKET)
		return;

	clientAdr.sin_family = AF_INET;			// of course internet adress family
	clientAdr.sin_addr.s_addr = INADDR_ANY;	// internet adress will be given after "connect" 
	clientAdr.sin_port = 0;					// the winsock library will choose a free number between 1024 and 5000
	
	err = bind(client, (LPSOCKADDR) &clientAdr, sizeof(clientAdr));
	if (err){
		closesocket(client);
		return;
	}

	srvAdr.sin_family = AF_INET;			// of course internet adress family
	srvAdr.sin_addr.s_addr = htonl(ipAddr);
	srvAdr.sin_port = htons((short int)portnum);	
	
	tru				= TRUE;

	if (ghMainWindow_p_for_tcp==NULL){
		ghMainWindow_p_for_tcp = &ghMainWindow;
		PM_SOCKET_EVENT_for_tcp = PM_SOCKET_EVENT;
	}

	err = WSAAsyncSelect(client,ghMainWindow,PM_SOCKET_EVENT,FD_CONNECT);
	if (err){
		closesocket(client);
		return;
	}

	err = connect(client, (LPSOCKADDR) &srvAdr, sizeof(srvAdr));
	if (err==SOCKET_ERROR){
		err = WSAGetLastError();		// a WSAEWOULDBLOCK error is a pretty harmless thing
		if (err!=WSAEWOULDBLOCK){
			closesocket(client);
			return;
		}
	}
	*errCodeP			= 0;
	*endpointRefP		= client;

	*errCodeP	= insertNewDictionaryItem(client);
	if (*errCodeP){
		closesocket(client);
		return;
	}

	{
		dictitem *ptr;
		ptr	= lookup(client);
		ptr->referenceCount			= 1;
		ptr->hasReceiveNotifier		= 0;
		ptr->hasSendableNotifier	= 1;
		ptr->aborted				= 0;
	}
}

void setEndpointDataC ( SOCKET endpointRef, int referenceCount,
						int hasReceiveNotifier, int hasSendableNotifier, int aborted)
{
	dictitem	*ptr			= lookup (endpointRef);
	
	if (ptr!=NULL){
		ptr->referenceCount			= referenceCount;
		ptr->hasReceiveNotifier		= hasReceiveNotifier ? 1 : 0;
		ptr->hasSendableNotifier	= hasSendableNotifier ? 1 : 0;
		ptr->aborted				= aborted ? 1 : 0;
	};

	if (ghMainWindow_p_for_tcp==NULL){
		ghMainWindow_p_for_tcp = &ghMainWindow;
		PM_SOCKET_EVENT_for_tcp = PM_SOCKET_EVENT;
	}

	WSAAsyncSelect (endpointRef, ghMainWindow, PM_SOCKET_EVENT, 
						(hasReceiveNotifier ? FD_READ | FD_OOB | FD_ACCEPT | FD_CLOSE : 0)
					  |	(hasSendableNotifier ? FD_WRITE | FD_CLOSE : 0));
}

void os_select_inetevents (	SOCKET endpointRef, int receiverCategory,
							int referenceCount, int getReceiveEvents, int getSendEvents, 
							int aborted)
{
	setEndpointDataC (endpointRef, referenceCount, getReceiveEvents, getSendEvents, aborted);
}
