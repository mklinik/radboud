/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

//#define FD_SETSIZE "maximal number of sockets to select on" (default is 64) 

#include <winsock.h>

//#include "util_121.h"
#define rMessageBox myMessageBox

#include "Clean.h"
#include "cTCP_121.h"
#define trace(x)

static void myMessageBox (HWND owner, UINT style, char *title, char *format,...)
{
	static char mbuff[128];
	va_list arglist;

	va_start (arglist, format);
	wvsprintf (mbuff, format, arglist);
	va_end (arglist);

	MessageBox (owner, mbuff, title, style);
}

/*
------------------------ THE FUNCTION PROTOTYPES ------------------------------------------

Naming convention: functions, which are called from Clean end with a "C". Function's that
*/

//--------------------- GLOBAL VARIABLES ------------------------------------------

//	The inetEventQueue: To append easily a new item to the end of the list, "toLastNext" points
//	to the "next" field of the last list element (or to &inetEventQueue)


dictitem	*endpointDict = NULL;

int			tcpStartedUp = FALSE;
int			rcvBuffSize;	// contains the size of the sockets internal buffer for receiving data.
CleanString	pRcvBuff;

HWND *ghMainWindow_p_for_tcp=NULL;
int PM_SOCKET_EVENT_for_tcp=0;

extern	void (*exit_tcpip_function)();	// the function will be called, when the Clean
										// program terminates 

//--------------------- FUNCTION IMPLEMENTATION -----------------------------------

int tcpPossibleC (void)
{
	StartUp (FALSE);
	return tcpStartedUp;
}

int os_eom (SOCKET endpointRef)
{
	int			err, err2;
	char		dummyBuffer[1];
	dictitem	*pDictitem;

	err		= recv (endpointRef, dummyBuffer, 1, MSG_PEEK);
	err2	= WSAGetLastError();
	if (err>0)
		return FALSE;
	if (err<0 && err2==WSAEWOULDBLOCK)
		return FALSE;
	pDictitem	= lookup(endpointRef);
	trace(	if (!pDictitem)
				rMessageBox(NULL, MB_APPLMODAL, "in os_eom", "ERROR");)
	if (pDictitem->availByteValid)
		return FALSE;
	if (err==0)
		return TRUE;
	return TRUE;
}

int os_disconnected (SOCKET endpointRef)
{
	int		err;
	char	string[1];
	dictitem	*pDictitem;

	pDictitem	= lookup(endpointRef);
	trace(	if (!pDictitem)
				rMessageBox(NULL, MB_APPLMODAL, "in os_disconnected", "ERROR");)
	if (pDictitem->disconnected)
		return TRUE;
	err	= send(	endpointRef, string, 0, 0);
	if (err!=SOCKET_ERROR)
		return FALSE;
	  else
		return WSAGetLastError()!=WSAEWOULDBLOCK;
				// only this error can happen with sockets that can still send
}

int os_connectrequestavailable (SOCKET endpointRef)
{
	FD_SET	readSet;
	TIMEVAL timeout;
	int		nr;

	FD_ZERO(&readSet);
	FD_SET(endpointRef,&readSet);

	timeout.tv_sec	= 0;			// Timeout in sec's
	timeout.tv_usec	= 0;			// Timeout in microsec's
	nr = select(0,&readSet,NULL,NULL,&timeout);
	return nr>0 && FD_ISSET(endpointRef,&readSet);
}

#define majorVrs 1
#define minorVrs 1

void StartUp(int abort)
{
	if (!tcpStartedUp)
		{
		WORD wVersionRequested; 
		WSADATA wsaData; 
		int err, four=4;
		SOCKET s;

		wVersionRequested = MAKEWORD(majorVrs, minorVrs); 
 		err = WSAStartup(wVersionRequested, &wsaData); 
 		if (err != 0) {
			if (abort) {
				rMessageBox(NULL, MB_APPLMODAL, "ERROR", "can't start up winsock"
							"\nprogram aborts");
				ExitProcess (255);
				}
			  else
				return;
			}

		/* Confirm that the Windows Sockets DLL supports version mj.mi.*/ 
		/* Note that if the DLL supports versions greater */ 
		/* than mj.mi in addition to mj.mi, it will still return */ 
		/* mj.mi in wVersion since that is the version we */ 
		/* requested. */ 
 
		if ( LOBYTE( wsaData.wVersion ) != majorVrs || 
			HIBYTE( wsaData.wVersion ) != minorVrs ) {
			WSACleanup(); 
			if (abort) {
				rMessageBox(NULL, MB_APPLMODAL, "ERROR", "winsock 1.1 or higher not available"
							"\nprogram aborts");
				ExitProcess (255);
				}
			  else
				return;
			};
		
		// initialize rcvBuffSize
		s = socket(PF_INET, SOCK_STREAM, 0);
		if (s==INVALID_SOCKET) {
			rMessageBox(NULL, MB_APPLMODAL, "ERROR", "can't create a socket"
						"\nprogram aborts");
			ExitProcess (255);
			};
		if (getsockopt(s, SOL_SOCKET, SO_RCVBUF, (char*) &rcvBuffSize, &four)) {
			rMessageBox(NULL, MB_APPLMODAL, "ERROR", "can't call getsockopt"
						"\nprogram aborts");
			ExitProcess (255);
			};
		
		pRcvBuff	= (CleanString) LocalAlloc(LMEM_FIXED, rcvBuffSize+4);
		if (pRcvBuff==NULL) {
			rMessageBox(NULL, MB_APPLMODAL, "ERROR", "out of memory"
						"\nprogram aborts");
			ExitProcess (255);
			};
		exit_tcpip_function	= CleanUp;
		tcpStartedUp	= TRUE;
		};
}
	
void lookupHost_syncC (CleanString inetAddr, int *errCode, int *ipAddrP)
// error code: 0 ok, 1 error (also: addr doesn't exist)
{
	HOSTENT *hostentP;
	unsigned long ipAddr;
	
	StartUp (TRUE);
	ipAddr	= inet_addr (CleanStringCharacters(inetAddr));
	if (ipAddr!=INADDR_NONE){
		*errCode	= 0;
		*ipAddrP	= ntohl(ipAddr);
		return;
	}
	*errCode = 1;
	hostentP = gethostbyname (CleanStringCharacters (inetAddr));	// string is alphanumerical 
	if (hostentP!=NULL){
		*ipAddrP = ntohl (((DWORD *)(*(hostentP->h_addr_list)))[0]);	
		if (*ipAddrP!=0)
		    *errCode = 0;
	}
}

void openTCP_ListenerC(int portNum, int *pErrCode, SOCKET *pEndpointRef)
// errCode: 0:ok;	otherwise:not ok
{
	SOCKET		s;
	SOCKADDR_IN	srvAdr;

	StartUp(TRUE);

	*pErrCode = 1;
	
	s = socket(PF_INET, SOCK_STREAM, 0);
	if (s==INVALID_SOCKET)
	  return;

	srvAdr.sin_family = AF_INET;			// of course internet adress family
	srvAdr.sin_addr.s_addr = INADDR_ANY;	// internet address will be given after "accept" 
	srvAdr.sin_port = htons((short int)portNum);
	
	*pErrCode = bind(s, (LPSOCKADDR) &srvAdr, sizeof(srvAdr));
	if (*pErrCode) {
		closesocket(s);
		return;
		};

	*pErrCode = listen(s,5);
	if (*pErrCode) {
		closesocket(s);
		return;
		};
	*pEndpointRef = s;

	*pErrCode	= insertNewDictionaryItem(s);
	if (*pErrCode)
		return;
	
	setEndpointData_no_new_notifiersC (s, 1,0,0,0);
}

void acceptC (SOCKET listener, int *pErrCode, int *pInetHost, SOCKET *pEndpointRef)
// errCode: 0:ok;	otherwise:not ok
{
	SOCKET		endpointRef;
	SOCKADDR_IN	clientAdr;
	int			clientAdrSize, tru;
	
	clientAdrSize	= sizeof(clientAdr);
	endpointRef		= accept(listener,(LPSOCKADDR) &clientAdr, &clientAdrSize);
	tru				= TRUE;
	ioctlsocket (endpointRef, FIONBIO, &tru);	// set mode to non blocking
	*pErrCode		= endpointRef==INVALID_SOCKET;
	if (*pErrCode)
		return;

	*pInetHost		= ntohl(clientAdr.sin_addr.s_addr);
	*pEndpointRef	= endpointRef;

	*pErrCode		= insertNewDictionaryItem(endpointRef);
	if (*pErrCode)
		return;

	setEndpointData_no_new_notifiersC (endpointRef,2,0,0,0);
}

void os_connectTCP_syncC (int onlyForMac, int doTimeout, unsigned int stopTime,
					 int ipAddr, int portnum,
					 int *errCodeP, int *timeoutExpiredP, size_t *endpointRefP)
// errCode: 0 ok;	1 not ok
{
	SOCKET client;
	SOCKADDR_IN	srvAdr,clientAdr;
	int err, tru;

	*errCodeP = 1;
	*timeoutExpiredP	= FALSE;
		
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

	if (doTimeout){
		ioctlsocket(client, FIONBIO, &tru);		// set mode to non blocking
		err = connect(client, (LPSOCKADDR) &srvAdr, sizeof(srvAdr));
		if (!err) {
			*errCodeP = 0;
			*timeoutExpiredP	= FALSE;
			*endpointRefP = client;
		} else if (WSAGetLastError()!=WSAEWOULDBLOCK) {
			closesocket(client);
			return;
		} else {
			FD_SET writeSet, exptnSet;
			TIMEVAL timeout;
			unsigned int	now;
			int				noOfWritableSockets, timeoutTicks;		
			FD_ZERO(&writeSet);
			FD_SET(client,&writeSet);
			FD_ZERO(&exptnSet);
			FD_SET(client,&exptnSet);

			now	= GetTickCount();
			timeoutTicks	= ((int)stopTime) - ((int)now);
			if (timeoutTicks<=0)
				{						// timeout expired
				closesocket(client);
				*timeoutExpiredP	= TRUE;
				return;
				};
			timeout.tv_sec	= timeoutTicks / 1000;			// Timeout in sec's
			timeout.tv_usec	= (timeoutTicks % 1000)*1000;	// Timeout in microsec's
			noOfWritableSockets = select(0,NULL,&writeSet,&exptnSet,&timeout);
			*errCodeP =		noOfWritableSockets<0
						||	(noOfWritableSockets>0 && FD_ISSET(client,&exptnSet));
			*timeoutExpiredP	= noOfWritableSockets==0;
			*endpointRefP		= client;
			if (*errCodeP || *timeoutExpiredP) {
				closesocket(client);
				return;
			}
		}
	}

	if (!doTimeout){
		err = connect(client, (LPSOCKADDR) &srvAdr, sizeof(srvAdr));
		if (err){
			closesocket(client);
			return;
		}

		ioctlsocket(client, FIONBIO, &tru);		// set mode to non blocking
		
		*errCodeP = 0;
		*timeoutExpiredP	= FALSE;
		*endpointRefP = client;
	}

	*errCodeP	= insertNewDictionaryItem(client);
	if (*errCodeP){
		closesocket(client);
		return;
	}

	setEndpointData_no_new_notifiersC (client,2,0,0,0);
}

void sendC (SOCKET endpointRef, CleanString data, int begin, int nBytes,
			int *pErrCode, int *pSentBytes)
{
	int sentBytes;

	*pErrCode	= 0;
	sentBytes = send (endpointRef, CleanStringCharacters(data)+begin,nBytes, 0);
	if (sentBytes==SOCKET_ERROR) {
		int err;
		sentBytes	= 0;
		err	= WSAGetLastError();
		if (err!=WSAEWOULDBLOCK) {
			dictitem	*pDictitem;
			pDictitem	= lookup(endpointRef);
			trace(	if (!pDictitem)
						rMessageBox(NULL, MB_APPLMODAL, "in sendC", "ERROR");)
			pDictitem->disconnected	=1;
			*pErrCode	= 1;
			}
		};
	*pSentBytes	= sentBytes;
}


void receiveC (SOCKET endpointRef, int maxSize, CleanString *pReceived)
{
	int			size, received;
	dictitem	*pDictitem;

	*pReceived	= (CleanString) pRcvBuff;
	size		= maxSize<=0 ? rcvBuffSize : maxSize;
	received	= recv (endpointRef, CleanStringCharacters(pRcvBuff), size, 0);
	pDictitem	= lookup(endpointRef);
	trace(	if (!pDictitem)
				rMessageBox(NULL, MB_APPLMODAL, "in receiveC", "ERROR");)
	if (received>0){
		pDictitem->availByteValid = 0;
		CleanStringLength(pRcvBuff)	= received;
	} else if (pDictitem->availByteValid) {
		CleanStringCharacters(pRcvBuff)[0]	= pDictitem->availByte;
		pDictitem->availByteValid = 0;
		CleanStringLength(pRcvBuff) = 1;
	} else
		CleanStringLength(pRcvBuff) = 0;
}

int data_availableC (SOCKET endpointRef)
{
	dictitem	*pDictitem;
	int			err;

	pDictitem	= lookup(endpointRef);
	trace(	if (!pDictitem)
				rMessageBox(NULL, MB_APPLMODAL, "in data_availableC", 
							"ERROR\nendpoint %i not found", endpointRef);)
	if (pDictitem->availByteValid)
		return TRUE;
	err	= recv(	endpointRef, &pDictitem->availByte, 1, MSG_PEEK);
	if (err>0){
		pDictitem->availByteValid = 1;
		return TRUE;
	}
	return FALSE;
}

void disconnectGracefulC (SOCKET endpointRef)
{
	shutdown (endpointRef,1);	// 1: graceful
}

void disconnectBrutalC (SOCKET endpointRef)
{
	LINGER	linger;
	linger.l_onoff	= 1;
	linger.l_linger	= 0;
	setsockopt (endpointRef, SOL_SOCKET, SO_LINGER, (char*) &linger, sizeof(linger));
}

void garbageCollectEndpointC (SOCKET endpointRef)
{
	dictitem	*pDictitem;

	pDictitem	= lookup (endpointRef);
	if (pDictitem!=NULL && pDictitem->referenceCount==0){
		closesocket(endpointRef);
		removeDictionaryItem(endpointRef);
	};
}

void initFD_SET(FD_SET **ppSet, SOCKET sockets[], int n)
{
	int	i;
	FD_SET	*pSet;

	pSet	= (FD_SET*) LocalAlloc(LMEM_FIXED | LMEM_ZEROINIT, n*sizeof(SOCKET)+sizeof(u_int));
	for(i=0; i<n; i++)
		FD_SET(sockets[i], pSet);
	*ppSet	= pSet;
}

void selectChC (int justForMac, int nonBlocking, int doTimeout, unsigned int stopTime, 
			    SOCKET *pRChannels, int *justForMac2, SOCKET *pSChannels,
				int *pErrCode)
// error code: 0=ok; 1=timeout expired, 3=other errors
{
	int				nRChannels, nSChannels, i;
	FD_SET			*pReadSet, *pWriteSet;
	TIMEVAL			timeout;
	unsigned int	now;
	int				n, timeoutTicks;		

	nRChannels	= (int) pRChannels[-2];
	nSChannels	= (int) pSChannels[-2];
	if (doTimeout){
		now	= GetTickCount();
		timeoutTicks	= nonBlocking ? 0 : ((int)stopTime) - ((int)now);
		if (timeoutTicks<0){
			*pErrCode	= 1;
			return;
		};
		timeout.tv_sec	= timeoutTicks / 1000;			// Timeout in sec's
		timeout.tv_usec	= (timeoutTicks % 1000)*1000;	// Timeout in microsec's
	}
	initFD_SET(&pReadSet,  pRChannels, nRChannels);
	initFD_SET(&pWriteSet, pSChannels, nSChannels);
	n = select (0,pReadSet,pWriteSet,NULL, doTimeout ? &timeout : NULL);
	if (n==0){
		// timeout expired
		*pErrCode	= 1;
		return;
	}
	if (n<0){
		*pErrCode	= 3;
		return;
	}
	for(i=0; i<nRChannels; i++)
		if (FD_ISSET(pRChannels[i], pReadSet))
			pRChannels[i]	= 0;
	for(i=0; i<nSChannels; i++)
		if (FD_ISSET(pSChannels[i], pWriteSet))
			pSChannels[i]	= 0;
	*pErrCode	= 0;
}

// this function is called from Cleans runtime system via *exit_tcpip_function
void CleanUp(void)
{
	dictitem	*pDictitem, *pTemp;
	int			referenceCount;
	pDictitem	= endpointDict;
	while (pDictitem) {
		referenceCount	= pDictitem->referenceCount;
		if (referenceCount!=0) {
			if (referenceCount==1 && pDictitem->aborted){
				disconnectBrutalC(pDictitem->endpointRef);
			} else
				disconnectGracefulC(pDictitem->endpointRef);
			closesocket(pDictitem->endpointRef);
			};
		pTemp	= pDictitem;
		pDictitem	= pDictitem->next;
		GlobalFree((char*) pTemp);
		};
	WSACleanup();
}


//------------------------ FUNCTION IMPLEMENTATIONS FOR THE ENDPOINT DICTIONARY -------

int insertNewDictionaryItem(SOCKET endpointRef)
{
	dictitem	*newItem			= (dictitem*) GlobalAlloc(LMEM_FIXED, sizeof(dictitem));
	if (newItem==NULL)
		return 1;
		
	newItem->endpointRef			= endpointRef;
	newItem->next					= endpointDict;
	newItem->availByteValid			= 0;
	newItem->aborted				= 0;
	newItem->disconnected			= 0;
	endpointDict					= newItem;

	return 0;
}

dictitem* lookup(SOCKET endpointRef)
{
	dictitem	*ptr=endpointDict;
	while (ptr!=NULL && (ptr->endpointRef!=endpointRef))
		ptr	= ptr->next;
	
	return ptr;
}

void setEndpointData_no_new_notifiersC
	(SOCKET endpointRef, int referenceCount, int hasReceiveNotifier, int hasSendableNotifier, int aborted)
{
	/*
	use only if no notifiers are added, because ghMainWindow_p_for_tcp
	and PM_SOCKET_EVENT_for_tcp may not be initialized
	*/
	dictitem	*ptr			= lookup (endpointRef);
	
	if (ptr!=NULL){
		ptr->referenceCount			= referenceCount;
		ptr->hasReceiveNotifier		= hasReceiveNotifier ? 1 : 0;
		ptr->hasSendableNotifier	= hasSendableNotifier ? 1 : 0;
		ptr->aborted				= aborted ? 1 : 0;
	};

	if (ghMainWindow_p_for_tcp!=NULL)
		WSAAsyncSelect (endpointRef, *ghMainWindow_p_for_tcp, PM_SOCKET_EVENT_for_tcp, 
							(hasReceiveNotifier ? FD_READ | FD_OOB | FD_ACCEPT | FD_CLOSE : 0)
						  |	(hasSendableNotifier ? FD_WRITE | FD_CLOSE : 0));
}

void getEndpointDataC (	SOCKET endpointRef, int *referenceCount,
						int *hasReceiveNotifier, int *hasSendableNotifier, int *aborted)
{
	dictitem	*ptr			= lookup (endpointRef);
	
	if (ptr!=NULL){
		*referenceCount			= ptr->referenceCount;
		*hasReceiveNotifier		= ptr->hasReceiveNotifier!=0;
		*hasSendableNotifier	= ptr->hasSendableNotifier!=0;
		*aborted				= ptr->aborted!=0;
	};
}

void removeDictionaryItem(SOCKET endpointRef)
// the dictionary MUST contain a valid item with the endpointRef
{
	dictitem	**ptr, *temp;
	int			notRemoved;

	ptr	= &endpointDict;
	notRemoved	= TRUE;
	while(notRemoved)
		if ((*ptr)->endpointRef==endpointRef)
			{
				temp	= *ptr;
				*ptr	= (*ptr)->next;
				GlobalFree((char*) temp);
				notRemoved	= FALSE;
			}
		  else
			ptr	= &((*ptr)->next);
}

