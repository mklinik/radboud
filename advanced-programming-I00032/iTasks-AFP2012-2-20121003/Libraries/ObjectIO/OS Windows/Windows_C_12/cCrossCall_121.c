/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	cCrossCall_121 defines the infrastructure required by the Object I/O library to call
	system procedures that interact with the Windows callback mechanism.

	The basic principle in cCrossCall_121 is to have a minimal cross call kernel. If Clean 
	code requires extension of the functionality of the OS thread, then this functionality 
	must be	registered before being applicable. 

	In this version the request codes are still statically fixed and are assumed to be 
	globally available both in the OS thread and the Clean thread. In a future version this 
	will probably be replaced by a dynamic allocation of cross call request codes.
********************************************************************************************/


/********************************************************************************************
	Include section.
********************************************************************************************/

#include "cCrossCall_121.h"
#include "cAcceleratorTable_121.h"					// Contains the implementation of accelerator tables.
#include "cCrossCallCursor_121.h"					// Contains the implementation of cursors.
#include <commctrl.h>


/**********************************************************************************************
	External global data section.
**********************************************************************************************/
CrossCallInfo gCci;									/* The global cross call information struct. */
char *gAppName;										/* The application name. */
HINSTANCE ghInst;									/* The handle to the instance of the OS thread. */
HWND ghMainWindow                = NULL;			/* The handle to the main HWND of the OS thread. */
HACCEL gAcceleratorTable         = NULL;			/* Refers to the accelerator table of the active frame. */
BOOL gAcceleratorTableIsUpToDate = TRUE;			/* Flag: TRUE iff accelerator table corresponds with active frame. */
HWND ghActiveFrameWindow         = NULL;			/* The currently active frame window (MDI/SDI). */
HWND ghActiveClientWindow        = NULL;			/* The currently active client window (MDI). */
HWND gActiveDialog               = NULL;			/* The currently active dialogue. */
HWND ghwndLastModalDialog        = NULL;			/* Keeps track of last modal dialog. */
HFONT gDlogFont                  = NULL;			/* The handle to the logical FONT that is used in dialogs. */
HFONT gControlFont;									/* The handle to the logical FONT that is used in all controls. */
HWND ghwndTT                     = NULL;			/* The tooltip control. */
HWND ghTCPWindow                 = NULL;			/* The handle to the TCP HWND of the OS thread. */
int gClipboardCount              = 0;				/* Keeps track of changes of clipboard. */
CrossCallProcedureTable gCrossCallProcedureTable;

/**********************************************************************************************
	Internal global data section.
**********************************************************************************************/
static BOOL gEventsInited        = FALSE;
static BOOL gAppRunning          = FALSE;
static HWND gNextClipboardViewer = NULL;		/* PA: stores next clipboardviewer. */
static LONG gPrevMsgTime         = -1;
static BOOL gMessStored          = FALSE;
static MSG gStoredMess;

static CrossCallInfo *MakeQuitCci (CrossCallInfo * pcci);


/*	Menu(item)IDs are not allowed to exceed OSMenuIDEnd.
	This is because window ids start at (OSMenuIDEnd+5), and need to be distinct from menu ids
	in case of MDI processes.
	The global gMenuItemID (initially 0) is incremented by NextMenuItemID each time a new 
	menu(item)ID is required.
	This implementation does not reuse freed ids and is therefore not adequate!!
*/
static int gMenuItemID = 0;

UINT NextMenuItemID (void)
{
	if (gMenuItemID>=OSMenuIDEnd)
		ErrorExit ("NextMenuItemID exceeded number of internal menu(item)IDs: %d",OSMenuIDEnd);

	gMenuItemID++;

	return gMenuItemID;
}


/*	GetModifiers returns the modifiers that are currently pressed.
*/
int GetModifiers (void)
{
	int mods;
	mods = 0;

	if (GetKeyState (VK_SHIFT) < 0)
		mods |= SHIFTBIT;
	if (GetKeyState (VK_CONTROL) < 0)
		mods |= CTRLBIT;
	if (GetKeyState (VK_MENU) < 0)
		mods |= ALTBIT;

	return mods;
}


/*	Translate virtual key codes to the codes shared with Clean.
	This procedure has been filtered from TranslateKeyboardMessage.
	If the keycode could not be translated, zero is returned.
*/
int CheckVirtualKeyCode (int keycode)
{
	int c = 0;
	switch (keycode)
	{
		case VK_UP:
			c = WinUpKey;
			break;
		case VK_DOWN:
			c = WinDownKey;
			break;
		case VK_LEFT:
			c = WinLeftKey;
			break;
		case VK_RIGHT:
			c = WinRightKey;
			break;
		case VK_PRIOR:
			c = WinPgUpKey;
			break;
		case VK_NEXT:
			c = WinPgDownKey;
			break;
		case VK_END:
			c = WinEndKey;
			break;
		case VK_HOME:
			c = WinBeginKey;
			break;
/* RWS
		case VK_BACK:
			c = WinBackSpKey;
			break;
*/
		case VK_DELETE:
			c = WinDelKey;
			break;
/*
		case VK_TAB:
			c = WinTabKey;
			break;
		case VK_RETURN:
			c = WinReturnKey;
			break;
		case VK_ESCAPE:
			c = WinEscapeKey;
			break;
*/
		case VK_HELP:
			c = WinHelpKey;
			break;
		case VK_F1:
			c = WinF1Key;
			break;
		case VK_F2:
			c = WinF2Key;
			break;
		case VK_F3:
			c = WinF3Key;
			break;
		case VK_F4:
			c = WinF4Key;
			break;
		case VK_F5:
			c = WinF5Key;
			break;
		case VK_F6:
			c = WinF6Key;
			break;
		case VK_F7:
			c = WinF7Key;
			break;
		case VK_F8:
			c = WinF8Key;
			break;
		case VK_F9:
			c = WinF9Key;
			break;
		case VK_F10:
			c = WinF10Key;
			break;
		case VK_F11:
			c = WinF11Key;
			break;
		case VK_F12:
			c = WinF12Key;
			break;
	}
	return c;
}


/*	EndSuspendTimerProc is the parameter of the SetTimer procedure used 
	in GetMessageQuickly to set a positive timer interval.
	The WaitMessage routine suspends the OS thread until an interesting
	event has occurred or the timer has been triggered. In that case
	EndSuspendTimerProc is called, which kills the timer and informs
	Clean about the timer event. 
*/
#ifdef _WIN64
static VOID CALLBACK EndSuspendTimerProc (HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime )
#else
static VOID CALLBACK EndSuspendTimerProc (HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime )
#endif
{
	KillTimer (ghMainWindow, (UINT) -2);
	SendMessage0ToClean (CcWmIDLETIMER);
};

static int GetMessageQuickly (BOOL gIdleTimerOn, int gSleeptime, MSG * pmsg)
{
	if (gMessStored)
	{
		*pmsg = gStoredMess;
		gMessStored = FALSE;
		return TRUE;
	}

	if (gSleeptime==0 && !PeekMessage (pmsg, NULL, 0, 0, PM_NOREMOVE))
	{
		POINT p;
		GetCursorPos (&p);
		pmsg->hwnd    = ghMainWindow;
		pmsg->message = WM_ENTERIDLE;
		pmsg->wParam  = MSGF_USER;
		pmsg->lParam  = (LPARAM) ghMainWindow;
		pmsg->time    = GetTickCount ();
		pmsg->pt      = p;
		return TRUE;
	}
	if (!gIdleTimerOn)
	{
		return GetMessage (pmsg, NULL, 0, 0);
	}
	else
	{
		if (PeekMessage (pmsg, NULL, 0, 0, PM_REMOVE))
			return (pmsg->message != WM_QUIT);
		else
		{
			POINT p;
			
			/*	The following code has been inserted to reduce the crosscall traffic.
				A timer is set to suspend this thread until atleast the timer interval
				has elapsed. 
			*/
#ifdef _WIN64
			if (SetTimer (ghMainWindow, (UINT_PTR) -2, (UINT)gSleeptime, &EndSuspendTimerProc))
#else
			if (SetTimer (ghMainWindow, (UINT) -2, (UINT)gSleeptime, &EndSuspendTimerProc))
#endif
				WaitMessage ();
			else
				rMessageBox (NULL,MB_APPLMODAL,"GetMessageQuickly","SetTimer failed to create timer");

			/*	End of insertion.
			*/

			GetCursorPos (&p);

			pmsg->hwnd    = ghMainWindow;
			pmsg->message = WM_ENTERIDLE;
			pmsg->wParam  = MSGF_USER;
			pmsg->lParam  = (LPARAM) ghMainWindow;
			pmsg->time    = GetTickCount ();
			pmsg->pt      = p;
			return TRUE;
		}
	}
}	/* GetMessageQuickly */



/*  PA: TranslateKeyboardMessage has been removed because it messes with the message queue.
        This makes it hard to memorize and keep correct. 
		Now all WindowProcedures handle virtual key codes themselves. 
//	TranslateKeyboardMessage is used by HandleCleanRequest only. 
static BOOL TranslateKeyboardMessage (MSG * pmsg)
{
	int c;
	UINT msg;

	c   = 0;
	msg = pmsg->message;
	if (msg==WM_SYSKEYDOWN || msg==WM_KEYDOWN)
	{
		c = CheckVirtualKeyCode ((int) pmsg->wParam);
	}
	if (c)
	{
		gStoredMess = *pmsg;
		gStoredMess.wParam = (WPARAM) c;

		if (pmsg->message == WM_SYSKEYDOWN)
			gStoredMess.message = WM_SYSCHAR;
		else
			gStoredMess.message = WM_CHAR;

		gMessStored = TRUE;
		return TRUE;
	}
	else
		return TranslateMessage (pmsg);
}	// TranslateKeyboardMessage
*/

void (*dispatch_null_message_hook) (MSG*)=NULL;

void HandleCleanRequest (CrossCallInfo * pcci)
{
	switch (pcci->mess)
	{
		case CcRqDOMESSAGE: 	// idleTimerOn, sleeptime; no result. 
			{
				MSG ms;
				int msgresult;

				msgresult  = GetMessageQuickly ((BOOL)pcci->p1, (int)pcci->p2, &ms);

				if (msgresult == -1)
					ErrorExit ("Fatal error: CcRqDoMessage, GetMessage result is -1");
				else if (msgresult == FALSE)
					ErrorExit ("Fatal error: CcRqDoMessage, GetMessage result is FALSE (WM_QUIT)");
				else
				{
					LONG msgtime = GetMessageTime ();

					gPrevMsgTime = msgtime;

					if (!gActiveDialog || !IsDialogMessage (gActiveDialog, &ms))
					{
						if (ghActiveClientWindow==NULL || !TranslateMDISysAccel (ghActiveClientWindow, &ms))
						{
							if (!gAcceleratorTableIsUpToDate)
							{	// Verify the correctness of gAcceleratorTable
								gAcceleratorTable = UpdateAcceleratorTable (gAcceleratorTable,ghActiveFrameWindow);
							}
						
							if (gAcceleratorTable==NULL || !TranslateAccelerator (ghActiveFrameWindow, gAcceleratorTable, &ms))
							{
								{	
									if (ms.hwnd==NULL && dispatch_null_message_hook!=NULL){
										(*dispatch_null_message_hook) (&ms);
									}

								//	TranslateKeyboardMessage (&ms);		// PA: confusing message manipulation removed
									TranslateMessage (&ms);				// PA: standard message loop code
									DispatchMessage  (&ms);
								}
							}
						}
					}
					MakeReturn0Cci (pcci);
				}
			}
			break;
		default:
			{
				CrossCallProcedure action;
				
				action = FindCrossCallEntry (gCrossCallProcedureTable, pcci->mess);

				if (action == NULL)
				{	// Cross call request code not installed.
					ErrorExit ("\'HandleCleanRequest\' got uninstalled CcRq request code from Clean: %d\n", pcci->mess);
				}
				else
				{	// Cross call request code found. Apply it to pcci.
					action (pcci);
				}
			}
	}
}	/* HandleCleanRequest */

void WinInitOs (Bool * ok, OS * os)
{
	if (gEventsInited)
	{
		*ok = FALSE;
		rprintf ("WIO: *ok = FALSE\n");
	}
	else
	{
		*ok = TRUE;
		gEventsInited = TRUE;
		rprintf ("WIO: *ok = TRUE\n");
	}

	*os = 54321;
}	/* WinInitOs */

Bool WinCloseOs (OS os)
{
	if (gEventsInited)
	{
		rprintf ("WCO: return TRUE\n");
		gEventsInited = FALSE;
		return TRUE;
	}
	else
	{
		rprintf ("WCO: return FALSE\n");
		return FALSE;
	}
}	/* WinCloseOs */

/*	InitGlobals is used by WinStartOSThread only. */
static void InitGlobals (void)
{
	LOGFONT lf;
	
	//	Globally, we create a logical font that is used in all controls. 
	SetLogFontData (&lf, "MS Sans Serif", 0, 8);
	gControlFont = CreateFontIndirect (&lf);

	//	The cross call procedure table is set to the empty table.
	gCrossCallProcedureTable = EmptyCrossCallProcedureTable ();
}	/* InitGlobals */

static DWORD InitWindowsAndMenu (void);

OS WinBeginOs (OS os)
{
	rprintf ("WSOT: Started\n");

	InitGlobals ();
	rprintf ("WSOT: Globals Initialised\n");

	InitWindowsAndMenu();

	return os;
}	/* WinBeginOs */

OS WinEndOs (OS os)
{
	rprintf ("OS WinKillOsThread (OS os)\n");

	/* CleanUp */
	if (gAcceleratorTable)
		DestroyAcceleratorTable (gAcceleratorTable);
	rprintf ("		if (gAcceleratorTable) \n");
	rprintf ("				DestroyAcceleratorTable( gAcceleratorTable );\n");
	DeleteCursors ();
	rprintf ("		DeleteCursors();\n");

	if (gDlogFont != NULL)
		DeleteObject (gDlogFont);
	rprintf ("		if(gDlogFont != NULL)\n");
	rprintf ("		   DeleteObject(gDlogFont);\n");

	DeleteObject (gControlFont);	// The global logical font must be deleted.
	if (gCrossCallProcedureTable)
		FreeCrossCallProcedureTable (gCrossCallProcedureTable);

// MW...
	ghMainWindow = NULL;
// ... MW

	return os;
}	/* WinEndOs */

OS os_toolbox;

void WinCallOsWithCallBack (int imess,
					  		size_t ip1, size_t ip2, size_t ip3,
					  		size_t ip4, size_t ip5, size_t ip6,
							OS ios,
						 	int *omess,
					 		size_t *op1, size_t *op2, size_t *op3,
					 		size_t *op4, size_t *op5, size_t *op6,
							OS * oos
					 		)
{
/*	rprintf("KOT: filling in Cci\n"); */
	rprintf ("WinCallOsWithCallBack (");
	printCCI (&gCci);
	rprintf (")\n");

	gCci.mess = imess;
	gCci.p1 = ip1;
	gCci.p2 = ip2;
	gCci.p3 = ip3;
	gCci.p4 = ip4;
	gCci.p5 = ip5;
	gCci.p6 = ip6;

	os_toolbox=ios;

	HandleCleanRequest (&gCci);

	*omess = gCci.mess;
	*op1 = gCci.p1;
	*op2 = gCci.p2;
	*op3 = gCci.p3;
	*op4 = gCci.p4;
	*op5 = gCci.p5;
	*op6 = gCci.p6;

	*oos=os_toolbox;
}	/* WinCallOsWithCallBack */

#define PRINTCROSSCALLS

#ifdef PRINTCROSSCALLS
static CrossCallInfo osstack[10];
static CrossCallInfo clstack[10];
static int ossp = -1;
static int clsp = -1;
#endif

extern void call_back_clean_object_io (int,size_t,size_t,size_t,size_t,size_t,size_t,OS,
										int*,size_t*,size_t*,size_t*,size_t*,size_t*,size_t*,OS*);

void SendMessageToClean (int mess, size_t p1, size_t p2, size_t p3, size_t p4, size_t p5, size_t p6)
{
	gCci.mess = mess;
	gCci.p1 = p1;
	gCci.p2 = p2;
	gCci.p3 = p3;
	gCci.p4 = p4;
	gCci.p5 = p5;
	gCci.p6 = p6;

#ifdef _WIN64
	{
	int new_mess;
	OS new_os_toolbox;

	call_back_clean_object_io (mess,p1,p2,p3,p4,p5,p6,os_toolbox,
							   &new_mess,&gCci.p1,&gCci.p2,&gCci.p3,&gCci.p4,&gCci.p5,&gCci.p6,&new_os_toolbox);
	gCci.mess=new_mess;
	os_toolbox=new_os_toolbox;
	}
#else
	call_back_clean_object_io (mess,p1,p2,p3,p4,p5,p6,os_toolbox,
							   &gCci.mess,&gCci.p1,&gCci.p2,&gCci.p3,&gCci.p4,&gCci.p5,&gCci.p6,&os_toolbox);
#endif
}

CrossCallInfo *MakeReturn0Cci (CrossCallInfo * pcci)
{
	pcci->mess = CcRETURN0;
	return pcci;
}

CrossCallInfo *MakeReturn1Cci (CrossCallInfo * pcci, size_t v1)
{
	pcci->mess = CcRETURN1;
	pcci->p1 = v1;
	return pcci;
}

CrossCallInfo *MakeReturn2Cci (CrossCallInfo * pcci, size_t v1, size_t v2)
{
	pcci->mess = CcRETURN2;
	pcci->p1 = v1;
	pcci->p2 = v2;
	return pcci;
}

CrossCallInfo *MakeReturn3Cci (CrossCallInfo * pcci, size_t v1, size_t v2, size_t v3)
{
	pcci->mess = CcRETURN3;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	return pcci;
}

CrossCallInfo *MakeReturn4Cci (CrossCallInfo * pcci, size_t v1, size_t v2, size_t v3, size_t v4)
{
	pcci->mess = CcRETURN4;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	return pcci;
}

CrossCallInfo *MakeReturn5Cci (CrossCallInfo * pcci, size_t v1, size_t v2, size_t v3, size_t v4, size_t v5)
{
	pcci->mess = CcRETURN5;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	return pcci;
}

CrossCallInfo *MakeReturn6Cci (CrossCallInfo * pcci, size_t v1, size_t v2, size_t v3, size_t v4, size_t v5, size_t v6)
{
	pcci->mess = CcRETURN6;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	pcci->p6 = v6;
	return pcci;
}

BOOL IsReturnCci (CrossCallInfo * pcci)
{
	if (pcci->mess >= CcRETURNmin && pcci->mess <= CcRETURNmax)
		return TRUE;

	return FALSE;
}


extern double c_div_real (double n, double d);
extern int c_ftoi (double d);

//	GetAppFileName used by OsThreadFunction only. 
static void GetAppFileName (void)
{
	char path[MAX_PATH + 1];
	int length, index;
	int start, end;
	BOOL newword;

	length = GetModuleFileName (NULL, path, MAX_PATH);

	for (index = length - 1; path[index] != '.'; index--)
		;
	end = index - 1;

	for (index = end;
		 path[index] != '/' &&
		 path[index] != '\\' &&
		 path[index] != ':';
		 index--)
		;

	start = index + 1;

	if (end - start > 31)
		end = start + 31;

	if (end - start >= 0)
	{
		gAppName = rmalloc (end - start + 2);
		for (index = 0; index <= end - start; index++)
			gAppName[index] = path[start + index];
		gAppName[index] = '\0';
	}
	else
	{
		gAppName = "Clean Application";
	}

	newword = TRUE;
	for (index = 0; gAppName[index] != '\0'; index++)
	{
		if (gAppName[index] >= 'A' && gAppName[index] <= 'Z' && !newword)
			gAppName[index] = gAppName[index] - ('A' - 'a');

		if (gAppName[index] == ' ')
			newword = TRUE;
		else
			newword = FALSE;
	}
}

/*	Registered Windows main window class name.
*/
static char MainWindowClassName[] = "__CleanMainWindow";	/* Class for main window (ghMainWindow). */


/*	The callback routine for the main window.
	PA: The WM_CREATE  message registers the main window as a clipboard viewer. 
		The WM_DESTROY message unregisters the main window. 
*/
static LRESULT CALLBACK MainWindowProcedure (HWND hWin, UINT uMess, WPARAM wPara, LPARAM lPara)
{
	printMessage ("Main Window", hWin, uMess, wPara, lPara);
	switch (uMess)
	{
		case WM_NCPAINT:
			break;
		case WM_ACTIVATEAPP:
			{
				if (wPara)
				{
					gAppRunning = TRUE;
				}
				else
				{
					gAppRunning = FALSE;
				}
			}
			break;
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;
				
				hwndModalDialog = (HWND)lPara;

				if (wPara == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(size_t)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return 0;
			}
			break;
		case WM_TIMER:
			{
				SendMessage2ToClean (CcWmTIMER, wPara, GetMessageTime ());
			}
			break;
		case WM_ENABLE:
			{
				HWND hwin;
				char title[64];

				hwin = GetWindow (ghMainWindow, GW_HWNDFIRST);
				while (hwin != NULL)
				{
					GetWindowText (hwin, title, 63);

					if (GetWindow (hwin, GW_OWNER) == ghMainWindow)
					{
						RECT r;
						GetWindowRect (hwin, &r);
						if (r.top != -1 || r.left != -1 || r.right != 0 || r.bottom != 0)
						{
							EnableWindow (hwin, (BOOL) wPara);
						}
					}
					hwin = GetWindow (hwin, GW_HWNDNEXT);
				}
			}
			break;
		/* The WM_CREATE message registers the ghMainWindow (hWin) as a clipboard viewer.
		*/
		case WM_CREATE:
			{
				gNextClipboardViewer = SetClipboardViewer (hWin);
			}
			break;
		/* The WM_DESTROY message unregisters the ghMainWindow (hWin) as a clipboard viewer.
		*/
		case WM_DESTROY:
			{
				ChangeClipboardChain (hWin, gNextClipboardViewer);
			}
			break;
		/* Other clipboard chain management messages:
		*/
		case WM_DRAWCLIPBOARD:
			{
				gClipboardCount += 1;
				if (gNextClipboardViewer != NULL)
					SendMessage (gNextClipboardViewer, uMess, wPara, lPara);

				return 0;
			}
			break;
		case WM_CHANGECBCHAIN:
			{
				if ((HWND)wPara == gNextClipboardViewer)	/* gNextClipboardViewer is no longer a clipboard viewer. */
					gNextClipboardViewer = (HWND)lPara;		/*	then store the new viewer. */
				else if (gNextClipboardViewer != NULL)
					SendMessage (gNextClipboardViewer, uMess, wPara, lPara);

				return 0;
			}
			break;
		/*	PM_SOCKET_EVENT and PM_DNS_EVENT are intercepted by MainWindowProcedure.
			If ghTCPWindow != NULL, then these messages are passed on to ghTCPWindow.
		*/
		case PM_SOCKET_EVENT:
		case PM_DNS_EVENT:
			{
				if (ghTCPWindow != NULL)
					SendMessage (ghTCPWindow, uMess, wPara, lPara);

				return 0;
			}
			break;
		case WM_DDE_INITIATE:
			{
				static char apptext[256], topictext[256];
				ATOM aApp, aTopic;
/* RWS ... */
				BOOL handleTopic;
/* ... RWS */
				GlobalGetAtomName (HIWORD (lPara), topictext, 256);

				if (lstrcmp (topictext, "CLEANOPEN") == 0)
/* RWS: compare application name */
				{
					GlobalGetAtomName (LOWORD (lPara), apptext, 256);
					handleTopic	= CompareStringA (LOCALE_USER_DEFAULT, NORM_IGNORECASE,
									apptext, lstrlen (apptext), gAppName, lstrlen (gAppName)) == 2;	/* 2 means they are equal */
				}
				else
					handleTopic	= FALSE;

				if (handleTopic)
				{
/* ... RWS */
					aApp = GlobalAddAtom (apptext);
					aTopic = GlobalAddAtom (topictext);
					SendMessage ((HWND) wPara, WM_DDE_ACK, (WPARAM) hWin, MAKELONG (aApp, aTopic));
					GlobalDeleteAtom (aApp);
					GlobalDeleteAtom (aTopic);
				}
				else
				{
					return DefWindowProc (hWin, uMess, wPara, lPara);
				}
			}
			break;
		case WM_DDE_EXECUTE:
			{
				char *commandstring;
				char *pcommand;
				int len;
				union
				{
					DDEACK ddeack;
					WORD w;
				}	da;

				pcommand = GlobalLock ((HANDLE) lPara);
				len = lstrlen (pcommand) + 1;
				commandstring = rmalloc (len);	/* this pointer is passed to and freed in the Clean code. */
				lstrcpyn (commandstring, pcommand, len);
				GlobalUnlock ((HANDLE) lPara);

				SendMessage1ToClean (CcWmDDEEXECUTE, commandstring);

				da.ddeack.bAppReturnCode = 0;
				da.ddeack.fBusy = 0;
				da.ddeack.fAck = 1;
				PostMessage ((HWND) wPara, WM_DDE_ACK, (WPARAM) hWin, PackDDElParam (WM_DDE_ACK, (UINT) da.w, lPara));
				return 0;
			}
			break;
		case WM_DDE_TERMINATE:
			{
				PostMessage ((HWND) wPara, WM_DDE_TERMINATE, (WPARAM) hWin, 0);
			} return 0;
		default:
			return DefWindowProc (hWin, uMess, wPara, lPara);
			break;
	}
	return 0;
}	/*	MainWindowProcedure */

static DWORD InitWindowsAndMenu (void)
{
	WNDCLASSEX wclass;
	int width, height;
	HMENU mainSystemMenu;

	/* register main window class */
	wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = CS_NOCLOSE;
	wclass.lpfnWndProc   = (WNDPROC) MainWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = NULL;
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = MainWindowClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);

	GetAppFileName ();

	width  =     GetSystemMetrics (SM_CXMAXIMIZED) - 2 * GetSystemMetrics (SM_CXSIZEFRAME);
	height = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION) + GetSystemMetrics (SM_CYMENU);

	ghMainWindow
		= CreateWindow (MainWindowClassName,	/* Class name					 */
						(LPCTSTR) gAppName, 	/* Window title 				 */
						WS_OVERLAPPEDWINDOW,	/* style flags					 */
						0, -5 - height,			/* x, y 						 */
						width, height,			/* width, height 				 */
						NULL,					/* Parent window				 */
						NULL,					/* menu handle					 */
						(HANDLE) ghInst,		/* Instance that owns the window */
						0);
	/*	Don't show the main window. This will result in one button less in the taskbar.
	ShowWindow (ghMainWindow, SW_SHOWNORMAL);
	*/
	/*	Before creating Clean controls, the tooltip control is created as the topmost child of this window. */
	ghwndTT = CreateWindowEx (	WS_EX_TOPMOST,					// Apply the topmost style for this window
								TOOLTIPS_CLASS,					// Class name
								NULL,							// Title (NULL)
								WS_POPUP | TTS_ALWAYSTIP,		// Style *must* be WS_POPUP
								CW_USEDEFAULT,					// Default position (x,y)
								CW_USEDEFAULT,
								CW_USEDEFAULT,					// Default size (w,h)
								CW_USEDEFAULT,
								ghMainWindow,					// Parent is the ghMainWindow
								(HMENU) NULL,					// No menu
								(HANDLE) ghInst,				// The instance
								NULL							// No window creation data
							 );
	
	mainSystemMenu = GetSystemMenu (ghMainWindow,FALSE);
	RemoveMenu (mainSystemMenu, SC_RESTORE,  MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MOVE,     MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_SIZE,     MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MINIMIZE, MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MAXIMIZE, MF_BYCOMMAND);
	DrawMenuBar (ghMainWindow);

	return 0;
}	/* OsThreadFunction */


PSTR WinGetAppPath (void)
{
	char *path;
	int idx, length;

	path = rmalloc (261);

	length = GetModuleFileName (NULL, path, 260);

	for (idx = length - 1;
		 path[idx] != '/' &&
		 path[idx] != '\\' &&
		 path[idx] != ':';
		 idx--)
		;

	path[idx + 1] = 0;

	return path;	/* relying on the calling clean function to de-allocate path. */
}	/* WinGetAppPath */

Bool WinFileExists (CLEAN_STRING name)
{
	char *file_name;
	HANDLE handle;
	WIN32_FIND_DATA find_data;

	file_name = cstring (name);

	handle = FindFirstFile (file_name, &find_data);

	if (handle != INVALID_HANDLE_VALUE)
	{
		FindClose (handle);

		return TRUE;
	}
	else
		return FALSE;
}	/* WinFileExists */

void WinCallProcess (PSTR commandline,
					 PSTR env,
					 PSTR dir,
					 PSTR in,
					 PSTR out,
					 PSTR err,
					 OS ios,
					 Bool * success,
					 int *exitcode,
					 OS * oos
				    )
{
	SECURITY_ATTRIBUTES sa;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;
	char *ep;
	HANDLE infile;
	HANDLE outfile;
	HANDLE errfile;

	rprintf ("WCP: starting...\n");

	rprintf ("success = %d, value = %d\n", success, *success);
	*success = FALSE;
	rprintf ("exitcode = %d; value = %d\n", exitcode, *exitcode);
	*exitcode = -1;
	rprintf ("oos = %d; value = %d\n", oos, *oos);
	*oos = ios;

	rprintf ("commandline = %d; value = %s\n", commandline, commandline);

	if (commandline != NULL)
		rprintf ("WCP: commandline = \"%s\"\n", commandline);
	else
		rprintf ("WCP: commandline empty\n");

	if (env != NULL)
	{
		ep = env;
		while (rstrlen (ep) != 0)
		{
			rprintf ("WCP:		   env = \"%s\"\n", ep);
			ep += rstrlen (ep) + 1;
		}
	}
	else
		rprintf ("WCP:		   env empty\n");

	if (dir != NULL)
		rprintf ("WCP: dir = \"%s\"\n", dir);
	else
		rprintf ("WCP: dir empty\n");

	if (in != NULL)
		rprintf ("WCP: in = \"%s\"\n", in);
	else
		rprintf ("WCP: in empty\n");

	if (out != NULL)
		rprintf ("WCP: out = \"%s\"\n", out);
	else
		rprintf ("WCP: out empty\n");

	if (err != NULL)
		rprintf ("WCP: err = \"%s\"\n", err);
	else
		rprintf ("WCP: err empty\n");

	sa.nLength = sizeof (SECURITY_ATTRIBUTES);
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

	if (in != NULL)
	{
		infile = CreateFile (in,
							 GENERIC_READ,
							 FILE_SHARE_READ | FILE_SHARE_WRITE,
							 &sa,
							 OPEN_EXISTING,
							 FILE_ATTRIBUTE_NORMAL,
							 NULL
			);
		if (infile == INVALID_HANDLE_VALUE)
		{
			rprintf ("infile creation failed\n");
			return;
		}
		rprintf ("redirection of input ok\n");
	}
	else
	{
		rprintf ("in == NULL\n");
		infile = GetStdHandle(STD_INPUT_HANDLE);
	}

	if (out != NULL)
	{
		outfile = CreateFile (out,
							  GENERIC_WRITE,
							  0,
							  &sa,
							  CREATE_ALWAYS,
							  FILE_ATTRIBUTE_NORMAL,
							  NULL
			);
		if (outfile == INVALID_HANDLE_VALUE)
		{
			rprintf ("outfile creation failed\n");
			return;
		}
		rprintf ("redirection of output ok\n");

	}
	else
	{
		rprintf ("out == NULL\n");
		outfile = GetStdHandle(STD_OUTPUT_HANDLE);
	}

	if (err != NULL)
	{
		errfile = CreateFile (err,
							  GENERIC_WRITE,
							  0,
							  &sa,
							  CREATE_ALWAYS,
							  FILE_ATTRIBUTE_NORMAL,
							  NULL
			);
		if (errfile == INVALID_HANDLE_VALUE)
		{
			rprintf ("errfile creation failed\n");
			return;
		}

		rprintf ("redirection of errors ok\n");
	}
	else
	{
		rprintf ("err == NULL\n");
		errfile = GetStdHandle(STD_ERROR_HANDLE);
	}

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = STARTF_USESTDHANDLES;
	si.hStdInput = infile;
	si.hStdOutput = outfile;
	si.hStdError = errfile;

	fsuccess =
		CreateProcess (NULL,				/* pointer to name of executable module		*/
					   commandline,			/* pointer to command line string			*/
					   NULL,				/* pointer to process security attributes	*/
					   NULL,				/* pointer to thread security attributes	*/
					   TRUE,				/* handle inheritance flag					*/
					   DETACHED_PROCESS,	/* creation flags							*/
					   env,					/* pointer to new environment block			*/
					   dir,					/* pointer to current directory name		*/
					   &si,					/* pointer to STARTUPINFO					*/
					   &pi					/* pointer to PROCESS_INFORMATION			*/
		);
	if (fsuccess)
	{
		rprintf ("WCP: success\n");
		WaitForSingleObject (pi.hProcess, INFINITE);
		GetExitCodeProcess (pi.hProcess, (unsigned long *) exitcode);
		rprintf ("WCP: exitcode = %d\n", *exitcode);
		*success = TRUE;
	}
	else
	{
		rprintf ("WCP: failure %d\n", (int) GetLastError ());
		*success = FALSE;
		*exitcode = -1;
	}

	if (in != NULL && infile != NULL)
	{
		if (CloseHandle (infile))
			rprintf ("closing infile ok\n");
		else
			rprintf ("closing infile failed\n");
	}
	else
		rprintf ("no need to close and reset input\n");

	if (out != NULL && outfile != NULL)
	{
		if (CloseHandle (outfile))
			rprintf ("closing outfile ok\n");
		else
			rprintf ("closing outfile failed\n");
	}
	else
		rprintf ("no need to close and reset output\n");

	if (err != NULL && errfile != NULL)
	{
		if (CloseHandle (errfile))
			rprintf ("closing errfile ok\n");
		else
			rprintf ("closing errfile failed\n");
	}
	else
		rprintf ("no need to close and reset errput\n");

	rprintf ("WCP: returning\n");
	/* *oos = ios; */
}	/* WinCallProcess */

void WinLaunchApp (CLEAN_STRING commandline, BOOL console, OS ios, Bool * success, OS * oos)
{
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;
	char path[_MAX_PATH];
	char *cl, *thepath;
	int i;
	DWORD error;

	rprintf ("WLA: starting...\n");

	*success = FALSE;
	*oos = ios;
	si.lpTitle = NULL;

	rprintf ("WLA: step 2.\n");

	cl = cstring (commandline);
	lstrcpy (path, cl);

	for (i = lstrlen (path); path[i] != '\\' && i >= 0; i--)
		path[i] = 0;

	if (i == 0)
		thepath = NULL;
	else
	{	/* path[i] = '\"'; */
		thepath = path + 1;
	}

	rprintf ("WLA: step 2a: directory = <%s>\n", thepath);

	rprintf ("WLA: step 3: filling in si.\n");

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.dwFlags = 0;
	si.lpTitle = NULL;

	rprintf ("WLA: step 4: calling process \"%s\".\n", cl);

	fsuccess =
		CreateProcess (NULL,	/* pointer to name of executable module		*/
					   cl,		/* pointer to command line string			*/
					   NULL,	/* pointer to process security attributes	*/
					   NULL,	/* pointer to thread security attributes	*/
					   TRUE,	/* handle inheritance flag					*/
					   0,		/* creation flags							*/
					   NULL,	/* pointer to new environment block			*/
					   thepath, /* pointer to current directory name		*/
					   &si, 	/* pointer to STARTUPINFO					*/
					   &pi		/* pointer to PROCESS_INFORMATION			*/
		);
	error = GetLastError ();
	if (fsuccess)
	{
		rprintf ("WLA: success\n");
	}
	else
	{
		rprintf ("WLA: failure %d\n", error);
	}

	rprintf ("WLA: step 5: returning\n");
	*success = fsuccess;
	*oos = ios;
	rprintf ("WLA: done...\n");
}	/* WinLaunchApp */

/*	New version of WinLaunchApp. In addition to the commandline, it gets a directory argument.
*/
void WinLaunchApp2 (CLEAN_STRING commandline, CLEAN_STRING pathname, BOOL console, OS ios, Bool * success, OS * oos)
{
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;
	char commandline2[_MAX_PATH];
	char *cl, *thepath;
	DWORD error;

	rprintf ("WLA2: starting...\n");

	*success = FALSE;
	*oos = ios;
	si.lpTitle = NULL;

	rprintf ("WLA2: step 2.\n");

	cl = cstring (commandline);
	lstrcpy (commandline2, cl);
	thepath = cstring (pathname);

	rprintf ("WLA2: step 3: filling in si.\n");

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.dwFlags = 0;
	si.lpTitle = NULL;

	rprintf ("WLA2: step 4: calling process \"%s\".\n", cl);

	fsuccess =
		CreateProcess (NULL,		/* pointer to name of executable module		*/
					   commandline2,/* pointer to command line string			*/
					   NULL,		/* pointer to process security attributes	*/
					   NULL,		/* pointer to thread security attributes	*/
					   TRUE,		/* handle inheritance flag					*/
					   0,			/* creation flags							*/
					   NULL,		/* pointer to new environment block			*/
					   thepath,		/* pointer to current directory name		*/
					   &si,			/* pointer to STARTUPINFO					*/
					   &pi			/* pointer to PROCESS_INFORMATION			*/
		);
	error = GetLastError ();
	if (fsuccess)
	{
		rprintf ("WLA2: success\n");
	}
	else
	{
		rprintf ("WLA2: failure %d\n", error);
	}

	rprintf ("WLA2: step 5: returning\n");
	*success = fsuccess;
	*oos = ios;
	rprintf ("WLA2: done...\n");
}	/* WinLaunchApp2 */


CLEAN_STRING WinGetModulePath (void)
{
	char path[MAX_PATH + 1];

	GetModuleFileName (NULL, path, MAX_PATH);

	return cleanstring (path);
}	/* WinGetModulePath */

void WinFileModifiedDate (CLEAN_STRING name, Bool *exists, int *yy, int *mm, int *dd, int *h, int *m, int *s)
{
	char *file_name;
	HANDLE handle;
	WIN32_FIND_DATA find_data;

	file_name = cstring (name);

	handle = FindFirstFile (file_name, &find_data);

	if (handle != INVALID_HANDLE_VALUE)
	{
		SYSTEMTIME system_time;

		FindClose (handle);

		if (FileTimeToSystemTime (&find_data.ftLastWriteTime, &system_time))
		{
			*exists = TRUE;
			*yy = system_time.wYear;
			*mm = system_time.wMonth;
			*dd = system_time.wDay;
			*h = system_time.wHour;
			*m = system_time.wMinute;
			*s = system_time.wSecond;
			return;
		}
	}

	*exists = FALSE;
	*yy = 0;
	*mm = 0;
	*dd = 0;
	*h = 0;
	*m = 0;
	*s = 0;
}	/* WinFileModifiedDate */
