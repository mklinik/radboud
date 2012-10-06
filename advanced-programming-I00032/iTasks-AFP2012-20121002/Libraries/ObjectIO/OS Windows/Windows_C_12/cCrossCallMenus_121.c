/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to menu handling.
********************************************************************************************/
#include "cCrossCallMenus_121.h"
#include "cAcceleratorTable_121.h"
#include "cCrossCall_121.h"


OS WinDestroyMenu (HMENU menu, OS os)
{
/*	rprintf ("DestroyMenu: menu = %d\n", (int) menu); */

	DestroyMenu (menu);
	return (os);
}

/*	Cross call procedure implementations.
	Eval<nr> corresponds with a CrossCallEntry generated by NewCrossCallEntry (nr,Eval<nr>).
*/
/*	Add a shortkey to a framewindow shortkey table. */
void EvalCcRqADDMENUSHORTKEY (CrossCallInfo *pcci)	/* frameptr, cmd, key; no result. */
{
	ProcessShortcutTable table;
	HWND frameptr;
	int cmd, key;

	frameptr = (HWND) pcci->p1;
	cmd      = pcci->p2;
	key      = pcci->p3;

#ifdef _WIN64
	table = (ProcessShortcutTable) GetWindowLongPtr (frameptr,0);
#else
	table = (ProcessShortcutTable) GetWindowLong (frameptr,0);
#endif
	table = AddProcessShortcut (key, cmd, table);
#ifdef _WIN64
	SetWindowLongPtr (frameptr, 0, (LONG_PTR)table);
#else
	SetWindowLong (frameptr, 0, (long)table);
#endif

	if (gAcceleratorTableIsUpToDate)
	{
		gAcceleratorTableIsUpToDate = !(ghActiveFrameWindow==frameptr);
	}

	MakeReturn0Cci (pcci);
}

/*	Remove a shortkey from a framewindow shortkey table. */
void EvalCcRqREMOVEMENUSHORTKEY (CrossCallInfo *pcci)	/* frameptr, cmd; no result. */
{
	ProcessShortcutTable table;
	HWND frameptr;
	int cmd;

	frameptr = (HWND) pcci->p1;
	cmd      = pcci->p2;

#ifdef _WIN64
	table    = (ProcessShortcutTable) GetWindowLongPtr (frameptr,0);
#else
	table    = (ProcessShortcutTable) GetWindowLong (frameptr,0);
#endif
	table    = RemoveProcessShortcut (cmd, table);
#ifdef _WIN64
	SetWindowLongPtr (frameptr, 0, (LONG_PTR)table);
#else
	SetWindowLong (frameptr, 0, (long)table);
#endif

	if (gAcceleratorTableIsUpToDate)
	{
		gAcceleratorTableIsUpToDate = !(ghActiveFrameWindow==frameptr);
	}

	MakeReturn0Cci (pcci);
}

void EvalCcRqMODIFYMENUITEM (CrossCallInfo *pcci)	/* hitem, hmenu, textptr; no result.	*/
{
	MENUITEMINFO info;

	info.cbSize        = (UINT) sizeof (MENUITEMINFO);
	info.fMask         = MIIM_TYPE;
	info.fType         = MFT_STRING;
	info.fState        = (UINT) 0;
	info.wID           = (UINT) pcci->p1;
	info.hSubMenu      = (HMENU) NULL;
	info.hbmpChecked   = NULL;
	info.hbmpUnchecked = NULL;
	info.dwItemData    = (DWORD) 0;
	info.dwTypeData    = (LPTSTR) pcci->p3;//correcttext;
	info.cch           = rstrlen ((LPTSTR)pcci->p3);//(correcttext);

	SetMenuItemInfo ((HMENU) pcci->p2, (UINT) pcci->p1, FALSE, &info);

	MakeReturn0Cci (pcci);
}

void EvalCcRqINSERTMENUITEM (CrossCallInfo *pcci)		/* on/off, hmenu, textptr, marked, pos;    HITEM result. */
{
	UINT graystate, checkstate;
	HITEM hitem;

	if (pcci->p1)
	{
		graystate = MF_ENABLED;
	}
	else
	{
		graystate = MF_GRAYED;
	}

	if (pcci->p4)
	{
		checkstate = MF_CHECKED;
	}
	else
	{
		checkstate = MF_UNCHECKED;
	}

	hitem = NextMenuItemID ();		/*	PA: replaced NextItemHandle by NextMenuItemID. */

	InsertMenu ((HMENU) pcci->p2,			/* hMenu	  */
				(UINT) pcci->p5,			/* position   */
				MF_BYPOSITION | MF_STRING | graystate | checkstate,
											/* Flags	  */
				(UINT) hitem,				/* id		  */
				(LPCTSTR) pcci->p3
		);
	MakeReturn1Cci (pcci, hitem);
}

void EvalCcRqITEMENABLE (CrossCallInfo *pcci)	/* parent, HITEM, onoff; no result.  */
{
	UINT greystate;

	if (pcci->p3)
	{
		greystate = MF_ENABLED;
	}
	else
	{
		greystate = MF_GRAYED;
	}
	EnableMenuItem ((HMENU) pcci->p1,			/* parent menu	*/
					(UINT) pcci->p2,			/* menu item id */
					MF_BYCOMMAND | greystate);	/* flags		*/
	MakeReturn0Cci (pcci);
}

/*	Destroy a menu 'physically' */
void EvalCcRqDESTROYMENU (CrossCallInfo *pcci)			/* HMENU; no result. */
{
	DestroyMenu ((HMENU) pcci->p1);
	MakeReturn0Cci (pcci);
}

/*	Remove a menu logically */
void EvalCcRqDELETEMENU (CrossCallInfo *pcci)			/* HMENU, HITEM; no result. */
{
	DeleteMenu ((HMENU) pcci->p1,		/* parent menu  */
				(UINT) pcci->p2,		/* menu item id */
				MF_BYCOMMAND);
	MakeReturn0Cci (pcci);
}

void EvalCcRqREMOVEMENUITEM (CrossCallInfo *pcci)		/* menu, HITEM; no result. */
{
	RemoveMenu ((HMENU) pcci->p1,
				(UINT) pcci->p2,
				MF_BYCOMMAND);
	MakeReturn0Cci (pcci);
}

void EvalCcRqINSERTSEPARATOR (CrossCallInfo *pcci)		/* hmenu, pos no result. */
{
	InsertMenu ((HMENU) pcci->p1,				/* hMenu				 */
				(UINT) pcci->p2,				/* position 			 */
				MF_BYPOSITION | MF_SEPARATOR,	/* Flags				 */
				0,								/* no id for separator	 */
				NULL);							/* no text for separator */
	MakeReturn0Cci (pcci);
}

void EvalCcRqMODIFYMENU (CrossCallInfo *pcci)	/* hitem, hmenu, textptr; no result.	*/
{
	MENUITEMINFO info;

	info.cbSize        = (UINT) sizeof (MENUITEMINFO);
	info.fMask         = MIIM_TYPE;
	info.fType         = MFT_STRING;
	info.fState        = (UINT) 0;
	info.wID           = (UINT) pcci->p1;
	info.hSubMenu      = (HMENU) pcci->p2;
	info.hbmpChecked   = NULL;
	info.hbmpUnchecked = NULL;
	info.dwItemData    = (DWORD) 0;
	info.dwTypeData    = (LPTSTR) pcci->p3;
	info.cch           = rstrlen ((LPTSTR)pcci->p3);

	SetMenuItemInfo ((HMENU) pcci->p2, (UINT) pcci->p1, FALSE, &info);

	MakeReturn0Cci (pcci);
}

/*	Insert a menu into the menu bar. */
void EvalCcRqINSERTMENU (CrossCallInfo *pcci)	/* on/off, hmenu, textptr, hsubmenu, pos; no result */
{
	UINT graystate;

	graystate = 0;
	if (pcci->p1)
	{
		graystate = MF_ENABLED;
	}
	else
	{
		graystate = MF_GRAYED;
	}

	InsertMenu ((HMENU) pcci->p2,						/* hMenu		*/
				(UINT) pcci->p5,						/* position		*/
				MF_BYPOSITION | MF_POPUP | graystate,	/* flags		*/
				(UINT) pcci->p4,						/* hSubMenu		*/
				(LPCTSTR) pcci->p3
		);
	MakeReturn0Cci (pcci);
}

void EvalCcRqMENUENABLE (CrossCallInfo *pcci)	/* parent, zero based position of menu, onoff; no result. */
{
	UINT greystate;

	if (pcci->p3)
	{
		greystate = MF_ENABLED;
	}
	else
	{
		greystate = MF_GRAYED;
	}
	EnableMenuItem ((HMENU) pcci->p1,			/* parent menu	*/
					(UINT) pcci->p2,			/* submenu	 	*/
					MF_BYPOSITION | greystate);	/* flags		*/
	MakeReturn0Cci (pcci);
}

void EvalCcRqDRAWMBAR (CrossCallInfo *pcci)		/* framePtr, clientPtr; no result. */
{
	HWND framePtr  = (HWND) pcci->p1;
	HWND clientPtr = (HWND) pcci->p2;

	if (clientPtr != 0)
		SendMessage (clientPtr,WM_MDIREFRESHMENU,(WPARAM)0,(LPARAM)0);
	
	DrawMenuBar (framePtr);
	MakeReturn0Cci (pcci);
}

/*	Track pop up menu. */
/*	This version returned BOOL. New version must return number of selected item.
void EvalCcRqTRACKPOPMENU (CrossCallInfo *pcci)	// popupmenu,framePtr; BOOL result. 
{
	HMENU popupmenu;
	HWND  framePtr;
	UINT  flags;
	POINT mousePos;
	BOOL  ok;

	popupmenu = (HMENU) pcci->p1;
	framePtr  = (HWND)  pcci->p2;
	flags     = TPM_CENTERALIGN | TPM_RIGHTBUTTON;
	GetCursorPos (&mousePos);

	ok = TrackPopupMenu (popupmenu,flags,mousePos.x,mousePos.y,0,framePtr,NULL);

	MakeReturn1Cci (pcci,(int)ok);
}
*/
void EvalCcRqTRACKPOPMENU (CrossCallInfo *pcci)	/* popupmenu,framePtr; return: index of selected item; modifiers. */
{
	HMENU popupmenu;
	HWND  framePtr;
	UINT  flags;
	POINT mousePos;
	int   menuItemID;

	popupmenu = (HMENU) pcci->p1;
	framePtr  = (HWND)  pcci->p2;
	flags     = TPM_CENTERALIGN | TPM_RIGHTBUTTON | TPM_NONOTIFY | TPM_RETURNCMD;
	GetCursorPos (&mousePos);

	menuItemID = (int)TrackPopupMenu (popupmenu,flags,mousePos.x,mousePos.y,0,framePtr,NULL);

	MakeReturn2Cci (pcci,menuItemID, GetModifiers());
}

void EvalCcRqCREATEPOPMENU (CrossCallInfo *pcci) /* no params; MENU result.   */
{
	MakeReturn1Cci (pcci, (size_t) CreatePopupMenu ());
}

void EvalCcRqCHECKMENUITEM (CrossCallInfo *pcci) /* menu, HITEM, on/off; no result.	*/
{
	UINT check;
	if (pcci->p3)
	{
		check = MF_CHECKED;
	}
	else
	{
		check = MF_UNCHECKED;
	}
	CheckMenuItem ((HMENU) pcci->p1, (UINT) pcci->p2, MF_BYCOMMAND | check);
	MakeReturn0Cci (pcci);
}


/*	Install the cross call procedures in the gCrossCallProcedureTable of cCrossCall_121.
*/
int InstallCrossCallMenus (int ios)
{
	CrossCallProcedureTable newTable;

	newTable = EmptyCrossCallProcedureTable ();
	AddCrossCallEntry (newTable, CcRqADDMENUSHORTKEY,    EvalCcRqADDMENUSHORTKEY);
	AddCrossCallEntry (newTable, CcRqREMOVEMENUSHORTKEY, EvalCcRqREMOVEMENUSHORTKEY);
	AddCrossCallEntry (newTable, CcRqMODIFYMENUITEM,     EvalCcRqMODIFYMENUITEM);
	AddCrossCallEntry (newTable, CcRqINSERTMENUITEM,     EvalCcRqINSERTMENUITEM);
	AddCrossCallEntry (newTable, CcRqITEMENABLE,         EvalCcRqITEMENABLE);
	AddCrossCallEntry (newTable, CcRqDESTROYMENU,        EvalCcRqDESTROYMENU);
	AddCrossCallEntry (newTable, CcRqDELETEMENU,         EvalCcRqDELETEMENU);
	AddCrossCallEntry (newTable, CcRqREMOVEMENUITEM,     EvalCcRqREMOVEMENUITEM);
	AddCrossCallEntry (newTable, CcRqINSERTSEPARATOR,    EvalCcRqINSERTSEPARATOR);
	AddCrossCallEntry (newTable, CcRqMODIFYMENU,         EvalCcRqMODIFYMENU);
	AddCrossCallEntry (newTable, CcRqINSERTMENU,         EvalCcRqINSERTMENU);
	AddCrossCallEntry (newTable, CcRqMENUENABLE,         EvalCcRqMENUENABLE);
	AddCrossCallEntry (newTable, CcRqDRAWMBAR,           EvalCcRqDRAWMBAR);
	AddCrossCallEntry (newTable, CcRqTRACKPOPMENU,       EvalCcRqTRACKPOPMENU);
	AddCrossCallEntry (newTable, CcRqCREATEPOPMENU,      EvalCcRqCREATEPOPMENU);
	AddCrossCallEntry (newTable, CcRqCHECKMENUITEM,      EvalCcRqCHECKMENUITEM);
	AddCrossCallEntries (gCrossCallProcedureTable, newTable);

	return ios;
}