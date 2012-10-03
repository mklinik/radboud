/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to window/dialog handling. 
********************************************************************************************/
#include "cCCallWindows_121.h"

int WinInvalidateWindow (size_t hwnd, int ios)
{
	InvalidateRect ((HWND) hwnd, NULL, FALSE);
	return ios;
}

int WinInvalidateRect (size_t hwnd, int left, int top, int right, int bottom, int ios)
{
	RECT rect;

	rect.left   = left;
	rect.top    = top;
	rect.right  = right;
	rect.bottom = bottom;
	InvalidateRect ((HWND) hwnd, &rect, FALSE);
	return ios;
}

int WinValidateRect (size_t hwnd, int left, int top, int right, int bottom, int ios)
{
	RECT rect;

	rect.left   = left;
	rect.top    = top;
	rect.right  = right;
	rect.bottom = bottom;
	ValidateRect ((HWND) hwnd, &rect);
	return ios;
}

int WinValidateRgn (size_t hwnd, size_t rgn, int ios)
{
	ValidateRgn ((HWND) hwnd, (HRGN) rgn);
	return ios;
}

/*	Win(M/S)DIClientToOuterSizeDims returns the width and height needed to add/subtract
	from the client/outer size to obtain the outer/client size. 
	These values must be the same as used by W95AdjustClean(M/S)DIWindowDimensions!
*/
void WinMDIClientToOuterSizeDims (int styleFlags, OS ios, int *dw, int *dh, OS *oos)
{
	if ((styleFlags&WS_THICKFRAME) != 0)
	{	/* resizable window */
		*dw = 2 * GetSystemMetrics (SM_CXSIZEFRAME);
		*dh = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
	} else
	{	/* fixed size window */
		*dw = 2 * GetSystemMetrics (SM_CXFIXEDFRAME);
		*dh = 2 * GetSystemMetrics (SM_CYFIXEDFRAME) + GetSystemMetrics (SM_CYCAPTION);
	}

	*oos = ios;
}

void WinSDIClientToOuterSizeDims (int styleFlags, OS ios, int *dw, int *dh, OS *oos)
{
	*dw = 2 * GetSystemMetrics (SM_CXSIZEFRAME);
	*dh = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);

	*oos = ios;
}

/*	Adjust the dimensions of a Clean MDI document window with W95 frame dimensions. 
	These values must be the same as returned by WinMDIClientToOuterSizeDims!
*/
void W95AdjustCleanMDIWindowDimensions (DWORD styleFlags, POINT * dim)
{
	if ((styleFlags&WS_THICKFRAME) != 0)
	{	/* resizable window */
		dim->x += 2 * GetSystemMetrics (SM_CXSIZEFRAME);
		dim->y += 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
	} else
	{	/* fixed size window */
		dim->x += 2 * GetSystemMetrics (SM_CXFIXEDFRAME);
		dim->y += 2 * GetSystemMetrics (SM_CYFIXEDFRAME) + GetSystemMetrics (SM_CYCAPTION);
	}
}

/*	Adjust the dimensions of a Clean SDI frame window with W95 frame and menu dimensions.
	These values must be the same as returned by WinSDIClientToOuterSizeDims!
*/
void W95AdjustCleanSDIWindowDimensions (DWORD styleFlags, POINT * dim)
{
	dim->x += 2 * GetSystemMetrics (SM_CXSIZEFRAME);
	dim->y += 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
}


/*	Set and get the GWL_USERDATA of a windowhandle.
*/
#ifdef _WIN64
void SetGWLP_USERDATA (LONG_PTR data, HWND hwnd)
{
	SetWindowLongPtr (hwnd, GWLP_USERDATA, data);
}
#else
void SetGWL_USERDATA (LONG data, HWND hwnd)
{
	SetWindowLong (hwnd, GWL_USERDATA, data);
}
#endif

#ifdef _WIN64
LONG_PTR GetGWLP_USERDATA (HWND hwnd)
{
	return GetWindowLongPtr (hwnd, GWLP_USERDATA);
}
#else
LONG GetGWL_USERDATA (HWND hwnd)
{
	return GetWindowLong (hwnd, GWL_USERDATA);
}
#endif

/*	UpdateWindowScrollbars updates any window scrollbars and non-client area if present.
	Uses the following access procedures to the GWL_STYLE of a windowhandle:
		GetGWL_STYLE (hwnd) returns the GWL_STYLE value of hwnd;
		WindowHasHScroll (hwnd) returns TRUE iff hwnd has a horizontal scrollbar;
		WindowHasVScroll (hwnd) returns TRUE iff hwnd has a vertical scrollbar;
*/
static LONG GetGWL_STYLE (HWND hwnd)
{
	return GetWindowLong (hwnd, GWL_STYLE);
}

static BOOL WindowHasHScroll (HWND hwnd)
{
	LONG hwndStyle = GetGWL_STYLE (hwnd);

	return (hwndStyle & WS_HSCROLL);
}

static BOOL WindowHasVScroll (HWND hwnd)
{
	LONG hwndStyle = GetGWL_STYLE (hwnd);

	return (hwndStyle & WS_VSCROLL);
}

void UpdateWindowScrollbars (HWND hwnd)
{
	int w,h;
	RECT rect;

	GetWindowRect (hwnd, &rect);
	w = rect.right -rect.left;
	h = rect.bottom-rect.top;

	if (WindowHasHScroll (hwnd))
	{
		rect.left   = 0;
		rect.top    = h-GetSystemMetrics (SM_CYHSCROLL);
		rect.right  = w;
		rect.bottom = h;
		InvalidateRect (hwnd,&rect,FALSE);
		RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
		ValidateRect (hwnd,&rect);
	}
	if (WindowHasVScroll (hwnd))
	{
		rect.left   = w-GetSystemMetrics (SM_CXVSCROLL);
		rect.top    = 0;
		rect.right  = w;
		rect.bottom = h;
		InvalidateRect (hwnd,&rect,FALSE);
		RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
		ValidateRect (hwnd,&rect);
	}
}


void WinScreenYSize (OS ios, int *py, OS * oos)
{
	*py = GetSystemMetrics (SM_CYSCREEN);

	*oos = ios;
}

void WinScreenXSize (OS ios, int *px, OS * oos)
{
	*px = GetSystemMetrics (SM_CXSCREEN);

	*oos = ios;
}

void WinMinimumWinSize (int *mx, int *my)
{
	*mx = 48;
	*my = 0;
}

/*	WinScrollbarSize determines system metrics of width and height of scrollbars.
*/
void WinScrollbarSize (OS ios, int *width, int *height, OS * oos)
{
	*width  = GetSystemMetrics (SM_CXVSCROLL);
	*height = GetSystemMetrics (SM_CYHSCROLL);

	*oos = ios;
}

void WinMaxFixedWindowSize (int *mx, int *my)
{
	*mx = GetSystemMetrics (SM_CXMAXIMIZED) -		/* MAXIMIZED size	  */
	4 * GetSystemMetrics (SM_CXFIXEDFRAME); 		/* window borders	  */

	*my = GetSystemMetrics (SM_CYMAXIMIZED) -		/* MAXIMIZED size	  */
		4 * GetSystemMetrics (SM_CYFIXEDFRAME) -	/* window borders	  */
		GetSystemMetrics (SM_CYCAPTION);			/* title bar		  */
}

void WinMaxScrollWindowSize (int *mx, int *my)
{
	*mx = GetSystemMetrics (SM_CXMAXIMIZED) -		// MAXIMIZED size	  
	4 * GetSystemMetrics (SM_CXSIZEFRAME) - 		// window borders	  
	4;												// 2 * client edge	  

	*my = GetSystemMetrics (SM_CYMAXIMIZED) -		// MAXIMIZED size	  
		4 * GetSystemMetrics (SM_CYSIZEFRAME) -		// window borders	  
		4 - 										// 2 * clientedge	  
		GetSystemMetrics (SM_CYCAPTION);			// title bar		  
}
