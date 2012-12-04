#include <windows.h>
#include "util_121.h"

extern int WinInvalidateWindow (size_t hwnd, int ios);
extern int WinInvalidateRect (size_t hwnd, int left, int top, int right, int bottom, int ios);
extern int WinValidateRect (size_t hwnd, int left, int top, int right, int bottom, int ios);
extern int WinValidateRgn (size_t hwnd, size_t rgn, int ios);

/*	Win(M/S)DIClientToOuterSizeDims returns the width and height needed to add/subtract
	from the client/outer size to obtain the outer/client size. 
	These values must be the same as used by W95AdjustClean(M/S)DIWindowDimensions!
*/
extern void WinMDIClientToOuterSizeDims (int styleFlags, OS ios, int *dw, int *dh, OS *oos);
extern void WinSDIClientToOuterSizeDims (int styleFlags, OS ios, int *dw, int *dh, OS *oos);
/*	Adjust the dimensions of a Clean MDI document window with W95 frame dimensions. 
	These values must be the same as returned by WinMDIClientToOuterSizeDims!
*/
extern void W95AdjustCleanMDIWindowDimensions (DWORD styleFlags, POINT * dim);
/*	Adjust the dimensions of a Clean SDI frame window with W95 frame and menu dimensions.
	These values must be the same as returned by WinSDIClientToOuterSizeDims!
*/
extern void W95AdjustCleanSDIWindowDimensions (DWORD styleFlags, POINT * dim);

/*	Set and get the GWL_USERDATA of a windowhandle.
*/
#ifdef _WIN64
extern void SetGWLP_USERDATA (LONG_PTR data, HWND hwnd);
extern LONG_PTR GetGWLP_USERDATA (HWND hwnd);
#else
extern void SetGWL_USERDATA (LONG data, HWND hwnd);
extern LONG GetGWL_USERDATA (HWND hwnd);
#endif

/*	UpdateWindowScrollbars updates any window scrollbars and non-client area if present.
*/
extern void UpdateWindowScrollbars (HWND hwnd);

/*	Access procedures to dimensions:
*/
extern void WinScreenYSize (OS ios, int *py, OS * oos);
extern void WinScreenXSize (OS ios, int *px, OS * oos);
extern void WinMinimumWinSize (int *mx, int *my);
extern void WinScrollbarSize (OS ios, int *width, int *height, OS * oos);
extern void WinMaxFixedWindowSize (int *mx, int *my);
extern void WinMaxScrollWindowSize (int *mx, int *my);
