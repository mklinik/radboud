#include <windows.h>
#include "util_121.h"

extern OS WinDestroyMenu (HMENU menu, OS os);

/*	Install the cross call procedures in the gCrossCallProcedureTable of cCrossCall_121.
*/
extern int InstallCrossCallMenus (int ios);
