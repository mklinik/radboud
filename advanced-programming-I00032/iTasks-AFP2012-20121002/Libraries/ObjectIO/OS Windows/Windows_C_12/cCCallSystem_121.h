#include "intrface_121.h"
#include "util_121.h"

extern int  WinBeep (int);
extern void WinGetTime (int,int*,int*,int*,int*);
extern void WinGetDate (int,int*,int*,int*,int*,int*);
extern int  WinWait (int,int);
extern void WinGetBlinkTime (int,int*,int*);
extern void WinGetTickCount (OS, int*, OS*);
extern void WinPlaySound (CLEAN_STRING, OS, Bool*, OS*);
