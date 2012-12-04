#include <windows.h>
#include "cTCP_121.h"

//	Global data with external references:
extern DNSInfo *DNSInfoList;

//	InstallCrossCallxDI adds the proper cross call procedures to the
//	cross call procedures managed by cCrossCall_121.c.
extern int InstallCrossCallTCP (int ios);
