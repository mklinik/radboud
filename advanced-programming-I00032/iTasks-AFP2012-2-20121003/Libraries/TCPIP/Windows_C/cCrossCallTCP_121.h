#include <windows.h>
#include "cTCP_121.h"

//	Global data with external references:
extern DNSInfo *DNSInfoList;

//	InstallCrossCallxDI adds the proper cross call procedures to the
//	cross call procedures managed by cCrossCall_121.c.
extern int InstallCrossCallTCP (int ios);

extern void os_connectTCPC_async
	(int onlyForMac, int ipAddr, int portnum, int *errCodeP, size_t *endpointRefP);
	// lookup entry (CFN)
extern void setEndpointDataC_no_new_notifiers (SOCKET endpointRef, int referenceCount,
							 int hasReceiveNotifier, int hasSendableNotifier, int aborted);
