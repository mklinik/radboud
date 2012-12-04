/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to system handling that is not part of standard cross call handling. 
********************************************************************************************/
#include "cCCallSystem_121.h"

int WinBeep (int ios)
{
	MessageBeep (MB_ICONASTERISK);
	return ios;
}

void WinGetTime (int ios, int * hour, int * minute, int * second, int * oos)
{
	SYSTEMTIME time;
	GetLocalTime (&time);
	*hour   = time.wHour;
	*minute = time.wMinute;
	*second = time.wSecond;

	*oos    = ios;
}

void WinGetDate (int ios, int * year, int * month, int * day, int * weekday, int * oos)
{
	SYSTEMTIME time;
	GetLocalTime (&time);
	*year    = time.wYear;
	*month   = time.wMonth;
	*day     = time.wDay;
	*weekday = time.wDayOfWeek + 1;

	*oos = ios;
}

int WinWait (int delay, int ios)
{
	Sleep (delay);
	return ios;
}

void WinGetBlinkTime (int ios, int* blinktime, int* oos)
{
	*blinktime = (int) GetCaretBlinkTime ();
	*oos       = ios;
}

void WinGetTickCount (OS ios, int *tickCount, OS * oos)
{
	*tickCount = GetTickCount ();
	*oos = ios;
}
char * toCstring (CLEAN_STRING s)
{
	char *cstr = (char *) NULL;

	cstr = (char *) rmalloc ((s->length) + 1);
	rsncopy (cstr, s->characters, s->length);
	cstr[s->length] = 0;
	return cstr;
}

void WinPlaySound (CLEAN_STRING clfilename, OS ios, Bool * ook, OS * oos)
{	char * cfilename;

	cfilename = toCstring (clfilename);
	if (PlaySound (cfilename, NULL, SND_FILENAME | SND_SYNC))
	{
		*ook = TRUE;
	}
	else
	{
		*ook = FALSE;
	}
	rfree (cfilename);

	*oos = ios;
}
