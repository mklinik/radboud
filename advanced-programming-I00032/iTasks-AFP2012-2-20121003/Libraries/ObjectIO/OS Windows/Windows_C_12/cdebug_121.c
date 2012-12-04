/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines useful for debugging. 
********************************************************************************************/
#include "cdebug_121.h"

int Rand (void)
{
	static int holdrand;
	static int randinited = 0;

	if (!randinited)
	{
		holdrand = (int) GetTickCount ();
		randinited = -1;
	}

	holdrand = holdrand * 214013 + 2531011;

	return ((holdrand >> 16) & 0x7fff);
}

OS ConsolePrint (CLEAN_STRING cleanstr, OS os)
{
	char *cstr;

	cstr = cstring (cleanstr);
	rprintf (cstr);
	return os;
}
