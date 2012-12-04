#ifndef _CPRINTER
#define _CPRINTER

#include "Clean.h"

//	PA: all made extern
extern char * strtokMW(char **str, const char ch1, const char ch2);
extern int startPage(size_t hdc);
extern int endPage  (size_t hdc);
extern int startDoc (size_t hdc);
			// returns err code: >0:no error, <=0: user cancelled file dialog
extern void endDoc  (size_t hdc);
extern void deleteDC(size_t hdc);
extern int wasCanceled(void);
extern void printSetup (int calledFromCleanThread, int devmodeSize,
						char *devmode, char *device, char *driver, char *output,
						int *ok, PRINTDLG **pdPtr
					   );
extern void getDC( int doDialog, int emulateScreen, int calledFromCleanThread, int devmodeLength,
				   char *devmode,char *device,char *driver,char *output,
				   int *err,
				   int *first, int *last, int *copies,
				   PRINTDLG	**ppPrintDlg,
				   size_t *deviceContext
	 			  );
					// err code: -1:no error, others: non fatal error
extern void get_printSetup_with_PRINTDLG(PRINTDLG *pd, CleanString *o_devmode,
										 CleanString *o_device, CleanString *o_driver, CleanString *o_output);
extern void getCaps(HDC hdcPrint, int unq,
					int *maxX, int *maxY,
					int *leftPaper, int *topPaper,
					int *rightPaper, int *bottomPaper,
					int *unqReturn
				   );

extern BOOL CALLBACK AbortProc (HDC hdcPrn, int iCode);
extern BOOL CALLBACK PrintDlgProc (HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);
extern HWND CreateCancelDialog(void);

#endif