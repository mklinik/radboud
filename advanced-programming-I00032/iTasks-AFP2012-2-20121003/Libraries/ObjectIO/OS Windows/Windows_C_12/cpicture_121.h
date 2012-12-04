#include "util_121.h"
#include "intrface_121.h"
#include <math.h>


/* PA: these prototypes have no implementation
void InitPicture( HDC hdc );
void DonePicture( HDC hdc );
*/

/* PA: PointsToPix is local to cpicture_121.c
static int PointsToPix(HDC hdc, int size);
*/

extern void WinGetDC (size_t,OS,HDC*,OS*);
extern OS WinReleaseDC (size_t,HDC,OS);
extern int WinGetVertResolution (void);
extern int WinGetHorzResolution (void);

extern void WinInitPicture (int,int,int,int,int,int,int,int,int,int,CLEAN_STRING,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinDonePicture (HDC,OS,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,CLEAN_STRING*,int*,int*,HDC*,OS*);

extern void WinClipPicture (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinClipRgnPicture(HRGN,HDC,OS,HDC*,OS*);
extern void WinSetClipRgnPicture (HRGN,HDC,OS,HDC*,OS*);
extern void WinGetClipRgnPicture (HDC,OS,HRGN*,HDC*,OS*);

/*	Operations to create, modify, and destroy polygon shapes.
*/
extern void WinAllocPolyShape (int,OS,POINT**,OS*);
extern OS   WinSetPolyPoint (int,int,int,POINT*,OS);
extern OS   WinFreePolyShape (POINT*,OS);

/*	Operations to create, modify and destroy regions.
*/
extern void WinCreateRectRgn (int,int,int,int,OS,HRGN*,OS*);
extern void WinCreateEllipseRgn (int,int,int,int,OS,HRGN*,OS*);
extern void WinCreatePolygonRgn (POINT*,int,int,OS,HRGN*,OS*);
extern void WinSetRgnToRect (int,int,int,int,HRGN,OS,HRGN*,OS*);
extern void WinCombineRgn (HRGN,HRGN,HRGN,int,OS,HRGN*,OS*);
extern void WinGetRgnBox (HRGN,OS,int*,int*,int*,int*,BOOL*,BOOL*,OS*);
extern OS WinPaintRgn (HDC,HRGN,OS);
extern OS WinDeleteObject (HGDIOBJ,OS);

extern void WinSetPenSize (int,HDC,OS,HDC*,OS*);
extern void WinSetPenColor (int,int,int,HDC,OS,HDC*,OS*);
extern void WinSetBackColor (int,int,int,HDC,OS,HDC*,OS*);
extern void WinSetMode (int,HDC,OS,HDC*,OS*);
extern void WinSetPattern (int,HDC,OS,HDC*,OS*);

extern void WinGetPenPos( HDC, OS, int*, int*, HDC*, OS* );
extern void WinMovePenTo (int,int,HDC,OS,HDC*,OS*);
extern void WinMovePen (int,int,HDC,OS,HDC*,OS*);
extern void WinLinePenTo (int,int,HDC,OS,HDC*,OS*);
extern void WinLinePen (int,int,HDC,OS,HDC*,OS*);

extern void WinDrawPoint (int,int,HDC,OS,HDC*,OS*);
extern void WinDrawLine (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinDrawCurve (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern void WinDrawCPoint (int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinDrawCLine (int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinDrawCCurve (int,int,int,int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern void WinDrawChar (int,HDC,OS,HDC*,OS*);
extern void WinDrawString (CLEAN_STRING,HDC,OS,HDC*,OS*);

extern void WinDrawRectangle (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinFillRectangle (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinEraseRectangle (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinInvertRectangle (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinMoveRectangleTo (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinMoveRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinCopyRectangleTo (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinCopyRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinScrollRectangle (int,int,int,int,int,int,HDC,OS,int*,int*,int*,int*,HDC*,OS*);

extern void WinDrawRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinFillRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinEraseRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinInvertRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern void WinDrawOval (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinFillOval (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinEraseOval (int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinInvertOval (int,int,int,int,HDC,OS,HDC*,OS*);

extern void WinDrawCircle (int,int,int,HDC,OS,HDC*,OS*);
extern void WinFillCircle (int,int,int,HDC,OS,HDC*,OS*);
extern void WinEraseCircle (int,int,int,HDC,OS,HDC*,OS*);
extern void WinInvertCircle (int,int,int,HDC,OS,HDC*,OS*);

extern void WinDrawWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinFillWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinEraseWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);
extern void WinInvertWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern OS WinStartPolygon (int,OS);
extern OS WinEndPolygon (OS);
extern OS WinAddPolygonPoint (int,int,OS);
extern void WinDrawPolygon (HDC,OS,HDC*,OS*);
extern void WinFillPolygon (HDC,OS,HDC*,OS*);
extern void WinErasePolygon (HDC,OS,HDC*,OS*);
extern void WinInvertPolygon (HDC,OS,HDC*,OS*);

//	Routines that temporarily create and destroy a DISPLAY HDC. Use this HDC only locally.
extern void WinCreateScreenHDC (OS,HDC*,OS*);
extern OS WinDestroyScreenHDC (HDC,OS);

extern void WinPrintResizedBitmap (int,int,int,int,int,int,char*,HDC,int,HDC*,int*);
extern void WinDrawResizedBitmap (int,int,int,int,int,int,HBITMAP,HDC,int,HDC*,int*);
extern void WinDrawBitmap (int,int,int,int,HBITMAP,HDC,int,HDC*,int*);
extern void WinCreateBitmap (int,char*,HDC,int,HBITMAP*,int*);

extern void WinSetFont (CLEAN_STRING,int,int,HDC,OS,HDC*,OS*);
extern void WinSetFontName (CLEAN_STRING,HDC,OS,HDC*,OS*);
extern void WinSetFontStyle (int,HDC,OS,HDC*,OS*);
extern void WinSetFontSize (int,HDC,OS,HDC*,OS*);
extern void WinGetFontInfo (CLEAN_STRING,int,int,int,HDC,OS,int*,int*,int*,int*,OS*);
extern void WinGetPicFontInfo (HDC,OS,int*,int*,int*,int*,HDC*,OS*);

extern void WinGetPicStringWidth (CLEAN_STRING,HDC,OS,int*,HDC*,OS*);
extern void WinGetPicCharWidth (int,HDC,OS,int*,HDC*,OS*);
extern void WinGetStringWidth (CLEAN_STRING,CLEAN_STRING,int,int,int,HDC,OS,int*,OS*);
extern void WinGetCharWidth (int,CLEAN_STRING,int,int,int,HDC,OS,int*,OS*);

//	Get the resolution of a picture
extern void getResolutionC(size_t,int*,int*);

//	Get scaling factors, which have to be applied to coordinates for clipping regions in case 
//	of emulating the screen resolution for printing (MM_ISOTROPIC)
extern void WinGetPictureScaleFactor(size_t,int,int*,int*,int*,int*,int*,int*);
