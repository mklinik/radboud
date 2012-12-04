/****************************************************************************
    Clean OS Windows library module version 1.2.1.
    This module is part of the Clean Object I/O library, version 1.2.1,
    for the Windows platform.
****************************************************************************/

/****************************************************************************
    About this module:
    Clean Game Library (low-level functions, DirectX implementation),
    Mike Wiering (mike.wiering@cs.kun.nl).
****************************************************************************/

#include "cOSGameLib_121.h"

#ifdef _WIN64
static void my_copy_memory (void *d_q, void *s_q, size_t s)
{
	char *d_p,*s_p;
	
	s_p=s_q;
	d_p=d_q;
	while (s>0){
		*d_p++ = *s_p++;
		--s;
	}
}

static void my_zero_memory (void *q,size_t s)
{
	char *p;
	
	p=q;

	while (s>=32){
		*(__int64*)p=0;
		*(__int64*)(p+16)=0;
		*(__int64*)(p+8)=0;		
		*(__int64*)(p+24)=0;
		p+=32;
		s-=32;
	}
	if (s>=16){
		*(__int64*)p=0;
		*(__int64*)(p+8)=0;
		p+=16;
		s-=16;
	}
	if (s>=8){
		*(__int64*)p=0;
		p+=8;
		s-=8;
	}
	if (s>=4){
		*(int*)p=0;
		p+=4;
		s-=4;
	}
	if (s>=2){
		*(short*)p=0;
		p+=2;
		s-=2;
	}
	if (s>0)
		*(char*)p=0;	
}
#endif

/* DD functions from DDUTIL.CPP ... */

/*
 *  DDLoadBitmap
 *
 *  create a DirectDrawSurface from a bitmap resource.
 *
 */
// extern "C"
IDirectDrawSurface * DDLoadBitmap(IDirectDraw *pdd, LPCSTR szBitmap, int dx, int dy)
{
    HBITMAP             hbm;
    BITMAP              bm;
    DDSURFACEDESC       ddsd;
    IDirectDrawSurface *pdds;

    //
    //  try to load the bitmap as a resource, if that fails, try it as a file
    //
    hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), szBitmap, IMAGE_BITMAP, dx, dy, LR_CREATEDIBSECTION);

    if (hbm == NULL)
    hbm = (HBITMAP)LoadImage(NULL, szBitmap, IMAGE_BITMAP, dx, dy, LR_LOADFROMFILE|LR_CREATEDIBSECTION);

    if (hbm == NULL)
    return NULL;

    //
    // get size of the bitmap
    //
    GetObject(hbm, sizeof(bm), &bm);      // get size of bitmap

    //
    // create a DirectDrawSurface for this bitmap
    //
#ifdef _WIN64
	my_zero_memory (&ddsd, sizeof(ddsd));
#else
    ZeroMemory(&ddsd, sizeof(ddsd));
#endif
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth = bm.bmWidth;
    ddsd.dwHeight = bm.bmHeight;

    if (IDirectDraw_CreateSurface (pdd, &ddsd, &pdds, NULL) != DD_OK)
    return NULL;

    DDCopyBitmap(pdds, hbm, 0, 0, 0, 0);

    DeleteObject(hbm);

    return pdds;
}

/*
 *  DDReLoadBitmap
 *
 *  load a bitmap from a file or resource into a directdraw surface.
 *  normaly used to re-load a surface after a restore.
 *
 */
HRESULT DDReLoadBitmap(IDirectDrawSurface *pdds, LPCSTR szBitmap)
{
    HBITMAP             hbm;
    HRESULT             hr;

    //
    //  try to load the bitmap as a resource, if that fails, try it as a file
    //
    hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), szBitmap, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);

    if (hbm == NULL)
    hbm = (HBITMAP)LoadImage(NULL, szBitmap, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE|LR_CREATEDIBSECTION);

    if (hbm == NULL)
    {
    OutputDebugString("handle is null\n");
    return E_FAIL;
    }

    hr = DDCopyBitmap(pdds, hbm, 0, 0, 0, 0);
    if (hr != DD_OK)
    {
    OutputDebugString("ddcopybitmap failed\n");
    }


    DeleteObject(hbm);
    return hr;
}

/*
 *  DDCopyBitmap
 *
 *  draw a bitmap into a DirectDrawSurface
 *
 */
// extern "C"
HRESULT DDCopyBitmap(IDirectDrawSurface *pdds, HBITMAP hbm, int x, int y, int dx, int dy)
{
    HDC                 hdcImage;
    HDC                 hdc;
    BITMAP              bm;
    DDSURFACEDESC       ddsd;
    HRESULT             hr;

    if (hbm == NULL || pdds == NULL)
    return E_FAIL;

    //
    // make sure this surface is restored.
    //
    IDirectDrawSurface_Restore(pdds);

    //
    //  select bitmap into a memoryDC so we can use it.
    //
    hdcImage = CreateCompatibleDC(NULL);
    if (!hdcImage)
    OutputDebugString("createcompatible dc failed\n");
    SelectObject(hdcImage, hbm);

    //
    // get size of the bitmap
    //
    GetObject(hbm, sizeof(bm), &bm);    // get size of bitmap
    dx = dx == 0 ? bm.bmWidth  : dx;    // use the passed size, unless zero
    dy = dy == 0 ? bm.bmHeight : dy;

    //
    // get size of surface.
    //
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    IDirectDrawSurface_GetSurfaceDesc(pdds, &ddsd);

    if ((hr = IDirectDrawSurface_GetDC(pdds, &hdc)) == DD_OK)
    {
    StretchBlt(hdc, 0, 0, ddsd.dwWidth, ddsd.dwHeight, hdcImage, x, y, dx, dy, SRCCOPY);
    IDirectDrawSurface_ReleaseDC(pdds, hdc);
    }

    DeleteDC(hdcImage);

    return hr;
}

//
//  DDLoadPalette
//
//  Create a DirectDraw palette object from a bitmap resoure
//
//  if the resource does not exist or NULL is passed create a
//  default 332 palette.
//
// extern "C"
IDirectDrawPalette * DDLoadPalette (IDirectDraw *pdd, LPCSTR szBitmap)
{
    IDirectDrawPalette* ddpal;
    int i, r, g, b;
    int n;
//    int                 fh;
// Mike ... //
    HANDLE              fh;
    SECURITY_ATTRIBUTES sa;
// ... Mike //
    HRSRC               h;
    LPBITMAPINFOHEADER  lpbi;
    PALETTEENTRY        ape[256];
    RGBQUAD *           prgb;

// Mike ... //
    sa.nLength = sizeof (SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = FALSE;
// ... Mike //

    //
    // build a 332 palette as the default.
    //
/*
    for (i=0; i<256; i++)
    {
    ape[i].peRed   = (BYTE)(((i >> 5) & 0x07) * 255 / 7);
    ape[i].peGreen = (BYTE)(((i >> 2) & 0x07) * 255 / 7);
    ape[i].peBlue  = (BYTE)(((i >> 0) & 0x03) * 255 / 3);
    ape[i].peFlags = (BYTE)0;
    }
*/
    for (i = 0; i < 256; i++)
    {
        if (i < 6 * 7 * 6)
        {
            r = 224 * ( i       % 6) / 5 + 16;
            g = 224 * ((i / 6)  % 7) / 6 + 16;
            b = 224 * ((i / 42) % 6) / 5 + 16;
        }
        else
        {
            switch (i)
            {
                case 252: { r = 0; g = 0; b = 0; break; }
                case 253: { r = 8; g = 8; b = 8; break; }
                case 254: { r = 247; g = 247; b = 247; break; }
                case 255: { r = 255; g = 255; b = 255; break; }
            }
        }
    ape[i].peRed   = (BYTE) r;
    ape[i].peGreen = (BYTE) g;
    ape[i].peBlue  = (BYTE) b;

    ape[i].peFlags = (BYTE) 0;
    }

    //
    // get a pointer to the bitmap resource.
    //
    if (szBitmap && (h = FindResource(NULL, szBitmap, RT_BITMAP)))
    {
    lpbi = (LPBITMAPINFOHEADER)LockResource(LoadResource(NULL, h));
    if (!lpbi)
        OutputDebugString("lock resource failed\n");
    prgb = (RGBQUAD*)((BYTE*)lpbi + lpbi->biSize);

    if (lpbi == NULL || lpbi->biSize < sizeof(BITMAPINFOHEADER))
        n = 0;
    else if (lpbi->biBitCount > 8)
        n = 0;
    else if (lpbi->biClrUsed == 0)
        n = 1 << lpbi->biBitCount;
    else
        n = lpbi->biClrUsed;

    //
    //  a DIB color table has its colors stored BGR not RGB
    //  so flip them around.
    //
    for(i=0; i<n; i++ )
    {
        ape[i].peRed   = prgb[i].rgbRed;
        ape[i].peGreen = prgb[i].rgbGreen;
        ape[i].peBlue  = prgb[i].rgbBlue;
        ape[i].peFlags = 0;
    }
    }
    else if (szBitmap &&
           //  (fh = _lopen(szBitmap, OF_READ)) != -1
           // Mike ... //
              (fh = CreateFile (szBitmap, GENERIC_READ,
                  FILE_SHARE_READ, &sa, OPEN_EXISTING,
                  FILE_ATTRIBUTE_NORMAL, NULL)
              ) != NULL
            )
           // ... Mike //
    {
    BITMAPFILEHEADER bf;
    BITMAPINFOHEADER bi;
    unsigned long iNumRead;

  /*
    _lread(fh, &bf, sizeof(bf));
    _lread(fh, &bi, sizeof(bi));
    _lread(fh, ape, sizeof(ape));
    _lclose(fh);
  */
    // Mike ... //
    ReadFile (fh, &bf, sizeof (bf), &iNumRead, NULL);
    ReadFile (fh, &bi, sizeof (bi), &iNumRead, NULL);
    ReadFile (fh, ape, sizeof (ape), &iNumRead, NULL);
    CloseHandle (fh);
    // ... Mike //

    if (bi.biSize != sizeof(BITMAPINFOHEADER))
        n = 0;
    else if (bi.biBitCount > 8)
        n = 0;
    else if (bi.biClrUsed == 0)
        n = 1 << bi.biBitCount;
    else
        n = bi.biClrUsed;

    //
    //  a DIB color table has its colors stored BGR not RGB
    //  so flip them around.
    //
    for(i=0; i<n; i++ )
    {
        BYTE r = ape[i].peRed;
        ape[i].peRed  = ape[i].peBlue;
        ape[i].peBlue = r;
    }
    }


    IDirectDraw_CreatePalette(pdd, DDPCAPS_8BIT, ape, &ddpal, NULL);

    return ddpal;
}

/*
 * DDColorMatch
 *
 * convert a RGB color to a pysical color.
 *
 * we do this by leting GDI SetPixel() do the color matching
 * then we lock the memory and see what it got mapped to.
 */
// extern "C"
DWORD DDColorMatch(IDirectDrawSurface *pdds, COLORREF rgb)
{
    COLORREF rgbT;
    HDC hdc;
    DWORD dw = CLR_INVALID;
    DDSURFACEDESC ddsd;
    HRESULT hres;

    //
    //  use GDI SetPixel to color match for us
    //
    if (rgb != CLR_INVALID && IDirectDrawSurface_GetDC(pdds, &hdc) == DD_OK)
    {
    rgbT = GetPixel(hdc, 0, 0);             // save current pixel value
    SetPixel(hdc, 0, 0, rgb);               // set our value
    IDirectDrawSurface_ReleaseDC(pdds, hdc);
    }

    //
    // now lock the surface so we can read back the converted color
    //
    ddsd.dwSize = sizeof(ddsd);
    while ((hres = IDirectDrawSurface_Lock(pdds, NULL, &ddsd, 0, NULL)) == DDERR_WASSTILLDRAWING)
    ;

    if (hres == DD_OK)
    {
    dw  = *(DWORD *)ddsd.lpSurface;                     // get DWORD
        if(ddsd.ddpfPixelFormat.dwRGBBitCount < 32)
            dw &= (1 << ddsd.ddpfPixelFormat.dwRGBBitCount)-1;  // mask it to bpp
    IDirectDrawSurface_Unlock(pdds, NULL);
    }

    //
    //  now put the color that was there back.
    //
    if (rgb != CLR_INVALID && IDirectDrawSurface_GetDC(pdds, &hdc) == DD_OK)
    {
    SetPixel(hdc, 0, 0, rgbT);
    IDirectDrawSurface_ReleaseDC(pdds, hdc);
    }

    return dw;
}

/*
 * DDSetColorKey
 *
 * set a color key for a surface, given a RGB.
 * if you pass CLR_INVALID as the color key, the pixel
 * in the upper-left corner will be used.
 */
// extern "C"
HRESULT DDSetColorKey(IDirectDrawSurface *pdds, COLORREF rgb)
{
    DDCOLORKEY          ddck;

    ddck.dwColorSpaceLowValue  = DDColorMatch(pdds, rgb);
    ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;
    return IDirectDrawSurface_SetColorKey(pdds, DDCKEY_SRCBLT, &ddck);
}

/* ... DD functions from DDUTIL.CPP */


/* sound functions from DSUTIL.C ... */

static const char c_szWAV[] = "WAV";

static HGLOBAL ptr = NULL;

///////////////////////////////////////////////////////////////////////////////
//
// DSLoadSoundBuffer
//
///////////////////////////////////////////////////////////////////////////////

IDirectSoundBuffer *DSLoadSoundBuffer(IDirectSound *pDS, LPCTSTR lpName)
{
    IDirectSoundBuffer *pDSB = NULL;
    DSBUFFERDESC dsBD = {0};
    BYTE *pbWaveData;

    if (DSGetWaveResource(NULL, lpName, &dsBD.lpwfxFormat, &pbWaveData, &dsBD.dwBufferBytes))
    {
        dsBD.dwSize = sizeof(dsBD);
        dsBD.dwFlags = DSBCAPS_STATIC | DSBCAPS_CTRLFREQUENCY | DSBCAPS_CTRLPAN | DSBCAPS_CTRLVOLUME | DSBCAPS_GETCURRENTPOSITION2;

        if (SUCCEEDED(IDirectSound_CreateSoundBuffer(pDS, &dsBD, &pDSB, NULL)))
        {
            if (!DSFillSoundBuffer(pDSB, pbWaveData, dsBD.dwBufferBytes))
            {
                IDirectSoundBuffer_Release(pDSB);
                pDSB = NULL;
            }
        }
        else
        {
            pDSB = NULL;
        }
    }

    if (ptr)
    {
        GlobalFree (ptr);
        ptr = NULL;
    }

    return pDSB;
}

///////////////////////////////////////////////////////////////////////////////
//
// DSReloadSoundBuffer
//
///////////////////////////////////////////////////////////////////////////////

BOOL DSReloadSoundBuffer(IDirectSoundBuffer *pDSB, LPCTSTR lpName)
{
    BOOL result=FALSE;
    BYTE *pbWaveData;
    DWORD cbWaveSize;

    if (DSGetWaveResource(NULL, lpName, NULL, &pbWaveData, &cbWaveSize))
    {
        if (SUCCEEDED(IDirectSoundBuffer_Restore(pDSB)) &&
            DSFillSoundBuffer(pDSB, pbWaveData, cbWaveSize))
        {
            result = TRUE;
        }
    }

    if (ptr)
    {
        GlobalFree (ptr);
        ptr = NULL;
    }

    return result;
}

///////////////////////////////////////////////////////////////////////////////
//
// DSGetWaveResource
//
///////////////////////////////////////////////////////////////////////////////

BOOL DSGetWaveResource(HMODULE hModule, LPCTSTR lpName,
    WAVEFORMATEX **ppWaveHeader, BYTE **ppbWaveData, DWORD *pcbWaveSize)
{
    HRSRC hResInfo;
    HGLOBAL hResData;
    void *pvRes;

    HANDLE fh;
    DWORD dwsize;
    DWORD dwread;

    if (((hResInfo = FindResource(hModule, lpName, c_szWAV)) != NULL) &&
        ((hResData = LoadResource(hModule, hResInfo)) != NULL) &&
        ((pvRes = LockResource(hResData)) != NULL) &&
        DSParseWaveResource(pvRes, ppWaveHeader, ppbWaveData, pcbWaveSize))
    {
        return TRUE;
    }

    fh = CreateFile (lpName, GENERIC_READ, FILE_SHARE_READ, NULL,
                     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (fh != INVALID_HANDLE_VALUE)
    {
        dwsize = GetFileSize (fh, NULL);
        if (dwsize != 0xFFFFFFFF)
        {
            if (ptr)
            {
                GlobalFree (ptr);
                ptr = NULL;
            }
            ptr = GlobalAlloc (GPTR, dwsize);
            if (ptr)
            {
                if (ReadFile (fh, ptr, dwsize, &dwread, NULL))
                {
                    if (DSParseWaveResource (ptr, ppWaveHeader, ppbWaveData,
                            pcbWaveSize))
                    {
                        return TRUE;
                    }
                }
            }
        }
    }

    return FALSE;
}

///////////////////////////////////////////////////////////////////////////////
// SndObj fns
///////////////////////////////////////////////////////////////////////////////

SNDOBJ *SndObjCreate(IDirectSound *pDS, LPCTSTR lpName, int iConcurrent)
{
    SNDOBJ *pSO = NULL;
    LPWAVEFORMATEX pWaveHeader;
    BYTE *pbData;
    UINT cbData;

    if (DSGetWaveResource(NULL, lpName, &pWaveHeader, &pbData, &cbData))
    {
        if (iConcurrent < 1)
            iConcurrent = 1;

        if ((pSO = (SNDOBJ *)LocalAlloc(LPTR, sizeof(SNDOBJ) +
        (iConcurrent-1) * sizeof(IDirectSoundBuffer *))) != NULL)
        {
            int i;

            pSO->iAlloc = iConcurrent;
            pSO->pbWaveData = pbData;
            pSO->cbWaveSize = cbData;
            pSO->Buffers[0] = DSLoadSoundBuffer(pDS, lpName);

            for (i=1; i<pSO->iAlloc; i++)
            {
                if (FAILED(IDirectSound_DuplicateSoundBuffer(pDS,
                        pSO->Buffers[0], &pSO->Buffers[i])))
                {
                    pSO->Buffers[i] = DSLoadSoundBuffer(pDS, lpName);
                    if (!pSO->Buffers[i])
                    {
                        SndObjDestroy(pSO);
                        pSO = NULL;
                        break;
                    }
                }
            }
        }
    }

    if (ptr)
    {
        GlobalFree (ptr);
        ptr = NULL;
    }

    return pSO;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

void SndObjDestroy(SNDOBJ *pSO)
{
    if (pSO)
    {
        int i;

        for (i=0; i<pSO->iAlloc; i++)
        {
            if (pSO->Buffers[i])
            {
                IDirectSoundBuffer_Release(pSO->Buffers[i]);
                pSO->Buffers[i] = NULL;
            }
        }
        LocalFree((HANDLE)pSO);
    }
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

IDirectSoundBuffer *SndObjGetFreeBuffer(SNDOBJ *pSO)
{
    IDirectSoundBuffer *pDSB;

    if (pSO == NULL)
        return NULL;

    if (pDSB = pSO->Buffers[pSO->iCurrent])
    {
        HRESULT hres;
        DWORD dwStatus;

        hres = IDirectSoundBuffer_GetStatus(pDSB, &dwStatus);

        if (FAILED(hres))
            dwStatus = 0;

        if ((dwStatus & DSBSTATUS_PLAYING) == DSBSTATUS_PLAYING)
        {
            if (pSO->iAlloc > 1)
            {
                if (++pSO->iCurrent >= pSO->iAlloc)
                    pSO->iCurrent = 0;

                pDSB = pSO->Buffers[pSO->iCurrent];
                hres = IDirectSoundBuffer_GetStatus(pDSB, &dwStatus);

                if (SUCCEEDED(hres) && (dwStatus & DSBSTATUS_PLAYING) == DSBSTATUS_PLAYING)
                {
                    IDirectSoundBuffer_Stop(pDSB);
                    IDirectSoundBuffer_SetCurrentPosition(pDSB, 0);
                }
            }
            else
            {
                pDSB = NULL;
            }
        }

        if (pDSB && (dwStatus & DSBSTATUS_BUFFERLOST))
        {
            if (FAILED(IDirectSoundBuffer_Restore(pDSB)) ||
                !DSFillSoundBuffer(pDSB, pSO->pbWaveData, pSO->cbWaveSize))
            {
                pDSB = NULL;
            }
        }
    }

    return pDSB;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL SndObjPlay(SNDOBJ *pSO, DWORD dwPlayFlags)
{
    BOOL result = FALSE;

    if (pSO == NULL)
        return FALSE;

    if ((!(dwPlayFlags & DSBPLAY_LOOPING) || (pSO->iAlloc == 1)))
    {
    IDirectSoundBuffer *pDSB = SndObjGetFreeBuffer(pSO);
    if (pDSB != NULL) {
        result = SUCCEEDED(IDirectSoundBuffer_Play(pDSB, 0, 0, dwPlayFlags));
    }
    }

    return result;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL SndObjStop(SNDOBJ *pSO)
{
    int i;

    if (pSO == NULL)
        return FALSE;

    for (i=0; i<pSO->iAlloc; i++)
    {
        IDirectSoundBuffer_Stop(pSO->Buffers[i]);
        IDirectSoundBuffer_SetCurrentPosition(pSO->Buffers[i], 0);
    }

    return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL DSFillSoundBuffer(IDirectSoundBuffer *pDSB, BYTE *pbWaveData, DWORD cbWaveSize)
{
    if (pDSB && pbWaveData && cbWaveSize)
    {
        LPVOID pMem1, pMem2;
        DWORD dwSize1, dwSize2;

        if (SUCCEEDED(IDirectSoundBuffer_Lock(pDSB, 0, cbWaveSize,
            &pMem1, &dwSize1, &pMem2, &dwSize2, 0)))
        {
#ifdef _WIN64
			my_copy_memory (pMem1, pbWaveData, dwSize1);
#else
            CopyMemory (pMem1, pbWaveData, dwSize1);
#endif

            if ( 0 != dwSize2 )
#ifdef _WIN64
				my_copy_memory (pMem2, pbWaveData+dwSize1, dwSize2);
#else
                CopyMemory (pMem2, pbWaveData+dwSize1, dwSize2);
#endif
            IDirectSoundBuffer_Unlock(pDSB, pMem1, dwSize1, pMem2, dwSize2);
            return TRUE;
        }
    }

    return FALSE;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL DSParseWaveResource(void *pvRes, WAVEFORMATEX **ppWaveHeader, BYTE **ppbWaveData,DWORD *pcbWaveSize)
{
    DWORD *pdw;
    DWORD *pdwEnd;
    DWORD dwRiff;
    DWORD dwType;
    DWORD dwLength;

    if (ppWaveHeader)
        *ppWaveHeader = NULL;

    if (ppbWaveData)
        *ppbWaveData = NULL;

    if (pcbWaveSize)
        *pcbWaveSize = 0;

    pdw = (DWORD *)pvRes;
    dwRiff = *pdw++;
    dwLength = *pdw++;
    dwType = *pdw++;

    if (dwRiff != mmioFOURCC('R', 'I', 'F', 'F'))
        goto exit;      // not even RIFF

    if (dwType != mmioFOURCC('W', 'A', 'V', 'E'))
        goto exit;      // not a WAV

    pdwEnd = (DWORD *)((BYTE *)pdw + dwLength-4);

    while (pdw < pdwEnd)
    {
        dwType = *pdw++;
        dwLength = *pdw++;

        switch (dwType)
        {
        case mmioFOURCC('f', 'm', 't', ' '):
            if (ppWaveHeader && !*ppWaveHeader)
            {
                if (dwLength < sizeof(WAVEFORMAT))
                    goto exit;      // not a WAV

                *ppWaveHeader = (WAVEFORMATEX *)pdw;

                if ((!ppbWaveData || *ppbWaveData) &&
                    (!pcbWaveSize || *pcbWaveSize))
                {
                    return TRUE;
                }
            }
            break;

        case mmioFOURCC('d', 'a', 't', 'a'):
            if ((ppbWaveData && !*ppbWaveData) ||
                (pcbWaveSize && !*pcbWaveSize))
            {
                if (ppbWaveData)
                    *ppbWaveData = (LPBYTE)pdw;

                if (pcbWaveSize)
                    *pcbWaveSize = dwLength;

                if (!ppWaveHeader || *ppWaveHeader)
                    return TRUE;
            }
            break;
        }

        pdw = (DWORD *)((BYTE *)pdw + ((dwLength+1)&~1));
    }

exit:
    return FALSE;
}

/* ... sound functions from DSUTIL.C */



extern HWND ghMainWindow;

int ScreenWidth = 320;
int ScreenHeight = 240;
int BitsPerPixel = 8;

int ActualWidth = 320;
int ActualHeight = 240;

int XShiftScreen = 0;
int YShiftScreen = 0;

BOOL FullScreen = TRUE;
BOOL bGameActive = FALSE;
HWND ghGameWindow = NULL;

static LPDIRECTDRAW lpDD = NULL;
static LPDIRECTDRAWSURFACE lpDDSFront = NULL;
static LPDIRECTDRAWSURFACE lpDDSBack = NULL;
static LPDIRECTDRAWPALETTE lpDDPal = NULL;
static IDirectDrawClipper *clipper = NULL;

typedef struct GAMEBLOCKSEQUENCE
{
    int iSequenceID;
    int iSequenceLength;
    char *sSequence;
    int iPosition;
    int iCounter;
    struct GAMEBLOCKSEQUENCE *gbsNext;
} GAMEBLOCKSEQUENCE;

typedef struct GAMEBITMAPINFO
{
    int iBitmapID;
    LPDIRECTDRAWSURFACE lpDDSBitmap;
    int iBitmapWidth;
    int iBitmapHeight;
    int iBlockWidth;
    int iBlockHeight;
    int iBlockCountX;
    int iBlockCountY;
    BOOL bTransparent;
    struct GAMEBLOCKSEQUENCE *gbsGameBlockSequence;
    struct GAMEBITMAPINFO *gbipNext;
    char *sName;
} GAMEBITMAPINFO;

static GAMEBITMAPINFO *gbipGameBitmapInfo = NULL;

static int iPrevGBIP = 0;
static GAMEBITMAPINFO *gbipPrev = NULL;


/* release DirectDraw object */
static void ReleaseDD (void)
{
  if (lpDDPal)
  {
    IDirectDrawSurface_Release (lpDDPal);
    lpDDPal = NULL;
  }

  if (clipper)
  {
     IDirectDrawClipper_Release (clipper);
     clipper = NULL;
  }

  if (lpDD != NULL)
  {
    if (lpDDSFront != NULL)
    {
      IDirectDraw_Release (lpDDSFront);
      lpDDSFront = NULL;
    }
    IDirectDraw_Release (lpDD);
    lpDD = NULL;
  }
} /* ReleaseDD */

/* restore directdraw object */
void DDRestoreAll (void)
{
    HRESULT ddrval;
    //    DDSURFACEDESC ddsd;     PA: not used
    GAMEBITMAPINFO *gbip = gbipGameBitmapInfo;

    if (lpDD != NULL)
    {
        ddrval = IDirectDrawSurface_Restore (lpDDSFront);
        if (ddrval == DD_OK)
        {
            ddrval = IDirectDrawSurface_Restore (lpDDSBack);
            if (ddrval == DD_OK)
            {
                while (gbip)
                {
                    ddrval = IDirectDrawSurface_Restore (gbip->lpDDSBitmap);
                    if (ddrval == DD_OK)
                    {
                        DDReLoadBitmap (gbip->lpDDSBitmap, gbip->sName);
                    }
                    gbip = gbip->gbipNext;
                }
            }
        }
    }
}   /* DDRestoreAll */




/* --------------------- window / screen functions --------------------- */
/* PA: the typedef DEVICES and the constants aDDDevs, MaxDevIndex, DevIndex appear only in commented code.
typedef struct {
  GUID *guid;
  GUID DriverGUID;
  CHAR DriverDesc[128];
  CHAR DriverName[128];
} DEVICES;

DEVICES aDDDevs[15];
int MaxDevIndex = 0;
int DevIndex = 0;
*/
/* PA: DDEnumCallback does not seem to be used. Refered to only in commented code (line 8-13 of OSInitGameWindow)
HRESULT CALLBACK DDEnumCallback (GUID *lpGUID,
                                 LPSTR DriverDesc,
                                 LPSTR DriverName,
                                 LPVOID lpContext)
{
    if (MaxDevIndex >= sizeof (aDDDevs) / sizeof (aDDDevs[0]))
        return E_FAIL;

    // Msg ("Device: %s (%s)", DriverDesc, DriverName);

    if (lpGUID == NULL || lpGUID == (GUID*) DDCREATE_EMULATIONONLY)
    {
        aDDDevs[MaxDevIndex].guid = lpGUID;
    }
    else
    {
        aDDDevs[MaxDevIndex].DriverGUID = *lpGUID;
        aDDDevs[MaxDevIndex].guid = &aDDDevs[MaxDevIndex].DriverGUID;
    }

    lstrcpyn(aDDDevs[MaxDevIndex].DriverDesc, DriverDesc, 128);
    lstrcpyn(aDDDevs[MaxDevIndex].DriverName, DriverName, 128);

    MaxDevIndex++;
    return DDENUMRET_OK;
}
*/

static void ShowError (char *Msg)
{
    ReleaseDD ();
    if (ghGameWindow)
    {
        DestroyWindow (ghGameWindow);
        ghGameWindow = NULL;
    }
    if (ghMainWindow)
    {
        DestroyWindow (ghMainWindow);
        ghMainWindow = NULL;
    }
    MessageBox (NULL, Msg, NULL, MB_OK);
}   /* ShowError */


/* set up the game window */
BOOL OSInitGameWindow (void)
{
    HRESULT ddrval;
    DDSURFACEDESC ddsd;
    DDSCAPS ddscaps;
    int result;

    result = FALSE;

//    DirectDrawEnumerate ((LPDDENUMCALLBACK) &DDEnumCallback, NULL);
//    DDEnumCallback ((GUID *) DDCREATE_EMULATIONONLY,
//                    "Hardware Emulation Layer", "", NULL);
//    DevIndex = MaxDevIndex - 1;

//    ddrval = DirectDrawCreate (aDDDevs[DevIndex].guid, &lpDD, NULL);
    ddrval = DirectDrawCreate (NULL, &lpDD, NULL);
    if (!(ddrval == DD_OK))
        ShowError ("DirectDrawCreate failed");
    else
    {
        if (FullScreen)
            ddrval = IDirectDraw_SetCooperativeLevel (lpDD, ghGameWindow,
                         DDSCL_EXCLUSIVE |
                         DDSCL_FULLSCREEN |
                         DDSCL_ALLOWMODEX |
                         DDSCL_ALLOWREBOOT);
        else
            ddrval = IDirectDraw_SetCooperativeLevel (lpDD, ghGameWindow,
                DDSCL_NORMAL);
        if (!(ddrval == DD_OK))
            ShowError ("IDirectDraw_SetCooperativeLevel failed");
        else
        {
            ActualWidth = ScreenWidth;
            ActualHeight = ScreenHeight;

            if (FullScreen)
            {
                int oldbpp = BitsPerPixel;

                ShowCursor (FALSE);
                ddrval = IDirectDraw_SetDisplayMode (lpDD, ScreenWidth, ScreenHeight, BitsPerPixel);

                /* added 11-24-99, try 8 bit color if 16/24/32 bit fails */
                if (BitsPerPixel > 8)
                {
                    if (!(ddrval == DD_OK))
                    {
                        BitsPerPixel = 8;
                        ddrval = IDirectDraw_SetDisplayMode (lpDD, ScreenWidth, ScreenHeight, BitsPerPixel);
                    }
                }
                /* added  3-29-00, try 640x480 if lower resolution fails */
                if ((!(ddrval == DD_OK)) && ((ScreenWidth < 640) || (ScreenHeight < 480)))
                {
                  //  MessageBox (NULL, "Screen mode not supported. Trying higher resolution", NULL, MB_OK);

                    ActualWidth = 640;
                    ActualHeight = 480;
                    BitsPerPixel = oldbpp;
                    ddrval = IDirectDraw_SetDisplayMode (lpDD, ActualWidth, ActualHeight, BitsPerPixel);

                    if (BitsPerPixel > 8)
                    {
                        if (!(ddrval == DD_OK))
                        {
                            BitsPerPixel = 8;
                            ddrval = IDirectDraw_SetDisplayMode (lpDD, ScreenWidth, ScreenHeight, BitsPerPixel);
                        }
                    }
                }
            }

            XShiftScreen = (ActualWidth - ScreenWidth) / 2;
            YShiftScreen = (ActualHeight - ScreenHeight) / 2;

            if (!(ddrval == DD_OK))
                ShowError ("IDirectDraw_SetDisplayMode failed");
            else
            {
#ifdef _WIN64
				my_zero_memory (&ddsd, sizeof (DDSURFACEDESC));  // set non-used values to zero!
#else
		        memset (&ddsd, 0, sizeof (DDSURFACEDESC));  // set non-used values to zero!
#endif
                ddsd.dwSize = sizeof (ddsd);
                if (FullScreen)
                {
                    ddsd.dwFlags = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
                    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE |
                                          DDSCAPS_FLIP |
                                          DDSCAPS_COMPLEX |
                                          DDSCAPS_MODEX;
                }
                else
                {
                    ddsd.dwFlags = DDSD_CAPS;
                    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;
                }

                ddsd.dwBackBufferCount = 1;
                ddrval = IDirectDraw_CreateSurface (lpDD, &ddsd, &lpDDSFront, NULL);

                if (!(ddrval == DD_OK))
                    ShowError ("IDirectDraw_CreateSurface failed (lpDDSFront)");
                else
                {
                    ddrval = IDirectDraw_CreateClipper (lpDD, 0, &clipper, NULL);
                    if (!(ddrval == DD_OK))
                        ShowError ("IDirectDraw_CreateClipper failed");
                    else
                    {
                        ddrval = IDirectDrawClipper_SetHWnd (clipper, 0, ghGameWindow);
                        if (!(ddrval == DD_OK))
                            ShowError ("IDirectDrawClipper_SetHWnd failed");
                        else
                        {
                            ddrval = IDirectDrawSurface_SetClipper (lpDDSFront, clipper);
                            if (!(ddrval == DD_OK))
                                ShowError ("IDirectDrawClipper_SetClipper failed");
                            else
                            {
                                if (!FullScreen)
                                {
#ifdef _WIN64
									my_zero_memory (&ddsd, sizeof (DDSURFACEDESC));
#else
                                    memset (&ddsd, 0, sizeof (DDSURFACEDESC));
#endif
                                    ddsd.dwSize = sizeof (ddsd);
                                    ddsd.dwFlags = DDSD_CAPS |
                                                   DDSD_WIDTH |
                                                   DDSD_HEIGHT;

                                    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN | DDSCAPS_SYSTEMMEMORY;
                                    ddsd.dwWidth = ScreenWidth;
                                    ddsd.dwHeight = ScreenHeight;
                                    ddrval = IDirectDraw_CreateSurface (lpDD, &ddsd, &lpDDSBack, NULL);
                                    if (!(ddrval == DD_OK))
                                        ShowError ("IDirectDrawClipper_CreateSurface failed (lpDDSBack)");
                                    else
                                        result = TRUE;
                                }
                                else
                                {
                                    ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
                                    ddrval = IDirectDrawSurface_GetAttachedSurface (lpDDSFront, &ddscaps, &lpDDSBack);
                                    if (!(ddrval == DD_OK))
                                        ShowError ("IDirectDrawSurface_GetAttachedSurface failed");
                                    else
                                    {
                                        if (BitsPerPixel == 8)
                                        {
                                           lpDDPal = DDLoadPalette (lpDD, NULL);
                                           if (lpDDPal == NULL)
                                               ShowError ("DDMakeDefaultPalette failed");
                                           else
                                               IDirectDrawSurface_SetPalette (lpDDSFront, lpDDPal);
                                        }

                                        result = TRUE;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return result;
}   /* OSInitGameWindow */

/* shut down the game window */
void OSDeInitGameWindow (void)
{
    ReleaseDD ();
}   /* OSDeInitGameWindow */

/* get game window handle */
BOOL OSGetGameWindowHDC (HDC *hdc)
{
   return (IDirectDrawSurface_GetDC (lpDDSBack, hdc) == DD_OK);
}   /* OSDeInitGameWindow */

/* release game window handle */
void OSReleaseGameWindowHandle (HDC hdc)
{
   IDirectDrawSurface_ReleaseDC (lpDDSBack, hdc);
}   /* OSReleaseGameWindowHandle */

/* clear the (visual) screen */
void OSClearScreen (void)
{
    DDBLTFX ddbltfx;

#ifdef _WIN64
	my_zero_memory (&ddbltfx, sizeof (ddbltfx));
#else
    memset (&ddbltfx, 0, sizeof (ddbltfx));
#endif
    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwFillColor = DDColorMatch (lpDDSFront, 0);
    IDirectDrawSurface_Blt (lpDDSFront, NULL, NULL, NULL,
        DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx);
}   /* OSClearScreen */

/* clear the virtual screen */
void OSClearVirtualScreen (COLORREF c)
{
    DDBLTFX ddbltfx;
    RECT dst;

    dst.left = XShiftScreen;
    dst.top = YShiftScreen;
    dst.right = ScreenWidth + XShiftScreen;
    dst.bottom = ScreenHeight + YShiftScreen;

#ifdef _WIN64
	my_zero_memory (&ddbltfx, sizeof (ddbltfx));
#else
    memset (&ddbltfx, 0, sizeof (ddbltfx));
#endif

    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwFillColor = DDColorMatch (lpDDSBack, c);
    IDirectDrawSurface_Blt (lpDDSBack, &dst, NULL, NULL,
        DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx);
}   /* OSClearVirtualScreen */

/* fill an area with black */
void OSFillBlack (BOOL vis, RECT dst)
{
    LPDIRECTDRAWSURFACE lpDDS = (vis)? lpDDSFront : lpDDSBack;
    DDBLTFX ddbltfx;

//    if (vis)
//    {
      dst.left += XShiftScreen;
      dst.top += YShiftScreen;
      dst.right += XShiftScreen;
      dst.bottom += YShiftScreen;
//    }

#ifdef _WIN64
	my_zero_memory (&ddbltfx, sizeof (ddbltfx));
#else
    memset (&ddbltfx, 0, sizeof (ddbltfx));
#endif
    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwFillColor = DDColorMatch (lpDDS, 0);
    IDirectDrawSurface_Blt (lpDDS, &dst, NULL, NULL,
        DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx);
}   /* OSFillBlack */

/* copy (part of) virtual screen to visual screen */
void OSBlit (RECT *r)
{
    HRESULT ddrval;
    DDBLTFX ddbltfx;
    int flags = DDBLT_WAIT + DDBLT_DDFX;
    RECT sr;

    sr.left = r->left + XShiftScreen;
    sr.top = r->top + YShiftScreen;
    sr.right = r->right + XShiftScreen;
    sr.bottom = r->bottom + YShiftScreen;

#ifdef _WIN64
	my_zero_memory (&ddbltfx, sizeof (ddbltfx));
#else
    memset (&ddbltfx, 0, sizeof (ddbltfx));
#endif
    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwDDFX = DDBLTFX_NOTEARING;

    ddrval = IDirectDrawSurface_Blt
                (lpDDSFront, &sr, lpDDSBack, &sr, flags, &ddbltfx);
    if (ddrval == DDERR_SURFACELOST)
        DDRestoreAll ();
}   /* OSBlit */

/* flip pages */
void OSFlip (void)
{
    IDirectDrawSurface_Flip (lpDDSFront, NULL, DDFLIP_WAIT);
}   /* OSFlip */


/* ------------------------- bitmap functions  ------------------------- */

/* get a pointer to the GAMEBITMAPINFO structure with id BID */
static GAMEBITMAPINFO *GetGameBitmapInfo (int BID)
{
    GAMEBITMAPINFO *gbip = gbipGameBitmapInfo;
    BOOL bFound = FALSE;

    if (BID == iPrevGBIP)
        gbip = gbipPrev;
    else
    {
        while (gbip && (!bFound))
        {
            if (gbip->iBitmapID == BID)
            {
                bFound = TRUE;
                iPrevGBIP = BID;
                gbipPrev = gbip;
            }
            else
                gbip = gbip->gbipNext;
        }
    }
    return gbip;
}   /* GetGameBitmapInfo */

/* free members of a GAMEBITMAPINFO node */
static void FreeGameBitmapInfoNode (GAMEBITMAPINFO *gbip)
{
    GAMEBLOCKSEQUENCE *gbs;

    if (gbip->lpDDSBitmap)
    {
        IDirectDraw_Release (gbip->lpDDSBitmap);
        gbip->lpDDSBitmap = NULL;
    }

    if (gbip->sName)
    {
        rfree (gbip->sName);
        gbip->sName = NULL;
    }

    gbs = NULL;
    while (gbip->gbsGameBlockSequence)
    {
        gbs = gbip->gbsGameBlockSequence->gbsNext;
        if (gbip->gbsGameBlockSequence->sSequence)
        {
            rfree (gbip->gbsGameBlockSequence->sSequence);
        }
        rfree (gbip->gbsGameBlockSequence);
        gbip->gbsGameBlockSequence = gbs;
    }
}   /* FreeGameBitmapInfoNode */


/* initialize a game bitmap */
int OSInitGameBitmap (int id, char *name,
                      int bitmapwidth, int bitmapheight,
                      int blockwidth, int blockheight)
{
    HRESULT ddrval;
    DDSURFACEDESC ddsd;
    //    DDSCAPS ddscaps;     PA: ddscaps not used
    int resultcode;
    GAMEBITMAPINFO *gbip1;
    GAMEBITMAPINFO *gbip2;
    LPDIRECTDRAWSURFACE lpDDS;

    iPrevGBIP = 0;
    gbipPrev = NULL;

    if (id==0)
    {
        /* find matching id or create new id */
        GAMEBITMAPINFO *gbip = gbipGameBitmapInfo;

        id = 1;
        while (gbip)
        {
            if (lstrcmp (gbip->sName, name) == 0)
            {
                resultcode = gbip->iBitmapID;
                return resultcode;
            }
            else
            {
                if (gbip->iBitmapID >= id)
                    id = gbip->iBitmapID + 1;
                gbip = gbip->gbipNext;
            }
        }
    }

    resultcode = GR_INVALID_BITMAP_ID;
    if (!GetGameBitmapInfo (id))  /* identifier not used yet */
    {
        /* set non-used values to zero! */
#ifdef _WIN64
		my_zero_memory (&ddsd, sizeof (DDSURFACEDESC));
#else
	    memset (&ddsd, 0, sizeof (DDSURFACEDESC));
#endif
        ddsd.dwSize = sizeof (ddsd);
        ddsd.dwFlags = DDSD_CAPS |
                       DDSD_WIDTH |
                       DDSD_HEIGHT;

        /* must always be in systemmemory if size > screensize */
        ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN; // | DDSCAPS_SYSTEMMEMORY;
        ddsd.dwWidth = bitmapwidth;
        ddsd.dwHeight = bitmapheight;
        ddrval = IDirectDraw_CreateSurface (lpDD, &ddsd, &lpDDS, NULL);

        resultcode = GR_NOT_FOUND;
        lpDDPal = DDLoadPalette (lpDD, name);
        if (lpDDPal)
        {
            ddrval = DDReLoadBitmap (lpDDS, name);
            if (ddrval == DD_OK)
            {
                resultcode = GR_OS_ERROR;
                if (ddrval == DD_OK)
                {
                    /* find last element of linked list */
                    gbip1 = gbipGameBitmapInfo;
                    gbip2 = NULL;
                    while (gbip1)
                    {
                        gbip2 = gbip1;
                        gbip1 = gbip1->gbipNext;
                    }

                    /* create new node */
                    gbip1 = rmalloc (sizeof (GAMEBITMAPINFO));
                    gbip1->iBitmapID = id;
                    gbip1->lpDDSBitmap = lpDDS;
                    gbip1->bTransparent = FALSE;    /* transparency can be set later */
                    gbip1->iBitmapWidth = bitmapwidth;
                    gbip1->iBitmapHeight = bitmapheight;
                    gbip1->iBlockWidth = blockwidth;
                    gbip1->iBlockHeight = blockheight;
                    gbip1->iBlockCountX = bitmapwidth / blockwidth;
                    gbip1->iBlockCountY = bitmapheight / blockheight;
                    gbip1->gbsGameBlockSequence = NULL;
                    gbip1->gbipNext = NULL;
                    gbip1->sName = rmalloc (strlen (name) + 1);
                    rsncopy (gbip1->sName, name, strlen (name));

                    /* gbip2 points to the last element or is NULL */
                    if (gbip2)
                        gbip2->gbipNext = gbip1;
                    else
                        gbipGameBitmapInfo = gbip1;  /* first element */

                    resultcode = GR_OK;
                }
            }
        }
    }
    if (resultcode == GR_OK)
        return id;
    else
        return resultcode;
}   /* OSInitGameBitmap */

/* get bitmap info */
BOOL OSGetGameBitmapInfo (int id, int *width, int *height,
                          int *blockwidth, int *blockheight,
                          int *blockcountx, int *blockcounty)
{
    GAMEBITMAPINFO *gbip;

    gbip = GetGameBitmapInfo (id);
    if (gbip)
    {
        *width = gbip->iBitmapWidth;
        *height = gbip->iBitmapHeight;
        *blockwidth = gbip->iBlockWidth;
        *blockheight = gbip->iBlockHeight;
        *blockcountx = gbip->iBlockCountX;
        *blockcounty = gbip->iBlockCountY;
         return TRUE;
   }
    else
        return FALSE;
}   /* OSGetGameBitmapInfo */


/* deinit a game bitmap */
int OSFreeGameBitmap (int id)
{
    GAMEBITMAPINFO *gbipCurrent = gbipGameBitmapInfo;
    GAMEBITMAPINFO *gbipNext = NULL;
    GAMEBITMAPINFO *gbipPrevious = NULL;
    int result;

    iPrevGBIP = 0;
    gbipPrev = NULL;

    result = GR_INVALID_BITMAP_ID;
    while (gbipCurrent)
    {
        if (gbipCurrent->iBitmapID != id)
        {
            gbipPrevious = gbipCurrent;
            gbipNext = gbipCurrent->gbipNext;
        }
        else
        {
            /* link previous node to next node */
            if (gbipPrevious)
                gbipPrevious->gbipNext = gbipCurrent->gbipNext;
            else
                gbipGameBitmapInfo = gbipCurrent->gbipNext;

            gbipNext = gbipCurrent->gbipNext;
            /* free the current node */

            FreeGameBitmapInfoNode (gbipCurrent);
            rfree (gbipCurrent);

            result = GR_OK;
        }
        gbipCurrent = gbipNext;
    }
    return result;
}   /* OSFreeGameBitmap */

/* deinit all game bitmaps */
void OSFreeGameBitmaps (void)
{
    GAMEBITMAPINFO *gbip;

    while (gbipGameBitmapInfo)
    {
        gbip = gbipGameBitmapInfo->gbipNext;
        FreeGameBitmapInfoNode (gbipGameBitmapInfo);
        rfree (gbipGameBitmapInfo);
        gbipGameBitmapInfo = gbip;
    }
    iPrevGBIP = 0;
    gbipPrev = NULL;
}   /* OSFreeGameBitmaps */


/* set transparent color */
int OSSetTransparentColor (int id, int x, int y)
{
    HDC hdc;
    COLORREF rgb;
    int result;
    GAMEBITMAPINFO *gbip = GetGameBitmapInfo (id);

    result = GR_INVALID_BITMAP_ID;
    if (gbip)
    {
        if (x < 0) x += gbip->iBitmapWidth;
        if (y < 0) y += gbip->iBitmapHeight;

        result = GR_OS_ERROR;
        if (IDirectDrawSurface_GetDC (gbip->lpDDSBitmap, &hdc) == DD_OK)
        {
            rgb = GetPixel (hdc, x, y);
            IDirectDrawSurface_ReleaseDC (gbip->lpDDSBitmap, hdc);

            DDSetColorKey (gbip->lpDDSBitmap, rgb);
            gbip->bTransparent = TRUE;

            // DDSetColorKey (gbip->lpDDSFront, rgb);
            result = GR_OK;
        }
    }
    return result;
}   /* OSSetTransparentColor */

/* initialize a block sequence */
int OSInitBlockSequence (int bitmapid, int seqid, char *seq, int len)
{
    GAMEBITMAPINFO *gbip = GetGameBitmapInfo (bitmapid);
    GAMEBLOCKSEQUENCE *gbsNew;
    GAMEBLOCKSEQUENCE *gbs1;
    GAMEBLOCKSEQUENCE *gbs2;
    int resultcode;

    resultcode = GR_INVALID_BITMAP_ID;
    if (gbip)
    {
        gbsNew = rmalloc (sizeof (GAMEBLOCKSEQUENCE));
        gbsNew->iSequenceID = seqid;
        gbsNew->iSequenceLength = len / (2 * sizeof (int));
        gbsNew->sSequence = rmalloc (len + 1);
        rsncopy (gbsNew->sSequence, seq, len);
        gbsNew->iPosition = gbsNew->iSequenceLength;
        gbsNew->iCounter = 0;
        gbsNew->gbsNext = NULL;

        gbs1 = gbip->gbsGameBlockSequence;
        gbs2 = NULL;
        while (gbs1)
        {
            gbs2 = gbs1;
            gbs1 = gbs1->gbsNext;
        }
        if (gbs2)
            gbs2->gbsNext = gbsNew;
        else
            gbip->gbsGameBlockSequence = gbsNew;
        resultcode = GR_OK;
    }
    return resultcode;
}   /* OSInitBlockSequence */

/* run block sequences */
void OSRunBlockSequences (void)
{
    GAMEBITMAPINFO *gbip;
    GAMEBLOCKSEQUENCE *gbs;
    int i;

    gbip = gbipGameBitmapInfo;
    while (gbip)
    {
        gbs = gbip->gbsGameBlockSequence;
        while (gbs)
        {
            if (--gbs->iCounter <= 0)
            {
                gbs->iPosition++;
                if (gbs->iPosition >= gbs->iSequenceLength)
                    gbs->iPosition = 0;
                i = (2 * gbs->iPosition * sizeof (int)) + sizeof (int);
                gbs->iCounter = (*(int *) &gbs->sSequence[i]);
            }
            gbs = gbs->gbsNext;
        }
        gbip = gbip->gbipNext;
    }
}   /* OSRunBlockSequences */

/* get current block */
int OSGetCurrentBlock (int bitmapid, int seqid)
{
    GAMEBITMAPINFO *gbip;
    GAMEBLOCKSEQUENCE *gbs;
    int result = 0;

    gbip = GetGameBitmapInfo (bitmapid);
    gbs = gbip->gbsGameBlockSequence;
    while ((gbs) && (seqid < 0))
    {
        if (gbs->iSequenceID == seqid)
        {
            int mappos;

            mappos = 2 * sizeof (int) * gbs->iPosition;
            result = (*(int *) &gbs->sSequence[mappos]);
        }
        gbs = gbs->gbsNext;
    }
    return result;
}   /* OSGetCurrentBlock */

/* draw part of a bitmap to virtual screen */
void OSDraw (RECT *dst, int id, RECT *src, BOOL mirlr, BOOL mirud, int flags)
{
    HRESULT ddrval;
    DDBLTFX ddbltfx;
    int bltflags, bltfastflags;
    BOOL fast;
    static int i;
    GAMEBITMAPINFO *gbip = GetGameBitmapInfo (id);
    RECT r;

    r.left = dst->left + XShiftScreen;
    r.top = dst->top + YShiftScreen;
    r.right = dst->right + XShiftScreen;
    r.bottom = dst->bottom + YShiftScreen;

    if (gbip)
    {
        fast = TRUE;

        bltfastflags = DDBLTFAST_WAIT;
        bltflags = DDBLT_WAIT | DDBLT_DDFX;

        if (gbip->bTransparent)
        {
            bltflags |= DDBLT_KEYSRC;
            bltfastflags |= DDBLTFAST_SRCCOLORKEY;
        }

      //  ddbltfx.dwDDFX = 0;
#ifdef _WIN64
		my_zero_memory (&ddbltfx, sizeof (ddbltfx));
#else
        memset (&ddbltfx, 0, sizeof (ddbltfx));
#endif
        ddbltfx.dwSize = sizeof (ddbltfx);

        if (mirlr)
        {
            ddbltfx.dwDDFX |= DDBLTFX_MIRRORLEFTRIGHT;
            fast = FALSE;
        }
        if (mirud)
        {
            ddbltfx.dwDDFX |= DDBLTFX_MIRRORUPDOWN;
            fast = FALSE;
        }

        if (flags & DO_ROTATE_90)
        {
            ddbltfx.dwDDFX |= DDBLTFX_ROTATE90;
            fast = FALSE;
        }
        if (flags & DO_ROTATE_180)
        {
            ddbltfx.dwDDFX |= DDBLTFX_ROTATE180;
            fast = FALSE;
        }
        if (flags & DO_ROTATE_270)
        {
            ddbltfx.dwDDFX |= DDBLTFX_ROTATE270;
            fast = FALSE;
        }

        if (fast)
            ddrval = IDirectDrawSurface_BltFast (lpDDSBack,
                         r.left, r.top, gbip->lpDDSBitmap, src,
                         bltfastflags);
        else
            ddrval = IDirectDrawSurface_Blt (lpDDSBack, &r,
                         gbip->lpDDSBitmap, src, bltflags, &ddbltfx);
        if (ddrval == DDERR_SURFACELOST)
            DDRestoreAll ();
    }
}   /* OSDraw */

/* ------------------------- sound functions ------------------------- */

typedef struct SOUNDSAMPLEINFO
{
    int iSampleID;
    HSNDOBJ hsoSoundBuffer;
    struct SOUNDSAMPLEINFO *ssiNext;
} SOUNDSAMPLEINFO;

static BOOL bSoundEnabled = FALSE;
static LPDIRECTSOUND lpDS = NULL;
static SOUNDSAMPLEINFO *ssiSoundSampleInfo = NULL;


/* get a pointer to the SOUNDSAMPLEINFO structure with id ID */
static SOUNDSAMPLEINFO *GetSoundSampleInfo (int ID)
{
    SOUNDSAMPLEINFO *ssi = ssiSoundSampleInfo;
    BOOL bFound = FALSE;

    while (ssi && (!bFound))
    {
        if (ssi->iSampleID == ID)
            bFound = TRUE;
        else
            ssi = ssi->ssiNext;
    }
    return ssi;
}   /* GetSoundSampleInfo */

BOOL OSInitSound (void)
{
    bSoundEnabled = FALSE;
    if (!(DirectSoundCreate (NULL, &lpDS, NULL) == DS_OK))
    {
       // MessageBox (NULL, "DirectSoundCreate failed", NULL, MB_OK);
    }
    else
    {
        if (!(IDirectSound_SetCooperativeLevel (lpDS, ghGameWindow,
                 DSSCL_NORMAL) == DS_OK))
        {
           // MessageBox (NULL, "IDirectSound_SetCooperativeLevel failed", NULL, MB_OK);
        }
        else
        {
            bSoundEnabled = TRUE;
        }
    }
    return bSoundEnabled;
}   /* OSInitSound */

/* deinitialize sound */
void OSDeInitSound (void)
{
    if (lpDS != NULL)
    {
        IDirectSound_Release (lpDS);
        lpDS = NULL;
    }
    bSoundEnabled = FALSE;
}   /* OSDeInitSound */

/* initialize a sound sample */
BOOL OSInitSoundSample (int id, char *name, int buffers)
{
    SOUNDSAMPLEINFO *ssi;

    if (bSoundEnabled)
    {
        ssi = rmalloc (sizeof (SOUNDSAMPLEINFO));
        ssi->iSampleID = id;
        ssi->hsoSoundBuffer = SndObjCreate (lpDS, name, buffers);
        ssi->ssiNext = ssiSoundSampleInfo;
        ssiSoundSampleInfo = ssi;
        return TRUE;
    }
    else
        return FALSE;
}   /* OSInitSoundSample */

/* free the SOUNDSAMPLEINFO list */
void OSFreeSoundSamples (void)
{
    SOUNDSAMPLEINFO *ssi;

    while (ssiSoundSampleInfo)
    {
        ssi = ssiSoundSampleInfo->ssiNext;
        if (ssiSoundSampleInfo->hsoSoundBuffer)
        {
            SndObjDestroy (ssiSoundSampleInfo->hsoSoundBuffer);
            ssiSoundSampleInfo->hsoSoundBuffer = NULL;
        }

        rfree (ssiSoundSampleInfo);
        ssiSoundSampleInfo = ssi;
    }
}   /* OSFreeSoundSamples */

/* play a sound sample */
BOOL OSPlaySoundSample (int id, int volume, int pan, int freq)
{
    SOUNDSAMPLEINFO *ssi = GetSoundSampleInfo (id);

    if (bSoundEnabled)
    {
        if (ssi)
        {
            IDirectSoundBuffer *pDSB = SndObjGetFreeBuffer (ssi->hsoSoundBuffer);

            if (pDSB)
            {
                IDirectSoundBuffer_SetVolume (pDSB, (LONG) volume);
                IDirectSoundBuffer_SetPan (pDSB, (LONG) pan);
                IDirectSoundBuffer_SetFrequency (pDSB, (LONG) freq);
                IDirectSoundBuffer_Play (pDSB, 0, 0, 0);
            }
        }
    }
    return bSoundEnabled;
}   /* OSPlaySoundSample */

/* start playing music in the background */
BOOL OSPlayMusic (char *midifile, BOOL restart)
{
    char buf[256];
    BOOL result = FALSE;

    if (midifile)
    {
        wsprintf (buf, "open %s type sequencer alias MUSIC", midifile);
        mciSendString ("close all", NULL, 0, NULL);

        if (mciSendString (buf, NULL, 0, NULL) == 0)
        {
            if (restart)
            {
                if (mciSendString ("play MUSIC from 0 notify", NULL, 0, ghGameWindow) == 0)
                    result = TRUE;
            }
            else
            {
                if (mciSendString ("play MUSIC from 0", NULL, 0, ghGameWindow) == 0)
                    result = TRUE;
            }
        }
    }
    else
        if (mciSendString ("play MUSIC from 0 notify", NULL, 0, ghGameWindow) == 0)
            result = TRUE;

    return result;
}   /* OSPlayMusic */

/* stop music */
BOOL OSStopMusic (void)
{
    BOOL result = FALSE;
    if (mciSendString ("close all", NULL, 0, NULL) == 0)
        result = TRUE;
    return result;
}   /* OSStopMusic */
