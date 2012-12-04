implementation module windowCCall_12


from	ostoolbox		import :: OSToolbox
from	ostypes			import :: HWND
from	rgnCCall_12		import :: HRGN
from	pictCCall_12	import :: HDC
import code from "cCCallWindows_121.","cpicture_121."


winInitialiseWindows:: !*OSToolbox -> *OSToolbox
winInitialiseWindows _
	= code
	{
		.inline InstallCrossCallWindows
			ccall InstallCrossCallWindows "I-I"
		.end
	}

winInvalidateWindow :: !HWND !*OSToolbox -> *OSToolbox
winInvalidateWindow _ _
	= code
	{	
		.inline WinInvalidateWindow
			ccall WinInvalidateWindow "II-I"
		.end
	}

winInvalidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
winInvalidateRect hwnd (left,top, right,bottom) tb
	= code
	{	
		.inline WinInvalidateRect
			ccall WinInvalidateRect "IIIIII-I"
		.end
	}

winValidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
winValidateRect hwnd (left,top, right,bottom) tb
	= code
	{	
		.inline WinValidateRect
			ccall WinValidateRect "IIIIII-I"
		.end
	}

winValidateRgn :: !HWND !HRGN !*OSToolbox -> *OSToolbox
winValidateRgn hwnd rgn tb
	= code
	{	
		.inline WinValidateRgn
			ccall WinValidateRgn "III-I"
		.end
	}

winGetDC :: !HWND !*OSToolbox -> (!HDC,!*OSToolbox)
winGetDC _ _
	= code
	{
		.inline WinGetDC
			ccall WinGetDC "II-II"
		.end
	}

winReleaseDC :: !HWND !(!HDC,!*OSToolbox) -> *OSToolbox
winReleaseDC hwnd (hdc,tb)
	= code
	{
		.inline WinReleaseDC
			ccall WinReleaseDC "III-I"
		.end
	}
