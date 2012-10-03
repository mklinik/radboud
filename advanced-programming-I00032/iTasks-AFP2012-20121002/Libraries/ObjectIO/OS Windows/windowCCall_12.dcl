definition module windowCCall_12


from	ostoolbox		import :: OSToolbox
from	ostypes			import :: HWND
from	rgnCCall_12		import :: HRGN
from	pictCCall_12	import :: HDC


winInitialiseWindows:: !*OSToolbox -> *OSToolbox
winInvalidateWindow	:: !HWND !*OSToolbox -> *OSToolbox
winInvalidateRect	:: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
winValidateRect		:: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
winValidateRgn		:: !HWND !HRGN !*OSToolbox -> *OSToolbox

winGetDC			:: !HWND !*OSToolbox -> (!HDC,!*OSToolbox)
winReleaseDC		:: !HWND !(!HDC,!*OSToolbox) -> *OSToolbox
