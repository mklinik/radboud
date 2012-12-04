definition module osbitmap

//	Clean object I/O library, version 1.2

import ospicture

::	Bitmap
::	OSBitmap

toBitmap	:: !OSBitmap -> Bitmap
fromBitmap	:: !Bitmap -> OSBitmap

//	osReadBitmap reads a bitmap from a file.
osReadBitmap :: !*File -> (!Bool,!OSBitmap,!*File)

//	osGetBitmapSize returns the size of the bitmap
osGetBitmapSize :: !OSBitmap -> (!Int,!Int)

//	osGetBitmapContent returns the content string of the bitmap
osGetBitmapContent :: !OSBitmap -> {#Char}

//	osGetBitmapHandle returns the handle of the bitmap
osGetBitmapHandle :: !OSBitmap -> Int

/*	osResizeBitmap (w,h) bitmap
		resizes the argument bitmap to the given size.
		It is assumed that w and h are not negative.
*/
osResizeBitmap :: !(!Int,!Int) !OSBitmap -> OSBitmap

/*	osDrawBitmap bitmap pos origin isScreenOutput pictContext
		draws the argument bitmap with the left top corner at pos, given the current origin and drawing context.
		The isScreenOutput MUST be False when producing printer output. For screen output this is not the case,
		but setting it to True is much more efficient. 
*/
osDrawBitmap :: !OSBitmap !(!Int,!Int) !(!Int,!Int) !Bool !OSPictContext !*OSToolbox -> (!OSPictContext,!*OSToolbox)
