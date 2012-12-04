implementation module StdBitmap


import	StdBool, StdFile, StdInt
import	osbitmap, ospicture
import	commondef
import	StdMaybe, StdPicture


openBitmap :: !{#Char} !*env -> (!Maybe Bitmap,!*env)	| FileSystem env
openBitmap name env
	# (ok,file,env)		= fopen name FReadData env
	| not ok
		= (Nothing,env)
	# (ok,osBitmap,file)= osReadBitmap file
    # (_,env)			= fclose file env
    | not ok
    	= (Nothing,env)
    | otherwise
		= (Just (toBitmap osBitmap),env)

getBitmapSize :: !Bitmap -> Size
getBitmapSize bitmap
	= fromTuple (osGetBitmapSize (fromBitmap bitmap))

resizeBitmap :: !Size !Bitmap -> Bitmap
resizeBitmap size=:{w,h} bitmap
	| w<0 || h<0
		= error "resizeBitmap" "StdBitmap" "a Size record with negative components was passed"
	| otherwise
		= toBitmap (osResizeBitmap (w,h) (fromBitmap bitmap))

instance Drawables Bitmap where
	draw :: !Bitmap !*Picture -> *Picture
	draw bitmap picture
		# (origin,pen,toScreen,pictContext,tb)	= peekPicture picture
		  (penPos,pen)							= getPenPenPos pen	// kan ook met getpictpenpos...
		# (pictContext,tb)						= osDrawBitmap (fromBitmap bitmap) (toTuple penPos) (toTuple origin) toScreen pictContext tb
		= unpeekPicture origin pen toScreen pictContext tb
	
	drawAt :: !Point2 !Bitmap !*Picture -> *Picture
	drawAt pos bitmap picture
		# (origin,pen,toScreen,pictContext,tb)	= peekPicture picture
		# (pictContext,tb)						= osDrawBitmap (fromBitmap bitmap) (toTuple pos) (toTuple origin) toScreen pictContext tb
		= unpeekPicture origin pen toScreen pictContext tb
	
	undraw :: !Bitmap !*Picture -> *Picture
	undraw bitmap picture
		= unfill {box_w=w,box_h=h} picture
	where
		(w,h)	= osGetBitmapSize (fromBitmap bitmap)
	
	undrawAt :: !Point2 !Bitmap !*Picture -> *Picture
	undrawAt pos bitmap picture
		= unfillAt pos {box_w=w,box_h=h} picture
	where
		(w,h)	= osGetBitmapSize (fromBitmap bitmap)
