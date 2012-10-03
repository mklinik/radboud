implementation module osfont


import	StdBool, StdClass, StdEnum, StdReal
import	clCrossCall_12, pictCCall_12
from	clCCall_12		import winMakeCString, winGetCString, :: CSTR, winGetVertResolution
from	StdPictureDef	import :: FontName, :: FontSize, :: FontStyle, BoldStyle, ItalicsStyle, UnderlinedStyle
from	commondef		import fatalError, isBetween, minmax, stateMap
from	ostypes			import :: OSPictContext, :: HDC


::	Font
	=	{	fontdef	:: !OSFontDef	// The font requested by the program
		,	fontimp	:: !OSFont		// The font selected  by the system
		}
::	OSFont
	=	{	osfontname	:: !String	// Name of the font
		,	osfontstyles:: !Int		// Style variations of the font
		,	osfontsize	:: !Int		// Size of the font
		}
::	OSFontDef
	:==	(	!String					// Name of the font
		,	![String]				// Style variations of the font
		,	!Int					// Point size of the font
		)

instance == OSFont where
	(==) :: !OSFont !OSFont -> Bool
	(==) f1 f2 = f1.osfontsize==f2.osfontsize && f1.osfontstyles==f2.osfontstyles && f1.osfontname==f2.osfontname

//	Font constants:
osSerifFontDef           :: OSFontDef;	osSerifFontDef			= ("Times New Roman",[],10)
osSansSerifFontDef       :: OSFontDef;	osSansSerifFontDef		= ("Arial",          [],10)
osSmallFontDef           :: OSFontDef;	osSmallFontDef			= ("Small Fonts",    [],7 )
osNonProportionalFontDef :: OSFontDef;	osNonProportionalFontDef= ("Courier New",    [],10)
osSymbolFontDef          :: OSFontDef;	osSymbolFontDef			= ("Symbol",         [],10)


osSelectfont :: !OSFontDef !*OSToolbox -> (!Bool,!Font,!*OSToolbox)
osSelectfont fdef=:(fName,fStyles,fSize) tb
	= (True,{fontdef=fdef,fontimp=fimp},tb)
where
	fimp	= {osfontname=fName,osfontstyles=sStyle2IStyle fStyles,osfontsize=fSize}

osDefaultfont :: !*OSToolbox -> (!Font,!*OSToolbox)
osDefaultfont tb
	= ({fontdef=def,fontimp=imp},tb)
where
	def		= (name,styles,size)
	imp		= {osfontname=name,osfontstyles=sStyle2IStyle styles,osfontsize=size}
	name	= "Times New Roman"
	styles	= []
	size	= 10

osDialogfont :: !*OSToolbox -> (!Font,!*OSToolbox)
osDialogfont tb
	= ({fontdef=def,fontimp=imp},tb)
where
	def		= (name,styles,size)
	imp		= {osfontname=name,osfontstyles=sStyle2IStyle styles,osfontsize=size}
	name	= "MS Sans Serif"
	styles	= []
	size	= 8

osFontgetdef :: !Font -> OSFontDef
osFontgetdef {fontdef}
	= fontdef

osFontgetimp :: !Font -> OSFont
osFontgetimp {fontimp}
	= fontimp

sStyle2IStyle :: ![FontStyle] -> Int
sStyle2IStyle styles
	= s2i styles 0
where
	s2i []                         i = i
	s2i [ BoldStyle       : rest ] i = s2i rest (i bitor iBold)
	s2i [ ItalicsStyle    : rest ] i = s2i rest (i bitor iItalic)
	s2i [ UnderlinedStyle : rest ] i = s2i rest (i bitor iUnderline)
 	s2i [ _               : rest ] i = s2i rest i

iStyle2SStyle :: !Int -> [FontStyle]
iStyle2SStyle istyle
	= idtofontstyles` istyle [iBold,iItalic,iUnderline,iStrikeOut]
where
	idtofontstyles` :: !Int ![Int] -> [String]
	idtofontstyles` 0 _
		= []
	idtofontstyles` istyle [styleflag:styleflags]
		| notStyleFlag	= styles
		| otherwise		= [style:styles]
	where
		notStyleFlag	= istyle bitand styleflag == 0
		styles			= idtofontstyles` (istyle-styleflag) styleflags
		style			= if (styleflag==iBold)      BoldStyle
						 (if (styleflag==iItalic)    ItalicsStyle
						 (if (styleflag==iUnderline) UnderlinedStyle
						                             (fatalError "iStyle2SStyle" "osfont"
																 "unmatched styleflag value ("+++toString styleflag+++")"
													 )))
	idtofontstyles` _ _
		= []


osFontnames :: !*OSToolbox -> (![String], !*OSToolbox)
osFontnames tb
	# getFontNamesCci		= {ccMsg=CcRqGETFONTNAMES,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0}
	# (_,unsortednames,tb)	= issueCleanRequest fontnamesCallback getFontNamesCci [] tb
	= (sortAndRemoveDuplicates unsortednames,tb)
where
	fontnamesCallback :: !CrossCallInfo ![FontName] !*OSToolbox -> (!CrossCallInfo,![String],!*OSToolbox)
	fontnamesCallback cci names os
		# (newname,os) = winGetCString cci.p1 os
		= (return0Cci,[newname:names],os)

sortAndRemoveDuplicates :: !u:[a] -> u:[a] | Ord a
sortAndRemoveDuplicates [e:es]
	= insert e (sortAndRemoveDuplicates es)
where
	insert :: a !u:[a] -> u:[a] | Ord a
	insert a list=:[b:x]
		| a<b		= [a:list]
		| a>b		= [b:insert a x]
		| otherwise	= list
	insert a _
		= [a]
sortAndRemoveDuplicates _
	= []


osFontstyles :: !String !*OSToolbox -> (![String],!*OSToolbox)
osFontstyles fname tb
	= ([BoldStyle,ItalicsStyle,UnderlinedStyle],tb)

osFontsizes :: !Int !Int !String !*OSToolbox -> (![Int],!*OSToolbox)
osFontsizes between1 between2 fname tb
	# (textptr,tb)			= winMakeCString fname tb
	  getFontSizesCci		= {ccMsg=CcRqGETFONTSIZES,p1=textptr,p2=0,p3=0,p4=0,p5=0,p6=0}
	# (_,unsortedsizes,tb)	= issueCleanRequest fontSizesCallback getFontSizesCci [] tb
	= (sortAndRemoveDuplicates unsortedsizes,tb)
where
	(low,high)	= minmax between1 between2
	
	fontSizesCallback :: !CrossCallInfo ![FontSize] !*OSToolbox -> (!CrossCallInfo,![FontSize],!*OSToolbox)
	fontSizesCallback cci=:{p1=size,p2=0} sizes tb
		= (return0Cci,newsizes,tb)
	where
		pts		= height2Points size
		newsizes= if (isBetween pts low high)
					 [pts:sizes]
					 sizes
	fontSizesCallback _ _ tb
		= (return0Cci,[low..high],tb)

height2Points :: !Int -> Int
height2Points h
	= toInt points
where
	dpi		= toReal winGetVertResolution
	phfactor= dpi / 72.0
	points	= toReal h / phfactor

/* XXX MW: probably not called anywhere
points2Height :: !Int -> Int
points2Height p
	= toInt height
where
	dpi		= toReal winGetVertResolution
	phfactor= dpi / 72.0
	height	= toReal p * phfactor
*/

osGetfontcharwidths :: !Bool !OSPictContext ![Char] !Font !*OSToolbox -> (![Int], !*OSToolbox)
osGetfontcharwidths hdcPassed maybeHdc chars {fontimp={osfontname,osfontstyles,osfontsize}} tb
	= stateMap (\c tb->winGetCharWidth c (osfontname,osfontstyles,osfontsize) (toInt hdcPassed) maybeHdc tb) chars tb

osGetfontstringwidth :: !Bool !OSPictContext !String !Font !*OSToolbox -> (!Int, !*OSToolbox)
osGetfontstringwidth hdcPassed maybeHdc string {fontimp={osfontname,osfontstyles,osfontsize}} tb
	= winGetStringWidth string (osfontname,osfontstyles,osfontsize) (toInt hdcPassed) maybeHdc tb

osGetfontstringwidths :: !Bool !OSPictContext ![String] !Font !*OSToolbox -> (![Int], !*OSToolbox)
osGetfontstringwidths hdcPassed maybeHdc strings {fontimp={osfontname,osfontstyles,osfontsize}} tb
	= stateMap (\s tb->winGetStringWidth s (osfontname,osfontstyles,osfontsize) (toInt hdcPassed) maybeHdc tb) strings tb

osGetfontmetrics :: !Bool !OSPictContext !Font !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
osGetfontmetrics hdcPassed maybeHdc {fontimp={osfontname,osfontstyles,osfontsize}} tb
	# (ascent,descent,maxwidth,leading,tb) = winGetFontInfo (osfontname,osfontstyles,osfontsize) (toInt hdcPassed) maybeHdc tb
	= ((ascent,descent,leading,maxwidth),tb)
