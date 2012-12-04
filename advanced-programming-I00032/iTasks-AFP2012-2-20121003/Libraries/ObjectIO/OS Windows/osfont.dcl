definition module osfont


//	Clean Object I/O library, version 1.2


from	StdOverloaded	import class ==
from	ostoolbox		import :: OSToolbox
from	ostypes			import :: OSPictContext


::	Font
::	OSFont
	=	{	osfontname	:: !String	// Name of the font
		,	osfontstyles:: !Int		// Style variations of the font
		,	osfontsize	:: !Int		// Point size of the font
		}
::	OSFontDef
	:==	(	!String					// Name of the font
		,	![String]				// Style variations of the font
		,	!Int					// Point size of the font
		)

instance == OSFont	// Equality on all fields

//	Font constants:
osSerifFontDef           :: OSFontDef
osSansSerifFontDef       :: OSFontDef
osSmallFontDef           :: OSFontDef
osNonProportionalFontDef :: OSFontDef
osSymbolFontDef          :: OSFontDef

/*	osSelectfont fontdef
		creates a font of the given name, style variations, and size in points.
		If successful, the Bool is True, and the Font contains a useful value.
		Otherwise, the Bool is False, and the Font is a dummy value.
	osDefaultfont 
		returns the default window text font.
	osDialogfont
		returns the default dialog text font.
	osFontgetdef
		returns the requested name, style variations, and size in points of the given font.
	osFontgetimp
		returns the internal representation of the font.
*/
osSelectfont             :: !OSFontDef					!*OSToolbox -> (!Bool,!Font,           !*OSToolbox)
osDefaultfont            ::								!*OSToolbox -> (      !Font,           !*OSToolbox)
osDialogfont             ::								!*OSToolbox -> (      !Font,           !*OSToolbox)
osFontgetdef             :: !Font                			        -> OSFontDef
osFontgetimp             :: !Font               			        -> OSFont

/*	osFontnames
		returns the set of names all currently available fonts.
	osFontstyles fontname
		returns the set of all currently available style variations for the font with the given
		fontname.
	osFontsizes x y fontname
		returns the set of all currently available sizes for the font with the given fontname 
		that lie between x and y (both inclusive).
*/
osFontnames              ::                  			!*OSToolbox -> (![String],             !*OSToolbox)
osFontstyles             :: !String         			!*OSToolbox -> (![String],             !*OSToolbox)
osFontsizes              :: !Int !Int !String			!*OSToolbox -> (![Int],                !*OSToolbox)

/*	osGetfontcharwidths hdcPassed maybeHdc chars font
		returns the widths of all given chars in the same order of the given font.
	osGetfontstringwidth hdcPassed maybeHdc string font
		returns the width of the given string of the given font.
	osGetfontstringwidths hdcPassed maybeHdc strings font
		returns the widths of all given strings in the same order of the given font.
	osGetfontmetrics hdcPassed maybeHdc font
		returns the (ascent,descent,leading,maxwidth) of the given font in that order.
*/
osGetfontcharwidths      :: !Bool !OSPictContext ![Char]   !Font !*OSToolbox -> (![Int],                !*OSToolbox)
osGetfontstringwidth     :: !Bool !OSPictContext !String   !Font !*OSToolbox -> (!Int, !*OSToolbox)
osGetfontstringwidths    :: !Bool !OSPictContext ![String] !Font !*OSToolbox -> (![Int],                !*OSToolbox)
osGetfontmetrics         :: !Bool !OSPictContext           !Font !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
