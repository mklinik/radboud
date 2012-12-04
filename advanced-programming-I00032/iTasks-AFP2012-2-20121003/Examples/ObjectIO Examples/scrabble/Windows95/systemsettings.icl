implementation module systemsettings


import	StdPictureDef


/*	This module contains macro's to make the scrabble application platform customisable.
*/


//	For graphics:

//	Font information:

font size			:== {SansSerifFontDef & fSize=8}
letterfont			:== {    SerifFontDef & fSize=9,fStyles=[BoldStyle]}
smallfont			:== {    SmallFontDef & fSize=6}

//	Background colour:

rbBackground		:==	LightGrey
