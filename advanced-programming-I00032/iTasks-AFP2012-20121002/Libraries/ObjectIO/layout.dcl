definition module layout


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	StdIOCommon
from	windowhandle	import :: LayoutInfo


::	LayoutItem
	=	{	liId		:: !Id					// The Id      of the item
		,	liItemPos	:: !ItemPos				// The ItemPos of the item
		,	liItemSize	:: !Size				// The Size    of the item
		}
::	Root
	=	{	rootItem	:: !LayoutItem			// The original item that has been laid out
		,	rootPos		:: !Vector2				// The exact location of the item relative to current origin
		,	rootTree	:: ![Relative]			// The dependent items
		}
::	Relative
	=	{	relativeItem:: !LayoutItem			// The original item that has been laid out
		,	relativePos	:: !Vector2				// The exact location of the item relative to current origin
		}

getLayoutItem			:: !Id ![Root] -> (!LayoutInfo,!Vector2,!Size,![Root])
/*	getLayoutItem id roots
		retrieves the position (Vector2) and size (Size) of the item identified by id.
		In case the item is a relative, it is removed from the [Root].
		In case the item is a root, it is removed only if it has an empty layout tree.
		The LayoutInfo classifies the ItemPos of the layout root of the retrieved item.
	getLayoutItem returns a runtime error in case no item could be identified.
*/

layoutItems				:: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![LayoutItem] -> (!Size,![Root])
/*	layoutItems calculates the layout of the items given the horizontal and vertical margins, item spaces,
	requested size, minimum size. The list of (ViewDomain,Point2) is the current view domains and origins of all 
	parent objects in ascending order.
*/

itemPosOffset			:: !ItemOffset [(ViewDomain,Origin)] -> Alt2 Vector2 Alignment
/*	itemPosOffset calculates the actual offset vector of the given ItemOffset value.
*/
