definition module relayout

//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

import	windowhandle
import	osfont, ossystem, ostoolbox, ostypes


::	RelayoutItem
	=	{	rliItemKind		:: !ControlKind			// The control kind
		,	rliItemPtr		:: !OSWindowPtr			// The ptr to the item
		,	rliItemPos		:: !Vector2				// The exact position of the item
		,	rliItemSize		:: !Size				// The exact size of the item
		,	rliItemSelect	:: !Bool				// The item is Able (True) or Unable (False)
		,	rliItemShow		:: !Bool				// The item is visible (True) or invisible (False)
		,	rliItemInfo		:: CompoundInfo			// If the control kind is IsCompoundControl: its CompoundInfo; otherwise undefined
		,	rliItemLook		:: LookInfo				// If the control kind is IsCustom(Button)Control: its LookInfo; otherwise undefined
		,	rliItems		:: ![RelayoutItem]		// If the control kind is Is(Compound/Layout)Control: its elements; otherwise: []
		}

relayoutItems :: !OSWindowMetrics !OSWindowPtr !Bool !(!OSRect,!Point2,!Vector2,![RelayoutItem]) 
                                                     !(!OSRect,!Point2,!Vector2,![RelayoutItem])
                                                     !*OSToolbox
                                    -> (!OSRgnHandle,!*OSToolbox)
/*	relayoutItems wMetrics guiPtr withinCompound (oldFrame,oldParent,oldCompound,oldItems) (newFrame,newParent,newCompound,newItems) 
	resizes and moves changed items.
		guiPtr            :: OSWindowPtr    is the parent window/compound control.
		withinCompound    :: Bool           is True iff the elements are inside a CompoundControl.
		(old/new)Frame    :: OSRect         are the window frames in which the elements reside.
		(old/new)Parent   :: Point2         are the positions of the parent window/compound/layout.
		(old/new)Compound :: Vector2        are the positions of the parent window/compound.
		(old/new)Items    :: [RelayoutItem] contain the elements at their location and size.
	Assumptions: 
		* (old/new)Items contain elements that are identical except for size and position.
		* (Radio/Check)Controls are flattened and have rliItemKind Is(Radio/Check)Control.
		* The ClipStates of CompoundControls are valid.
	relayoutItems returns the background region that needs to be updated.
*/
