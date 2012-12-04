implementation module keyfocus


import	StdBool, StdFunc, StdList, StdMisc
import	commondef
import	StdMaybe


::	KeyFocus
	=	{	kfItem		:: !Maybe Int		// Case (Just nr): the item with (wItemNr nr) has the keyboard input focus; Nothing: no item has focus
		,	kfItems		:: !.[FocusItem]	// The items of the window that can have the keyboard input focus
		}
::	FocusItem
	=	{	focusNr		:: !Int				// The item nr of the item
		,	focusShow	:: !Bool			// Flag: True iff item is visible
		}

isShownFocusItem :: !FocusItem -> Bool
isShownFocusItem {focusShow} = focusShow

eqFocusItemNr :: !Int !FocusItem -> Bool
eqFocusItemNr nr {focusNr} = nr==focusNr

newFocusItems :: !*[FocusItem] -> *KeyFocus
newFocusItems items
	# (found,item,items) = ucselect isShownFocusItem undef items
	= {	kfItem = if found (Just item.focusNr) Nothing
	  ,	kfItems= items
	  }

copyKeyFocus :: !*KeyFocus -> (!KeyFocus,!*KeyFocus)
copyKeyFocus kf=:{kfItem,kfItems}
	# (items`,items)	= copyFocusItems kfItems
	= ({kfItem=kfItem,kfItems=items`},{kf & kfItems=items})
where
	copyFocusItems :: !*[FocusItem] -> (![FocusItem],!*[FocusItem])
	copyFocusItems [item:items]
		# (items`,items)	= copyFocusItems items
		= ([item:items`],[item:items])
	copyFocusItems []
		= ([],[])

openFocusItems :: !(Maybe Int) !*[FocusItem] !*KeyFocus -> *KeyFocus
openFocusItems (Just behind) new kf=:{kfItems}
	= {kf & kfItems=openFocusItems` behind new kfItems}
where
	openFocusItems` :: !Int !*[FocusItem] !*[FocusItem] -> *[FocusItem]
	openFocusItems` behind new [item=:{focusNr}:items]
		| behind==focusNr	= [item:new++items]
		| otherwise			= [item:openFocusItems` behind new items]
	openFocusItems` _ new _
		= new
openFocusItems _ items kf=:{kfItems}
	= {kf & kfItems=kfItems++items}

closeFocusItems :: ![Int] !*KeyFocus -> *KeyFocus
closeFocusItems nrs kf=:{kfItem,kfItems}
	= {	kf & kfItems	= closeFocusItems` nrs kfItems
		   , kfItem		= if (isNothing kfItem) kfItem
						 (if (isMember (fromJust kfItem) nrs) Nothing kfItem)
	  }
where
	closeFocusItems` :: ![Int] !*[FocusItem] -> *[FocusItem]
	closeFocusItems` _ []
		= []
	closeFocusItems` [] _
		= []
	closeFocusItems` nrs [item:items]
		# (found,nrs)	= removeCheck item.focusNr nrs
		| found
			= closeFocusItems` nrs items
		| otherwise
			= [item:closeFocusItems` nrs items]

showFocusItems :: ![Int] !*KeyFocus -> *KeyFocus
showFocusItems nrs kf=:{kfItems}
	= {kf & kfItems=setShowFocusItems True nrs kfItems}

setShowFocusItems :: !Bool ![Int] !*[FocusItem] -> *[FocusItem]
setShowFocusItems show [] _
	= []
setShowFocusItems show _ []
	= []
setShowFocusItems show nrs [item:items]
	# (found,nrs)	= removeCheck item.focusNr nrs
	| found
		= [{item & focusShow=show}:setShowFocusItems show nrs items]
	| otherwise
		= [item:setShowFocusItems show nrs items]

hideFocusItems :: ![Int] !*KeyFocus -> *KeyFocus
hideFocusItems nrs kf=:{kfItem,kfItems}
	= {	kf & kfItems	= setShowFocusItems False nrs kfItems
		   , kfItem		= if (isNothing kfItem) kfItem
						 (if (isMember (fromJust kfItem) nrs) Nothing kfItem)
	  }

getCurrentFocusItem :: !*KeyFocus -> (!Maybe Int,!*KeyFocus)
getCurrentFocusItem kf=:{kfItem} = (kfItem,kf)

setNoFocusItem :: !*KeyFocus -> *KeyFocus
setNoFocusItem kf = {kf & kfItem=Nothing}

setNewFocusItem :: !Int !*KeyFocus -> *KeyFocus
setNewFocusItem new kf
	= {kf & kfItem=Just new}

setNextFocusItem :: !(Maybe Int) !*KeyFocus -> (!Maybe Int,!*KeyFocus)
setNextFocusItem (Just behind) kf=:{kfItems}
	# (before,item_after)		= span (not o (eqFocusItemNr behind)) kfItems
	# (no_after,item_after)		= uisEmpty item_after
	| no_after
		= (Nothing,{kf & kfItems=before})
	# (a,tl_item_after)			= hdtl item_after
	# (found,item,tl_item_after)= ucselect isShownFocusItem undef tl_item_after
	| found
		= (Just item.focusNr,{kf & kfItem=Just item.focusNr,kfItems=before++[a:tl_item_after]})
	# (found,item,before)		= ucselect isShownFocusItem undef before
	| found
		= (Just item.focusNr,{kf & kfItem=Just item.focusNr,kfItems=before++[a:tl_item_after]})
	| otherwise
		= (Nothing,{kf & kfItems=before++[a:tl_item_after]})
setNextFocusItem _ kf=:{kfItems}
	# (found,item,items)		= ucselect isShownFocusItem undef kfItems
	| not found
		= (Nothing,{kf & kfItem=Nothing,kfItems=items})
	| otherwise
		= (Just item.focusNr,{kf & kfItem=Just item.focusNr,kfItems=items})
