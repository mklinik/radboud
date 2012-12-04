definition module controlrelayout


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	relayout, wstate


relayoutControls :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !(!OSRect,!Point2,!Vector2,![WElementHandle`])
                                                                                !(!OSRect,!Point2,!Vector2,!*[WElementHandle .ls .pst]) 
                                                                                !*OSToolbox
                                   -> (!OSRgnHandle,!*[WElementHandle .ls .pst],!*OSToolbox)
relayoutControls`:: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !(!OSRect,!Point2,!Vector2,![WElementHandle`])
                                                                                !(!OSRect,!Point2,!Vector2,![WElementHandle`]) 
                                                                                !*OSToolbox
                                                               -> (!OSRgnHandle,!*OSToolbox)
/*	relayoutControls(`) wMetrics guiPtr defaultId withinCompound isAble isVisible (oldFrame,oldParentPos,oldCompoundPos,old) 
	                                                                              (newFrame,newParentPos,newCompoundPos,new)
	resizes, moves, and updates changed WElementHandle(`)s. 
		guiPtr               :: OSWindowPtr             is the parent window/compound.
		defaultId            :: Maybe Id                is the optional Id of the default control.
		withinCompound       :: Bool                    is True iff the elements are inside a CompoundControl.
		isAble               :: Bool                    is True iff the parent window/compound is Able.
		isVisible            :: Bool                    is True iff the the elements are in a visible window/compound/layout.
		oldFrame             :: OSRect                  is the clipping rect of the parent window/compound at the original location and size.
		newFrame             :: OSRect                  is the clipping rect of the parent window/compound at the new location and size.
		(old/new)ParentPos   :: Point2                  are the positions of the respective parent window/compound/layout of the elements.
		(old/new)CompoundPos :: Vector2                 are the positions of the respective parent window/compound        of the elements.
		old                  :: [WElementHandle`]       contains the elements at their original location and size.
		new                  :: [WElementHandle ls pst] (relayoutControls)
		                     :: [WElementHandle`]       (relayoutControls`)
		                                                contains the elements at their new location and size.
	relayoutControls(`) assumes that old and new contain elements that are identical except for size and position.
		If this is not the case, a runtime error will occur.
	relayoutControls(`) assumes that the ClipStates of all compound elements are valid.
	The return OSRgnHandle is the area of the window that requires to be updated (use updatewindowbackgrounds [windowupdate] for this purpose).
*/
