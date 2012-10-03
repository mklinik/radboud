implementation module relayout


import	StdBool, StdFunc, StdList, StdTuple
import	ospicture, osrgn, oswindow, ossystem
import	commondef, windowhandle


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

relayoutFatalError :: String String -> .x
relayoutFatalError function error
	= fatalError function "relayout" error


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
	This version uses the HDC of the parent window in order to suppress calls to initpicture.
		Regions are used to clip sibling controls.
		In addition, two regions (validRegion,invalidRegion) are maintained that administrate whether part of the window requires
		update after relayout. This is done as follows:
			* initially validRegion and invalidRegion are empty.
			* for each relayoutitem: if its oldFrame<>newFrame then it adds newFrame to validRegion, and oldFrame to invalidRegion
			* the area to be updated equals validRegion - invalidRegion (so if its empty, then no update is required)
	relayoutItems returns the background region that needs to be updated.
*/
relayoutItems :: !OSWindowMetrics !OSWindowPtr !Bool !(!OSRect,!Point2,!Vector2,![RelayoutItem])
                                                     !(!OSRect,!Point2,!Vector2,![RelayoutItem]) 
                                                     !*OSToolbox
                                    -> (!OSRgnHandle,!*OSToolbox)
relayoutItems wMetrics wPtr withinCompound (oldFrame,oldParentPos,oldCompoundPos,oldHs) (newFrame,newParentPos,newCompoundPos,newHs) tb
	#! (clipRgn,tb)		= osnewrectrgn newFrame tb
	#! (validRgn,tb)	= osnewrectrgn zero tb
	#! (invalidRgn,tb)	= osnewrectrgn zero tb
	#! (osPict,tb)		= osGrabWindowPictContext wPtr tb
	#! picture			= packPicture zero defaultPen True osPict tb
	#! ((clipRgn,validRgn,invalidRgn),picture)
						= accClipPicture (toRegion (rectToRectangle newFrame)) 
							(relayoutItems` wPtr wMetrics withinCompound newArea (oldFrame,oldParentPos,oldCompoundPos,oldHs)
																                 (newFrame,newParentPos,newCompoundPos,newHs)
																                 (clipRgn,validRgn,invalidRgn)
							) picture
	#! (_,_,_,osPict,tb)= unpackPicture picture
	#! tb				= osReleaseWindowPictContext wPtr osPict tb
	#! (updRgn,tb)		= osdiffrgn	invalidRgn validRgn tb
	#! tb				= stateMap2 osdisposergn [clipRgn,validRgn,invalidRgn] tb
	= (updRgn,tb)
where
	newArea				= subtractRects newFrame oldFrame
	
	relayoutItems` :: !OSWindowPtr !OSWindowMetrics !Bool ![OSRect] !(!OSRect,!Point2,!Vector2,![RelayoutItem]) 
	                                                                !(!OSRect,!Point2,!Vector2,![RelayoutItem])
	                                                                !(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle) !*Picture
	                                                            -> (!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle),!*Picture)
	relayoutItems` wPtr wMetrics withinCompound newArea (oldFrame,oldParentPos,oldCompoundPos,[oldH:oldHs])
	                                                    (newFrame,newParentPos,newCompoundPos,[newH:newHs]) 
	                                                    rgnHs picture
		# (rgnHs,picture)	= relayoutItem   wPtr wMetrics withinCompound newArea (oldFrame,oldParentPos,oldCompoundPos,oldH)
							                                                      (newFrame,newParentPos,newCompoundPos,newH)
							                                                      rgnHs picture
		# (rgnHs,picture)	= relayoutItems` wPtr wMetrics withinCompound newArea (oldFrame,oldParentPos,oldCompoundPos,oldHs)
							                                                      (newFrame,newParentPos,newCompoundPos,newHs)
							                                                      rgnHs picture
		= (rgnHs,picture)
	where
		relayoutItem :: !OSWindowPtr !OSWindowMetrics !Bool ![OSRect] !(!OSRect,!Point2,!Vector2,!RelayoutItem)
		                                                              !(!OSRect,!Point2,!Vector2,!RelayoutItem)
		                                                              !(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle) !*Picture
		                                                          -> (!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle),!*Picture)
		relayoutItem wPtr wMetrics withinCompound newArea old=:(_,_,_,{rliItemShow,rliItemKind=k1}) new=:(_,_,_,{rliItemKind=k2}) rgnHs picture
			| k1<>k2			= relayoutFatalError "relayoutItem" "mismatching RelayoutItems"
		//	| not rliItemShow	= (rgnHs,picture)		// the items are invisible, so nothing needs to be relayn out
			| otherwise			= relayout wPtr wMetrics withinCompound newArea k1 old new rgnHs picture
		where
			/*	relayout assumes that the two RelayoutItem arguments 
				have the same ControlKind (fourth argument) and differ only in size or position or both.
			*/
			relayout :: !OSWindowPtr !OSWindowMetrics !Bool ![OSRect] !ControlKind !(!OSRect,!Point2,!Vector2,!RelayoutItem) 
			                                                                       !(!OSRect,!Point2,!Vector2,!RelayoutItem)
			                                                                       !(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle) !*Picture
			                                                                   -> (!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle),!*Picture)
			
			relayout wPtr wMetrics withinCompound newArea IsCompoundControl (oldFrame,oldParentPos,oldCompoundPos,old)
			                                                                (newFrame,newParentPos,newCompoundPos,new)
			                                                                (clipRgn,validRgn,invalidRgn) picture
				#! picture				= apppicttoolbox (moveF o sizeF) picture
				#! picture				= updF picture
				#! picture				= apppicttoolbox updScrollbars picture		// update scrollbars AFTER moving/sizing/updating
				#! ((clipRgn,validRgn,invalidRgn),picture)
										= relayoutItems` wPtr wMetrics True newArea1 (oldFrame1,oldAbsolutePos,oldCompoundPos1,old.rliItems)
										                                             (newFrame1,newAbsolutePos,newCompoundPos1,new.rliItems)
										                                             (clipRgn,validRgn,invalidRgn) picture
				| new.rliItemShow
					#! ((validRgn,invalidRgn),picture)
										= accpicttoolbox (checkUpdateRegions oldFrame1 newFrame1 (validRgn,invalidRgn)) picture
					#! (clipRgn,picture)= accpicttoolbox (subtractRectFromRgn (intersectRects newFrame newCompoundRect) clipRgn) picture
					=  ((clipRgn,validRgn,invalidRgn),picture)
				| otherwise
					= ((clipRgn,validRgn,invalidRgn),picture)
			where
				sameSize		= oldSize==newSize
//				samePos			= osCompoundMovesControls && oldPos==newPos || oldAbsolutePos==newAbsolutePos
				samePos			= if (withinCompound && osCompoundMovesControls) (oldPos==newPos) (oldAbsolutePos==newAbsolutePos)
				sizeF			= if sameSize
									id (osSetCompoundSize wPtr newCompoundPos` itemPtr newPos` newSize` True)
				moveF			= if (samePos && all isEmptyRect (map (intersectRects newFrame1) newArea))
									id (osSetCompoundPos  wPtr newCompoundPos` itemPtr newPos` newSize` True)
				updScrollbars	= if (sameSize && samePos && all isEmptyRect (flatten (map (\area->[intersectRects hRect` area,intersectRects vRect` area]) newArea)))
									id ( (setCompoundScroll wPtr (snd hasScrolls) wMetrics itemPtr False newAble newVThumbSize oldOrigin.y newOrigin.y vRect`)
									   o (setCompoundScroll wPtr (fst hasScrolls) wMetrics itemPtr True  newAble newHThumbSize oldOrigin.x newOrigin.x hRect`)
									   )
				updF			= if (sameSize && oldAbsolutePos==newAbsolutePos && oldFrame1==newFrame1 || isEmptyRect newFrame1 || not new.rliItemShow)
									id (updatecustomcontrol wPtr newParentPos clipRgn newFrame1 new)
				newParentPos`	= toTuple newParentPos
				newCompoundPos`	= toTuple newCompoundPos
				itemPtr			= new.rliItemPtr
				newAble			= new.rliItemSelect
				newSize`		= toTuple newSize
				newPos`			= toTuple newAbsolutePos
				newSize			= new.rliItemSize;							oldSize			= old.rliItemSize
				newPos			= new.rliItemPos;							oldPos			= old.rliItemPos
				newAbsolutePos	= movePoint newPos newParentPos;			oldAbsolutePos	= movePoint oldPos oldParentPos
				newCompoundPos1	= newCompoundPos + newPos;					oldCompoundPos1	= oldCompoundPos + oldPos
				newInfo			= new.rliItemInfo;							oldInfo			= old.rliItemInfo
				newOrigin		= newInfo.compoundOrigin;					oldOrigin		= oldInfo.compoundOrigin
				newDomainRect	= newInfo.compoundDomain;					oldDomainRect	= oldInfo.compoundDomain
				newCompoundRect	= posSizeToRect newAbsolutePos newSize;		oldCompoundRect	= posSizeToRect oldAbsolutePos oldSize
				newFrame1		= intersectRects newFrame newContentRect;	oldFrame1		= intersectRects oldFrame oldContentRect
				newArea1		= subtractRects newFrame1 oldFrame1
				hasScrolls		= (isJust newInfo.compoundHScroll,isJust newInfo.compoundVScroll)
				(hScrollPtr,vScrollPtr)
								= (mscrollptr newInfo.compoundHScroll,mscrollptr newInfo.compoundVScroll)
				mscrollptr		= mapMaybe (\{scrollItemPtr}->scrollItemPtr)
				newVisScrolls	= osScrollbarsAreVisible wMetrics newDomainRect newSize` hasScrolls
				oldVisScrolls	= osScrollbarsAreVisible wMetrics oldDomainRect (toTuple oldSize) hasScrolls
				newHThumbSize	= if (snd newVisScrolls) (newSize.w-wMetrics.osmVSliderWidth  + 1) (newSize.w+1)
				newVThumbSize	= if (fst newVisScrolls) (newSize.h-wMetrics.osmHSliderHeight + 1) (newSize.h+1)
				oldContentRect	= osGetCompoundContentRect wMetrics oldVisScrolls oldCompoundRect
				newContentRect	= osGetCompoundContentRect wMetrics newVisScrolls newCompoundRect
				newContentSize	= rectSize newContentRect
				hRect`			= osGetCompoundHScrollRect wMetrics newVisScrolls (posSizeToRect newAbsolutePos newSize)
				vRect`			= osGetCompoundVScrollRect wMetrics newVisScrolls (posSizeToRect newAbsolutePos newSize)
				
				setCompoundScroll :: !OSWindowPtr !Bool OSWindowMetrics OSWindowPtr Bool Bool Int Int Int !OSRect !*OSToolbox -> *OSToolbox
				setCompoundScroll wPtr hasScroll wMetrics compoundPtr isHorizontal able size old new rect=:{rright,rbottom} tb
					| not hasScroll
						= tb
					# tb			= osSetCompoundSliderThumbSize wMetrics wPtr compoundPtr scrollPtr min max wid rect isHorizontal able (old==new) tb
					| old==new
						# tb		= osUpdateCompoundScroll wPtr scrollPtr rect tb
						= tb
					| otherwise
						# tb		= osSetCompoundSliderThumb wMetrics wPtr compoundPtr scrollPtr rect isHorizontal pos (rright,rbottom) True tb
						# tb		= osUpdateCompoundScroll wPtr scrollPtr rect tb
						= tb
				where
					(min,pos,max,wid)	= toOSscrollbarRange (scrollMin,new,scrollMax) scrollSize
					scrollPtr		= if isHorizontal (fromJust hScrollPtr) (fromJust vScrollPtr)
					scrollMin		= if isHorizontal newDomainRect.rleft  newDomainRect.rtop
					scrollMax		= if isHorizontal newDomainRect.rright newDomainRect.rbottom
					scrollSize		= if isHorizontal newContentSize.w newContentSize.h
			
			relayout wPtr wMetrics withinCompound newArea IsLayoutControl (oldFrame,oldParentPos,oldCompoundPos,old) 
			                                                              (newFrame,newParentPos,newCompoundPos,new)
			                                                              rgnHs picture
				= relayoutItems` wPtr wMetrics withinCompound newArea (oldFrame1,oldAbsolutePos,oldCompoundPos,old.rliItems)
				                                                      (newFrame1,newAbsolutePos,newCompoundPos,new.rliItems)
				                                                      rgnHs picture
			where
				newSize					= new.rliItemSize;								oldSize			= old.rliItemSize
				newPos					= new.rliItemPos;								oldPos			= old.rliItemPos
				newAbsolutePos			= movePoint newPos newParentPos;				oldAbsolutePos	= movePoint oldPos oldParentPos
				newLayoutRect			= posSizeToRect newAbsolutePos newSize;			oldLayoutRect	= posSizeToRect oldAbsolutePos oldSize
				newFrame1				= intersectRects newFrame newLayoutRect;		oldFrame1		= intersectRects oldFrame oldLayoutRect
				
			relayout wPtr wMetrics withinCompound newArea controlKind (oldFrame,oldParentPos,oldCompoundPos,old)
			                                                          (newFrame,newParentPos,newCompoundPos,new) 
			                                                          (clipRgn,validRgn,invalidRgn) picture
				#! picture				= apppicttoolbox (moveF o sizeF) picture
				#! picture				= updF picture
				| new.rliItemShow
					#! ((validRgn,invalidRgn),picture)
										= accpicttoolbox (checkUpdateRegions oldFrame1 newFrame1 (validRgn,invalidRgn)) picture
					#! (clipRgn,picture)= accpicttoolbox (subtractRectFromRgn newFrame1 clipRgn) picture
					= ((clipRgn,validRgn,invalidRgn),picture)
				| otherwise
					= ((clipRgn,validRgn,invalidRgn),picture)
			where
				sameSize				= oldSize==newSize
//				samePos					= osCompoundMovesControls && oldPos==newPos || oldAbsolutePos==newAbsolutePos
				samePos					= if (withinCompound && osCompoundMovesControls) (oldPos==newPos) (oldAbsolutePos==newAbsolutePos)
				sizeF					= if sameSize id (setSize wPtr newCompoundPos` itemPtr newPos` newSize` (not redraw))
				moveF					= if (samePos && all isEmptyRect (map (intersectRects newFrame1) newArea))
											          id (setPos  wPtr newCompoundPos` itemPtr newPos` (toTuple oldSize) (not redraw))
				updF					= if (not redraw || sameSize && oldAbsolutePos==newAbsolutePos && newFrame1==oldFrame1 || isEmptyRect newFrame1 || not new.rliItemShow)
													  id (updatecustomcontrol wPtr newParentPos clipRgn newFrame1 new)
				newParentPos`			= toTuple newParentPos
				newCompoundPos`			= toTuple newCompoundPos
				itemPtr					= new.rliItemPtr
				newSize`				= toTuple newSize
				newPos`					= toTuple newAbsolutePos
				newPos					= new.rliItemPos;					oldPos			= old.rliItemPos;
				newAbsolutePos			= movePoint newPos newParentPos;	oldAbsolutePos	= movePoint oldPos oldParentPos
				newSize					= new.rliItemSize;					oldSize			= old.rliItemSize;
				oldFrame1				= intersectRects oldFrame (posSizeToRect oldAbsolutePos oldSize)
				newFrame1				= intersectRects newFrame (posSizeToRect newAbsolutePos newSize)
				(setPos,setSize,redraw)	= case controlKind of
											IsRadioControl			-> (osSetRadioControlPos,		\_ _ _ _ _ _ tb->tb,		 False)		// DvA: moeten op mac elements gewijs verplaatst worden...
											IsCheckControl			-> (osSetCheckControlPos,		\_ _ _ _ _ _ tb->tb,		 False)		// DvA: moeten op mac elements gewijs verplaatst worden...
											IsPopUpControl			-> (osSetPopUpControlPos,		osSetPopUpControlSize,		 False)
											IsSliderControl			-> (osSetSliderControlPos,		osSetSliderControlSize,		 False)
											IsTextControl			-> (osSetTextControlPos,		osSetTextControlSize,		 False)
											IsEditControl			-> (osSetEditControlPos,		osSetEditControlSize,		 False)
											IsButtonControl			-> (osSetButtonControlPos,		osSetButtonControlSize,		 False)
											IsCustomButtonControl	-> (osSetCustomButtonControlPos,osSetCustomButtonControlSize,True)
											IsCustomControl			-> (osSetCustomControlPos,		osSetCustomControlSize,		 True)
											(IsOtherControl _)		-> (\_ _ _ _ _ _ tb->tb,		\_ _ _ _ _ _ tb->tb,		 False)
											_						-> relayoutFatalError "relayout" "unexpected ControlKind alternative"
			
	relayoutItems` _ _ _ _ (_,_,_,[]) (_,_,_,[]) rgnHs picture
		= (rgnHs,picture)
	relayoutItems` _ _ _ _ _ _ _ _
		= relayoutFatalError "relayoutItems`" "mismatching RelayoutItems"
	
	checkUpdateRegions :: !OSRect !OSRect !(!OSRgnHandle,!OSRgnHandle) !*OSToolbox -> (!(!OSRgnHandle,!OSRgnHandle),!*OSToolbox)
	checkUpdateRegions oldFrame newFrame rgnHs=:(validRgn,invalidRgn) tb
		| oldFrame==newFrame
			= (rgnHs,tb)
		| otherwise
			# (newFrameRgn,  tb)= osnewrectrgn newFrame tb
			# (oldFrameRgn,  tb)= osnewrectrgn oldFrame tb
			# (okNewRgn,     tb)= osdiffrgn  newFrameRgn invalidRgn tb	// PA+++
			# (newValidRgn,  tb)= osunionrgn okNewRgn validRgn tb		// PA: okNewRgn <-- newFrameRgn
			# (newInvalidRgn,tb)= osunionrgn oldFrameRgn invalidRgn tb
			# tb				= stateMap2 osdisposergn [validRgn,invalidRgn,newFrameRgn,oldFrameRgn,okNewRgn] tb	// PA: okNewRgn added
			= ((newValidRgn,newInvalidRgn),tb)
	
	subtractRectFromRgn :: !OSRect !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
	subtractRectFromRgn rect rgn tb
		| isEmptyRect rect
			= (rgn,tb)
		| otherwise
			# (rectRgn,tb)	= osnewrectrgn rect tb
			# (diffRgn,tb)	= osdiffrgn rgn rectRgn tb
			# tb			= osdisposergn rectRgn tb
			# tb			= osdisposergn rgn tb
			= (diffRgn,tb)
	
	//	updatecustomcontrol assumes that the item is visible.
	updatecustomcontrol :: !OSWindowPtr !Point2 !OSRgnHandle !OSRect !RelayoutItem !*Picture -> *Picture
	updatecustomcontrol parentPtr parentPos clipRgn contentRect itemH=:{rliItemKind=IsCustomButtonControl} picture
		#! (curOrigin,picture)			= getpictorigin picture
		#! (curPen,   picture)			= getpictpen picture
		#! picture						= setpictorigin (zero-absolutePos) picture
		#! picture						= setpictpen lookPen picture
		#! picture						= clipospicture clipRgn contentRect (lookFun selectState updState) picture
		#! picture						= setpictpen curPen picture
		#  picture						= setpictorigin curOrigin picture
		= picture
	where
		absolutePos						= movePoint itemH.rliItemPos parentPos
		selectState						= if itemH.rliItemSelect Able Unable
		{lookFun,lookPen}				= itemH.rliItemLook
		cFrame							= sizeToRectangle itemH.rliItemSize
		updState						= {oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
	
	updatecustomcontrol parentPtr parentPos clipRgn contentRect itemH=:{rliItemKind=IsCustomControl} picture
		#! (curOrigin,picture)			= getpictorigin picture
		#! (curPen,   picture)			= getpictpen picture
		#! picture						= setpictorigin (zero-absolutePos) picture
		#! picture						= setpictpen lookPen picture
		#! picture						= clipospicture clipRgn contentRect (lookFun selectState updState) picture
		#! picture						= setpictpen curPen picture
		# picture						= setpictorigin curOrigin picture
		= picture
	where
		absolutePos						= movePoint itemH.rliItemPos parentPos
		selectState						= if itemH.rliItemSelect Able Unable
		{lookFun,lookPen}				= itemH.rliItemLook
		cFrame							= sizeToRectangle itemH.rliItemSize
		updState						= {oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
	
	updatecustomcontrol parentPtr parentPos clipRgn contentRect itemH=:{rliItemKind=IsCompoundControl} picture
		#! (curOrigin,picture)			= getpictorigin picture
		#! (curPen,   picture)			= getpictpen picture
		#! picture						= setpictorigin (origin-absolutePos) picture
		#! picture						= setpictpen lookPen picture
		#! (clip,picture)				= accpicttoolbox (ossectrgn clipRgn clipInfo.clipRgn) picture
		#! picture						= clipospicture clip clipRect (lookFun selectState updState) picture
		#! picture						= apppicttoolbox (osdisposergn clip) picture
		#! picture						= setpictpen curPen picture
		# picture						= setpictorigin curOrigin picture
		= picture
	where
		absolutePos						= movePoint itemH.rliItemPos parentPos
		selectState						= if itemH.rliItemSelect Able Unable
		itemSize						= itemH.rliItemSize
		info							= itemH.rliItemInfo
		(origin,domainRect,hasScrolls)	= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
		visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
		cFrameRect						= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect origin itemSize)
		cFrame							= rectToRectangle cFrameRect
		compLookInfo					= info.compoundLookInfo
		{lookFun,lookPen}				= compLookInfo.compoundLook
		clipInfo						= compLookInfo.compoundClip
		updState						= {oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
		cRect							= addVector (toVector (absolutePos-origin)) cFrameRect
		clipRect						= intersectRects contentRect cRect
	
	clipospicture :: !OSRgnHandle !OSRect !(IdFun *Picture) !*Picture -> *Picture
	clipospicture newClipRgn rect drawf picture
		#! (rectRgn,picture)	= accpicttoolbox (osnewrectrgn rect) picture
		#! (curClipRgn,picture)	= pictgetcliprgn picture
		#! (emptyCur,picture)	= accpicttoolbox (osisemptyrgn curClipRgn) picture
		#! (emptyNew,picture)	= accpicttoolbox (osisemptyrgn newClipRgn) picture
		#! picture				= (if emptyCur (pictsetcliprgn rectRgn) (pictandcliprgn rectRgn)) picture
		#! picture				= (if emptyNew id (pictandcliprgn newClipRgn)) picture
/*
		#! picture				= (if (curClipRgn==0) (pictsetcliprgn rectRgn) (pictandcliprgn rectRgn)) picture
		#! picture				= (if (newClipRgn==0) id (pictandcliprgn newClipRgn)) picture
*/
		#! picture				= drawf picture
		#! picture				= pictsetcliprgn curClipRgn picture
		#  picture				= apppicttoolbox
									( osdisposergn rectRgn
									o (if (curClipRgn==0)
										id
										(osdisposergn curClipRgn))
									) picture
		= picture
