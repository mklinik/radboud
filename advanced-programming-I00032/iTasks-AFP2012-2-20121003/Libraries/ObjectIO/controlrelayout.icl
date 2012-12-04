implementation module controlrelayout


import StdBool, StdList, StdMisc
import relayout, windowaccess, windowclipstate, wstateaccess


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
		new                  :: [WElementHandle ls pst] contains the elements at their new location and size.
	relayoutControls(`) assumes that old and new contain elements that are identical except for size and position.
		If this is not the case, a runtime error will occur.
	relayoutControls(`) assumes that the ClipStates of all compound elements are valid.
	The return OSRgnHandle is the area of the window that requires to be updated (use updatewindowbackgrounds [windowupdate] for this purpose).
*/
relayoutControls :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !(!OSRect,!Point2,!Vector2,![WElementHandle`])
                                                                                !(!OSRect,!Point2,!Vector2,!*[WElementHandle .ls .pst]) 
                                                                                !*OSToolbox
                                   -> (!OSRgnHandle,!*[WElementHandle .ls .pst],!*OSToolbox)
relayoutControls wMetrics wPtr defaultId withinCompound isAble isVisible (oldFrame,oldParentPos,oldCompoundPos,oldHs) 
                                                                         (newFrame,newParentPos,newCompoundPos,newHs) tb
	# oldRelayoutItems			= wElementHandles`ToRelayoutItems isAble isVisible oldHs []
	# (newHs,newRelayoutItems)	= wElementHandlesToRelayoutItems  isAble isVisible newHs []
	# (updRgn,tb)				= relayoutItems wMetrics wPtr withinCompound (oldFrame,oldParentPos,oldCompoundPos,oldRelayoutItems) 
								                                             (newFrame,newParentPos,newCompoundPos,newRelayoutItems) tb
	= (updRgn,newHs,tb)
where
	wElementHandlesToRelayoutItems :: !Bool !Bool ![WElementHandle .ls .pst] ![RelayoutItem] -> (![WElementHandle .ls .pst],![RelayoutItem])
	wElementHandlesToRelayoutItems isAble isVisible [itemH:itemHs] items
		# (itemHs,items)	= wElementHandlesToRelayoutItems isAble isVisible itemHs items
		# (itemH, items)	= wElementHandleToRelayoutItems  isAble isVisible itemH  items
		= ([itemH:itemHs],items)
	where
		wElementHandleToRelayoutItems :: !Bool !Bool !(WElementHandle .ls .pst) ![RelayoutItem] -> (!WElementHandle .ls .pst,![RelayoutItem])
		wElementHandleToRelayoutItems isAble isVisible (WItemHandle itemH=:{wItemKind}) items
			# (itemH,items)	= wItemHandleToRelayoutItems wItemKind isAble isVisible itemH items
			= (WItemHandle itemH,items)
		where
			wItemHandleToRelayoutItems :: !ControlKind !Bool !Bool !(WItemHandle .ls .pst) ![RelayoutItem] -> (!WItemHandle .ls .pst,![RelayoutItem])
			wItemHandleToRelayoutItems controlKind=:IsRadioControl isAble isVisible itemH=:{wItemSelect,wItemShow,wItemInfo} items
				= (itemH,radioItemToRelayoutItems (isAble && wItemSelect) (isVisible && wItemShow) (getWItemRadioInfo wItemInfo).radioItems items)
			where
				radioItemToRelayoutItems :: !Bool !Bool ![RadioItemInfo *(.ls,.pst)] ![RelayoutItem] -> [RelayoutItem]
				radioItemToRelayoutItems isAble isVisible [radio:radios] items
					#! item	= radioItemInfoToRelayoutItem isAble isVisible radio
					=  [item:radioItemToRelayoutItems isAble isVisible radios items]
				where
					radioItemInfoToRelayoutItem :: !Bool !Bool !(RadioItemInfo .pst) -> RelayoutItem
					radioItemInfoToRelayoutItem isAble isVisible {radioItemPos,radioItemSize,radioItemPtr}
						= {	rliItemKind		= controlKind
						  ,	rliItemPtr		= radioItemPtr
						  ,	rliItemPos		= radioItemPos
						  ,	rliItemSize		= radioItemSize
						  ,	rliItemSelect	= isAble
						  ,	rliItemShow		= isVisible
						  ,	rliItemInfo		= undef
						  ,	rliItemLook		= undef
						  ,	rliItems		= []
						  }
				radioItemToRelayoutItems _ _ _ items
					= items
			
			wItemHandleToRelayoutItems controlKind=:IsCheckControl isAble isVisible itemH=:{wItemSelect,wItemShow,wItemInfo} items
				= (itemH,checkItemToRelayoutItems (isAble && wItemSelect) (isVisible && wItemShow) (getWItemCheckInfo wItemInfo).checkItems items)
			where
				checkItemToRelayoutItems :: !Bool !Bool ![CheckItemInfo *(.ls,.pst)] ![RelayoutItem] -> [RelayoutItem]
				checkItemToRelayoutItems isAble isVisible [check:checks] items
					#! item	= checkItemInfoToRelayoutItem isAble isVisible check
					=  [item:checkItemToRelayoutItems isAble isVisible checks items]
				where
					checkItemInfoToRelayoutItem :: !Bool !Bool !(CheckItemInfo .pst) -> RelayoutItem
					checkItemInfoToRelayoutItem isAble isVisible {checkItemPos,checkItemSize,checkItemPtr}
						= {	rliItemKind		= controlKind
						  ,	rliItemPtr		= checkItemPtr
						  ,	rliItemPos		= checkItemPos
						  ,	rliItemSize		= checkItemSize
						  ,	rliItemSelect	= isAble
						  ,	rliItemShow		= isVisible
						  ,	rliItemInfo		= undef
						  ,	rliItemLook		= undef
						  ,	rliItems		= []
						  }
				checkItemToRelayoutItems _ _ _ items
					= items
			
			wItemHandleToRelayoutItems controlKind isAble isVisible itemH=:{wItemPtr,wItemPos,wItemSize,wItemSelect,wItemShow} items
				#! (info,look,items`,itemH)	= getinfo controlKind isAble` isVisible` itemH
				#! item						= {	rliItemKind		= controlKind
											  ,	rliItemPtr		= wItemPtr
											  ,	rliItemPos		= wItemPos
											  ,	rliItemSize		= wItemSize
											  ,	rliItemSelect	= isAble`
											  ,	rliItemShow		= isVisible`
											  ,	rliItemInfo		= info
											  ,	rliItemLook		= look
											  ,	rliItems		= items`
											  }
				= (itemH,[item:items])
			where
				isAble`						= isAble    && wItemSelect
				isVisible`					= isVisible && wItemShow
				
				getinfo :: !ControlKind !Bool !Bool !*(WItemHandle .ls .pst) -> (CompoundInfo,LookInfo,![RelayoutItem],!*WItemHandle .ls .pst)
				getinfo IsCompoundControl isAble isVisible itemH=:{wItemInfo,wItems=itemHs}
					# (itemHs,items)		= wElementHandlesToRelayoutItems isAble isVisible itemHs []
					= (info,info.compoundLookInfo.compoundLook,items,{itemH & wItems=itemHs})
				where
					info					= getWItemCompoundInfo wItemInfo
				getinfo IsCustomButtonControl _ _ itemH=:{wItemInfo}
					= (undef,(getWItemCustomButtonInfo wItemInfo).cButtonInfoLook,[],itemH)
				getinfo IsCustomControl _ _ itemH=:{wItemInfo}
					= (undef,(getWItemCustomInfo wItemInfo).customInfoLook,[],itemH)
				getinfo IsLayoutControl isAble isVisible itemH=:{wItems=itemHs}
					# (itemHs,items)		= wElementHandlesToRelayoutItems isAble isVisible itemHs []
					= (undef,undef,items,{itemH & wItems=itemHs})
				getinfo _ _ _ itemH
					= (undef,undef,[],itemH)
		
		wElementHandleToRelayoutItems isAble isVisible (WListLSHandle itemHs) items
			# (itemHs,items)	= wElementHandlesToRelayoutItems isAble isVisible itemHs items
			= (WListLSHandle itemHs,items)
		
		wElementHandleToRelayoutItems isAble isVisible (WExtendLSHandle wExH=:{wExtendItems=itemHs}) items
			# (itemHs,items)	= wElementHandlesToRelayoutItems isAble isVisible itemHs items
			= (WExtendLSHandle {wExH & wExtendItems=itemHs},items)
		
		wElementHandleToRelayoutItems isAble isVisible (WChangeLSHandle wChH=:{wChangeItems=itemHs}) items
			# (itemHs,items)	= wElementHandlesToRelayoutItems isAble isVisible itemHs items
			= (WChangeLSHandle {wChH & wChangeItems=itemHs},items)
	
	wElementHandlesToRelayoutItems _ _ [] items
		= ([],items)


relayoutControls` :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !(!OSRect,!Point2,!Vector2,![WElementHandle`])
                                                                                 !(!OSRect,!Point2,!Vector2,![WElementHandle`]) 
                                                                                 !*OSToolbox
                                                                -> (!OSRgnHandle,!*OSToolbox)
relayoutControls` wMetrics wPtr defaultId withinCompound isAble isVisible (oldFrame,oldParentPos,oldCompoundPos,oldHs) 
                                                                          (newFrame,newParentPos,newCompoundPos,newHs) tb
	= relayoutItems wMetrics wPtr withinCompound (oldFrame,oldParentPos,oldCompoundPos,wElementHandles`ToRelayoutItems isAble isVisible oldHs [])
	                                             (newFrame,newParentPos,newCompoundPos,wElementHandles`ToRelayoutItems isAble isVisible newHs [])
	                                             tb

wElementHandles`ToRelayoutItems :: !Bool !Bool ![WElementHandle`] ![RelayoutItem] -> [RelayoutItem]
wElementHandles`ToRelayoutItems isAble isVisible [itemH:itemHs] items
	= wElementHandle`ToRelayoutItems isAble isVisible itemH (wElementHandles`ToRelayoutItems isAble isVisible itemHs items)
where
	wElementHandle`ToRelayoutItems :: !Bool !Bool !WElementHandle` ![RelayoutItem] -> [RelayoutItem]
	wElementHandle`ToRelayoutItems isAble isVisible (WItemHandle` itemH=:{wItemKind`}) items
		= wItemHandle`ToRelayoutItems wItemKind` isAble isVisible itemH items
	where
		wItemHandle`ToRelayoutItems :: !ControlKind !Bool !Bool !WItemHandle` ![RelayoutItem] -> [RelayoutItem]
		wItemHandle`ToRelayoutItems controlKind=:IsRadioControl isAble isVisible itemH=:{wItemSelect`,wItemShow`} items
			= radioItem`ToRelayoutItems (isAble && wItemSelect`) (isVisible && wItemShow`) (getWItemRadioInfo` itemH.wItemInfo`).radioItems` items
		where
			radioItem`ToRelayoutItems :: !Bool !Bool ![RadioItemInfo`] ![RelayoutItem] -> [RelayoutItem]
			radioItem`ToRelayoutItems isAble isVisible [radio:radios] items
				#! item	= radioItemInfo`ToRelayoutItem isAble isVisible radio
				=  [item:radioItem`ToRelayoutItems isAble isVisible radios items]
			where
				radioItemInfo`ToRelayoutItem :: !Bool !Bool !RadioItemInfo` -> RelayoutItem
				radioItemInfo`ToRelayoutItem isAble isVisible {radioItemPos`,radioItemSize`,radioItemPtr`}
					= {	rliItemKind		= controlKind
					  ,	rliItemPtr		= radioItemPtr`
					  ,	rliItemPos		= radioItemPos`
					  ,	rliItemSize		= radioItemSize`
					  ,	rliItemSelect	= isAble
					  ,	rliItemShow		= isVisible
					  ,	rliItemInfo		= undef
						  ,	rliItemLook		= undef
					  ,	rliItems		= []
					  }
			radioItem`ToRelayoutItems _ _ _ items
				= items
		
		wItemHandle`ToRelayoutItems controlKind=:IsCheckControl isAble isVisible itemH=:{wItemSelect`,wItemShow`} items
			= checkItem`ToRelayoutItems (isAble && wItemSelect`) (isVisible && wItemShow`) (getWItemCheckInfo` itemH.wItemInfo`).checkItems` items
		where
			checkItem`ToRelayoutItems :: !Bool !Bool ![CheckItemInfo`] ![RelayoutItem] -> [RelayoutItem]
			checkItem`ToRelayoutItems isAble isVisible [check:checks] items
				#! item	= checkItemInfo`ToRelayoutItem isAble isVisible check
				=  [item:checkItem`ToRelayoutItems isAble isVisible checks items]
			where
				checkItemInfo`ToRelayoutItem :: !Bool !Bool !CheckItemInfo` -> RelayoutItem
				checkItemInfo`ToRelayoutItem isAble isVisible {checkItemPos`,checkItemSize`,checkItemPtr`}
					= {	rliItemKind		= controlKind
					  ,	rliItemPtr		= checkItemPtr`
					  ,	rliItemPos		= checkItemPos`
					  ,	rliItemSize		= checkItemSize`
					  ,	rliItemSelect	= isAble
					  ,	rliItemShow		= isVisible
					  ,	rliItemInfo		= undef
					  ,	rliItemLook		= undef
					  ,	rliItems		= []
					  }
			checkItem`ToRelayoutItems _ _ _ items
				= items
		
		wItemHandle`ToRelayoutItems controlKind isAble isVisible itemH=:{wItemPtr`,wItemPos`,wItemSize`,wItemSelect`,wItemShow`} items
			#! item		=	{	rliItemKind		= controlKind
							,	rliItemPtr		= wItemPtr`
							,	rliItemPos		= wItemPos`
							,	rliItemSize		= wItemSize`
							,	rliItemSelect	= isAble`
							,	rliItemShow		= isVisible`
							,	rliItemInfo		= info
							,	rliItemLook		= look
							,	rliItems		= items`
							}
			= [item:items]
		where
			isAble`				= isAble    && wItemSelect`
			isVisible`			= isVisible && wItemShow`
			(info,look,items`)	= getinfo controlKind isAble` isVisible` itemH
			
			getinfo :: !ControlKind !Bool !Bool !WItemHandle` -> (CompoundInfo,LookInfo,![RelayoutItem])
			getinfo IsCompoundControl isAble isVisible {wItemInfo`,wItems`}
				= (info,info.compoundLookInfo.compoundLook,wElementHandles`ToRelayoutItems isAble isVisible wItems` [])
			where
				info	= getWItemCompoundInfo` wItemInfo`
			getinfo IsCustomButtonControl _ _ {wItemInfo`}
				= (undef,(getWItemCustomButtonInfo` wItemInfo`).cButtonInfoLook,[])
			getinfo IsCustomControl _ _ {wItemInfo`}
				= (undef,(getWItemCustomInfo` wItemInfo`).customInfoLook,[])
			getinfo IsLayoutControl isAble isVisible {wItems`}
				= (undef,undef,wElementHandles`ToRelayoutItems isAble isVisible wItems` [])
			getinfo _ _ _ _
				= (undef,undef,[])
	
	wElementHandle`ToRelayoutItems isAble isVisible (WRecursiveHandle` itemHs _) items
		= wElementHandles`ToRelayoutItems isAble isVisible itemHs items

wElementHandles`ToRelayoutItems _ _ _ items
	= items
