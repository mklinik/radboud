implementation module controllayout


import	StdBool, StdList, StdTuple, StdMisc
import	oswindow
import	commondef, layout, StdControlAttribute, windowaccess, wstateaccess


controllayoutError :: String String -> .x
controllayoutError rule message
	= error rule "controllayout" message

controllayoutFatalError :: String String -> .x
controllayoutFatalError rule message
	= fatalError rule "controllayout" message


//	Calculate the precise position (in pixels) of each Control.

layoutControls :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle .ls .pst] !*OSToolbox
																								  -> (!Size, ![WElementHandle .ls .pst],!*OSToolbox)
layoutControls wMetrics hMargins vMargins spaces reqSize minSize orientations itemHs tb
	# (_,_,itemHs)				= validateFirstWElementsPos False itemHs
	# (layouts,_,_,_,itemHs,tb)	= getLayoutItems wMetrics hMargins vMargins spaces orientations [] (sysId (-1)) (-2) itemHs tb
	  (size,roots)				= layoutItems hMargins vMargins spaces reqSize minSize orientations layouts
	  (_,itemHs)				= setLayoutItems roots itemHs
	= (size,itemHs,tb)

layoutControls` :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle`] !*OSToolbox
																								   -> (!Size, ![WElementHandle`],!*OSToolbox)
layoutControls` wMetrics hMargins vMargins spaces reqSize minSize orientations itemHs tb
	# (_,_,itemHs)				= validateFirstWElementsPos` False itemHs
	# (layouts,_,_,_,itemHs,tb)	= getLayoutItems` wMetrics hMargins vMargins spaces orientations [] (sysId (-1)) (-2) itemHs tb
	  (size,roots)				= layoutItems hMargins vMargins spaces reqSize minSize orientations layouts
	  (_,itemHs)				= setLayoutItems` roots itemHs
	= (size,itemHs,tb)

calcControlsSize :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle .ls .pst] !*OSToolbox
																																-> (!Size,!*OSToolbox)
calcControlsSize wMetrics hMargins vMargins spaces reqSize minSize orientations itemHs tb
	# (_,_,itemHs)				= validateFirstWElementsPos False itemHs
	# (layouts,_,_,_,_,tb)		= getLayoutItems wMetrics hMargins vMargins spaces orientations [] (sysId (-1)) (-2) itemHs tb
	  (size,_)					= layoutItems hMargins vMargins spaces reqSize minSize orientations layouts
	= (size,tb)


/*	validateFirstWElementsPos(`) verifies that the first non line layout, not virtual, WElementHandle either: 
	-	already has a layout attribute, or 
	-	obtains the (Left,zero) layout attribute if not preceded by a fix or corner WItemHandle.
*/
validateFirstWElementsPos :: !Bool ![WElementHandle .ls .pst] -> (!Bool,!Bool,![WElementHandle .ls .pst])
validateFirstWElementsPos fix_corner_item_found []
	= (False,fix_corner_item_found,[])
validateFirstWElementsPos fix_corner_item_found [itemH:itemHs]
	# (done,fix_corner_item_found,itemH)		= validateFirstWElementPos fix_corner_item_found itemH
	| done
		= (done,fix_corner_item_found,[itemH:itemHs])
	| otherwise
		# (done,fix_corner_item_found,itemHs)	= validateFirstWElementsPos fix_corner_item_found itemHs
		= (done,fix_corner_item_found,[itemH:itemHs])
where
	validateFirstWElementPos :: !Bool !(WElementHandle .ls .pst) -> (!Bool,!Bool,!WElementHandle .ls .pst)
	validateFirstWElementPos fix_corner_item_found (WItemHandle itemH=:{wItemVirtual,wItemAtts})
		| wItemVirtual
			= (False,fix_corner_item_found,WItemHandle itemH)
		| not hasPos
			| fix_corner_item_found
				= (True,fix_corner_item_found,WItemHandle itemH)
			// otherwise
				= (True,fix_corner_item_found,WItemHandle {itemH & wItemAtts=[posAtt:wItemAtts]})
		| otherwise
			# fix_corner_item_found	= case pos of
			  							Fix			-> True
			  							LeftTop		-> True
			  							RightTop	-> True
			  							LeftBottom	-> True
			  							RightBottom	-> True
			  							_			-> fix_corner_item_found
			  is_line_item			= case pos of
			  							Left		-> True
			  							Right		-> True
			  							Center		-> True
			  							_			-> False
			= (is_line_item,fix_corner_item_found,WItemHandle itemH)
	where
		(hasPos,posAtt)		= cselect isControlPos (ControlPos (Left,NoOffset)) wItemAtts
		pos					= fst (getControlPosAtt posAtt)
	
	validateFirstWElementPos fix_corner_item_found (WListLSHandle itemHs)
		# (done,fix_corner_item_found,itemHs)	= validateFirstWElementsPos fix_corner_item_found itemHs
		= (done,fix_corner_item_found,WListLSHandle itemHs)
	
	validateFirstWElementPos fix_corner_item_found (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (done,fix_corner_item_found,itemHs)	= validateFirstWElementsPos fix_corner_item_found itemHs
		= (done,fix_corner_item_found,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	validateFirstWElementPos fix_corner_item_found (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (done,fix_corner_item_found,itemHs)	= validateFirstWElementsPos fix_corner_item_found itemHs
		= (done,fix_corner_item_found,WChangeLSHandle {wChH & wChangeItems=itemHs})


validateFirstWElementsPos` :: !Bool ![WElementHandle`] -> (!Bool,!Bool,![WElementHandle`])
validateFirstWElementsPos` fix_corner_item_found itemHs
	| isEmpty itemHs
		= (False,fix_corner_item_found,itemHs)
	# (itemH,itemHs)							= hdtl itemHs
	  (done,fix_corner_item_found,itemH)		= validateFirstWElementPos` fix_corner_item_found itemH
	| done
		= (done,fix_corner_item_found,[itemH:itemHs])
	| otherwise
		# (done,fix_corner_item_found,itemHs)	= validateFirstWElementsPos` fix_corner_item_found itemHs
		= (done,fix_corner_item_found,[itemH:itemHs])
where
	validateFirstWElementPos` :: !Bool !WElementHandle` -> (!Bool,!Bool,!WElementHandle`)
	validateFirstWElementPos` fix_corner_item_found (WItemHandle` itemH=:{wItemVirtual`,wItemAtts`})
		| wItemVirtual`
			= (False,fix_corner_item_found,WItemHandle` itemH)
		| not hasPos
			| fix_corner_item_found
				= (True,fix_corner_item_found,WItemHandle` itemH)
			// otherwise
				= (True,fix_corner_item_found,WItemHandle` {itemH & wItemAtts`=[posAtt:wItemAtts`]})
		| otherwise
			# fix_corner_item_found	= case pos of
			  							Fix			-> True
			  							LeftTop		-> True
			  							RightTop	-> True
			  							LeftBottom	-> True
			  							RightBottom	-> True
			  							_			-> fix_corner_item_found
			  is_line_item			= case pos of
			  							Left		-> True
			  							Right		-> True
			  							Center		-> True
			  							_			-> False
			= (is_line_item,fix_corner_item_found,WItemHandle` itemH)
	where
		(hasPos,posAtt)		= cselect iscontrolpos` (ControlPos` (Left,NoOffset)) wItemAtts`
		pos					= fst (getcontrolpos` posAtt)
	
	validateFirstWElementPos` fix_corner_item_found (WRecursiveHandle` itemHs kind)
		# (done,fix_corner_item_found,itemHs)	= validateFirstWElementsPos` fix_corner_item_found itemHs
		= (done,fix_corner_item_found,WRecursiveHandle` itemHs kind)


/*	Transform the list of WElementHandles to LayoutItem elements and add private information to 
	the list of WElementHandles(`). 
	Only the definition fields of the WElementHandle are inspected (except for the recursive
	WElementHandles (WList/WElim/WIntro/WExtend/WChange(LSHandle), and IsCompoundControls) 
	which also inspects the recursive elements). 
	The recursive LayoutItems of WList/WElim/WIntro/WExtend/WChange(LSHandle)s are flattened.
	In case a control has no Id or an invalid Id (the Id already occurs earlier), then the control 
		is provided with a correct ControlId attribute in the attribute list. 
		The new ControlId (or the legal ControlId) attribute is placed in front of the attribute 
		list in the resulting WElementHandle list. This front position is assumed by 
		setLayoutItems!
	In case a control has no ControlPos attribute then it becomes (RightTo previous,NoOffset).
*/
getLayoutItems :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
								 [Id] Id !Int ![WElementHandle .ls .pst] !*OSToolbox
			   -> (![LayoutItem],[Id],Id,!Int,![WElementHandle .ls .pst],!*OSToolbox)
getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId cId [itemH:itemHs] tb
	# (itPoss1,prevIds,prevId,cId,itemH, tb) = getLayoutItem  wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH  tb
	# (itPoss2,prevIds,prevId,cId,itemHs,tb) = getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemHs tb
	= (itPoss1++itPoss2,prevIds,prevId,cId,[itemH:itemHs],tb)
where
	getLayoutItem :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
											[Id] Id !Int !(WElementHandle .ls .pst) !*OSToolbox
						  -> (![LayoutItem],[Id],Id,!Int,! WElementHandle .ls .pst, !*OSToolbox)
	getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId cId (WItemHandle itemH=:{wItemVirtual}) tb
		| wItemVirtual
			= ([],prevIds,prevId,cId,WItemHandle itemH,tb)
		| otherwise
			# (itPos,prevIds,prevId,cId,itemH,tb)	= getLayoutWItem wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH tb
			= ([itPos],prevIds,prevId,cId,WItemHandle itemH,tb)
	where
		getLayoutWItem :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
										   [Id] Id !Int !(WItemHandle .ls .pst) !*OSToolbox
						   -> (!LayoutItem,[Id],Id,!Int,! WItemHandle .ls .pst, !*OSToolbox)
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsButtonControl,wItemInfo,wItemAtts,wItemSize} tb
			# itPos					= newLayoutItem id pos wItemSize
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsCustomButtonControl,wItemAtts,wItemSize} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			itPos					= newLayoutItem id pos (checkCustomSize wItemSize)
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsTextControl,wItemInfo,wItemAtts,wItemSize} tb
			# itPos					= newLayoutItem id pos wItemSize
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsEditControl,wItemInfo,wItemAtts,wItemSize} tb
			# itPos					= newLayoutItem id pos wItemSize
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			pos						= getLayoutItemPos prevId wItemAtts
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsPopUpControl,wItemInfo,wItemAtts,wItemSize} tb
			# itPos					= newLayoutItem id pos wItemSize
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			pos						= getLayoutItemPos prevId wItemAtts
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsRadioControl,wItemInfo,wItemAtts} tb
			| isEmpty items
				= controllayoutError "RadioControl definition" "Empty list of RadioItem-s"
			| otherwise
				# colitemsizes		= toColumns layout items
				  colwidths			= map (map (\{radioItemSize={w}}->w)) colitemsizes
				  colmaxwidths		= map listmax colwidths
				  width				= sum colmaxwidths
				  height			= itemHeight*(length (hd colwidths))
				  itPos				= newLayoutItem id pos {w=width,h=height}
				  collaynoutitems	= position_items (\pos item->{item & radioItemPos=pos}) itemHeight 0 colmaxwidths colitemsizes
				  laynoutitems		= fromColumns layout collaynoutitems
				  info				= RadioInfo {info & radioItems=laynoutitems}
				= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts],wItemInfo=info},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			itemHeight				= osGetRadioControlItemHeight wMetrics
			info					= getWItemRadioInfo wItemInfo
			items					= info.radioItems
			layout					= info.radioLayout
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsCheckControl,wItemInfo,wItemAtts} tb
			| isEmpty items
				= controllayoutError "CheckControl definition" "Empty list of CheckItem-s"
			| otherwise
				# colitemsizes		= toColumns layout items
				  colwidths			= map (map (\{checkItemSize={w}}->w)) colitemsizes
				  colmaxwidths		= map listmax colwidths
				  width				= sum colmaxwidths
				  height			= itemHeight*(length (hd colwidths))
				  itPos				= newLayoutItem id pos {w=width,h=height}
				  collaynoutitems	= position_items (\pos item->{item & checkItemPos=pos}) itemHeight 0 colmaxwidths colitemsizes
				  laynoutitems		= fromColumns layout collaynoutitems
				  info				= CheckInfo {info & checkItems=laynoutitems}
				= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts],wItemInfo=info},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			itemHeight				= osGetCheckControlItemHeight wMetrics
			info					= getWItemCheckInfo wItemInfo
			items					= info.checkItems
			layout					= info.checkLayout
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsCustomControl,wItemAtts,wItemSize} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			pos						= getLayoutItemPos prevId wItemAtts
			itPos					= newLayoutItem id pos (checkCustomSize wItemSize)
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsSliderControl,wItemInfo,wItemAtts,wItemSize} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			pos						= getLayoutItemPos prevId wItemAtts
			itPos					= newLayoutItem id pos wItemSize
		
		getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind=IsOtherControl _,wItemAtts,wItemSize} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:wItemAtts]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			pos						= getLayoutItemPos prevId wItemAtts
			itPos					= newLayoutItem id pos wItemSize
		
		getLayoutWItem wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH=:{wItemKind=IsCompoundControl,wItemInfo,wItemAtts,wItems} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:atts1],wItems=items,wItemInfo=CompoundInfo info1},tb1)
		where
			info					= getWItemCompoundInfo wItemInfo
			(size,info1,items,atts1,tb1)
									= calcCompoundSize wMetrics hMargins vMargins spaces orientations info wItems wItemAtts tb
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			pos						= getLayoutItemPos prevId wItemAtts
			itPos					= newLayoutItem id pos size
			
			calcCompoundSize :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
									  !CompoundInfo ![WElementHandle .ls .pst] ![ControlAttribute *(.ls,.pst)] !*OSToolbox
							-> (!Size,!CompoundInfo,![WElementHandle .ls .pst],![ControlAttribute *(.ls,.pst)],!*OSToolbox)
			calcCompoundSize wMetrics hMargins=:(lMargin,rMargin) vMargins=:(tMargin,bMargin) spaces orientations info itemHs atts tb
				= (okDerivedSize,info1,itemHs1,if hadSize atts2 [ControlViewSize okDerivedSize:atts2],tb1)
			where
				origin					= info.compoundOrigin
				domainRect				= info.compoundDomain
				hasHScroll				= isJust info.compoundHScroll
				hasVScroll				= isJust info.compoundVScroll
				(minSize,atts1)			= validateMinSize atts
				(hadSize,reqSize,atts2)	= validateCompoundSize wMetrics (rectToRectangle domainRect) (hasHScroll,hasVScroll) atts1
				(_,hMarginAtt)			= cselect isControlHMargin (ControlHMargin lMargin rMargin) atts2
				newHMargins				= validateControlMargin (getControlHMarginAtt hMarginAtt)
				(_,vMarginAtt)			= cselect isControlVMargin (ControlVMargin tMargin bMargin) atts2
				newVMargins				= validateControlMargin (getControlVMarginAtt vMarginAtt)
				(_,spaceAtt)			= cselect isControlItemSpace (ControlItemSpace (fst spaces) (snd spaces)) atts2
				newItemSpaces			= validateControlItemSpace (getControlItemSpaceAtt spaceAtt)
				domain					= rectToRectangle domainRect
				newOrientations			= [(domain,origin):orientations]
				(derSize,itemHs1,tb1)	= layoutControls wMetrics newHMargins newVMargins newItemSpaces reqSize minSize newOrientations itemHs tb
				okDerivedSize			= validateDerivedCompoundSize wMetrics domainRect (hasHScroll,hasVScroll) derSize reqSize
				info1					= layoutScrollbars wMetrics okDerivedSize info

				validateMinSize :: ![ControlAttribute .st] -> (!Size,![ControlAttribute .st])
				validateMinSize atts
					= (okMinSize,if hadMinSize [ControlMinimumSize okMinSize:atts1] atts1)
				where
					(defMinW,defMinH)			= osMinCompoundSize
					(hadMinSize,minAtt,atts1)	= remove isControlMinimumSize (ControlMinimumSize {w=defMinW,h=defMinH}) atts
					minSize						= getControlMinimumSizeAtt minAtt
					okMinSize					= {w=max defMinW minSize.w,h=max defMinH minSize.h}

			/*	validateCompoundSize wMetrics viewDomain (hasHScroll,hasVScroll) atts
					validates the Control(View/Outer)Size attribute. The Boolean result is True iff the attribute list contains
					the Control(View/Outer)Size attribute.
					The Booleans hasHScroll hasVScroll should be True iff the compound has the ControlHScroll, ControlVScroll
					attribute set respectively. 
					In addition, the ControlOuterSize attribute is mapped to ControlViewSize attribute.
			*/
				validateCompoundSize :: !OSWindowMetrics !ViewDomain !(!Bool,!Bool) ![ControlAttribute .st]
																	-> (!Bool,!Size,![ControlAttribute .st])
				validateCompoundSize wMetrics domain hasScrolls atts
					| not hasSize
						= (False,zero,atts)
					| isControlViewSize sizeAtt
						= (True,size1,snd (creplace isControlViewSize (ControlViewSize size1) atts))
					with
						size			= getControlViewSizeAtt sizeAtt
						size1			= {w=max size.w (fst minSize),h=max size.h (snd minSize)}
					| otherwise
						# (w,h)			= toTuple outerSize
						  visScrolls	= osScrollbarsAreVisible wMetrics (rectangleToRect domain) (w,h) hasScrolls
						  viewSize		= rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect {w=w,h=h}))
						# (_,_,atts)	= remove isControlOuterSize undef atts
						# (_,_,atts)	= remove isControlViewSize  undef atts
						= (True,viewSize,[ControlViewSize viewSize:atts])
					with
						outerSize		= getControlOuterSizeAtt sizeAtt
				where
					(hasSize,sizeAtt)	= cselect (\att->isControlViewSize att || isControlOuterSize att) undef atts
					minSize				= osMinCompoundSize
				
				validateDerivedCompoundSize :: !OSWindowMetrics !OSRect !(!Bool,!Bool) Size !Size -> Size
				validateDerivedCompoundSize wMetrics domain hasScrolls derSize reqSize
					| reqSize==zero		= validateScrollbarSize wMetrics domain hasScrolls derSize
					| otherwise			= validateScrollbarSize wMetrics domain hasScrolls reqSize
				where
					validateScrollbarSize :: !OSWindowMetrics !OSRect !(!Bool,!Bool) !Size -> Size
					validateScrollbarSize wMetrics domainRect (hasHScroll,hasVScroll) size=:{w,h}
						| domainSize==zero			= size
						| visHScroll && visVScroll	= {w=w`,h=h`}
						| visHScroll				= {size & h=h`}
						| visVScroll				= {size & w=w`}
						| otherwise					= size
					where
						domainSize					= rectSize domainRect
						visHScroll					= hasHScroll && osScrollbarIsVisible (domainRect.rleft,domainRect.rright)  w
						visVScroll					= hasVScroll && osScrollbarIsVisible (domainRect.rtop, domainRect.rbottom) h
						(w`,h`)						= (w+wMetrics.osmVSliderWidth,h+wMetrics.osmHSliderHeight)
		
		getLayoutWItem wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH=:{wItemKind=IsLayoutControl,wItemAtts,wItems} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts=[ControlId id:atts1],wItems=items},tb1)
		where
			(size,items,atts1,tb1)	= calcLayoutSize wMetrics hMargins vMargins spaces orientations wItems wItemAtts tb
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId prevIds
			pos						= getLayoutItemPos prevId wItemAtts
			itPos					= newLayoutItem id pos size
			
			calcLayoutSize :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
									  ![WElementHandle .ls .pst] ![ControlAttribute *(.ls,.pst)] !*OSToolbox
							-> (!Size,![WElementHandle .ls .pst],![ControlAttribute *(.ls,.pst)],!*OSToolbox)
			calcLayoutSize wMetrics hMargins=:(lMargin,rMargin) vMargins=:(tMargin,bMargin) spaces orientations itemHs atts tb
//				= (okDerivedSize,itemHs1,if hadSize atts2 [ControlViewSize okDerivedSize:atts2],tb1)
				= (okDerivedSize,itemHs1,atts2,tb1)		// PA: do not modify attribute list, because this fixes size of LayoutControl during life-cycle!
			where
				(minSize,atts1)			= validateMinSize atts
				(hadSize,reqSize,atts2)	= validateLayoutSize wMetrics atts1
				(_,hMarginAtt)			= cselect isControlHMargin (ControlHMargin lMargin rMargin) atts2
				newHMargins				= validateControlMargin (getControlHMarginAtt hMarginAtt)
				(_,vMarginAtt)			= cselect isControlVMargin (ControlVMargin tMargin bMargin) atts2
				newVMargins				= validateControlMargin (getControlVMarginAtt vMarginAtt)
				(_,spaceAtt)			= cselect isControlItemSpace (ControlItemSpace (fst spaces) (snd spaces)) atts2
				newItemSpaces			= validateControlItemSpace (getControlItemSpaceAtt spaceAtt)
				(derSize,itemHs1,tb1)	= layoutControls wMetrics newHMargins newVMargins newItemSpaces reqSize minSize orientations itemHs tb
				okDerivedSize			= validateDerivedLayoutSize wMetrics derSize reqSize

				validateMinSize :: ![ControlAttribute .st] -> (!Size,![ControlAttribute .st])
				validateMinSize atts
					= (okMinSize,if hadMinSize [ControlMinimumSize okMinSize:atts1] atts1)
				where
					(hadMinSize,minAtt,atts1)	= remove isControlMinimumSize (ControlMinimumSize zero) atts
					minSize						= getControlMinimumSizeAtt minAtt
					okMinSize					= {w=max 0 minSize.w,h=max 0 minSize.h}

			/*	validateLayoutSize wMetrics atts
					validates the Control(View/Outer)Size attribute. The Boolean result is True iff the attribute list contains
					the Control(View/Outer)Size attribute.
					In addition, the ControlOuterSize attribute is mapped to ControlViewSize attribute (identical value).
			*/
				validateLayoutSize :: !OSWindowMetrics ![ControlAttribute .st] -> (!Bool,!Size,![ControlAttribute .st])
				validateLayoutSize wMetrics atts
					| not hasSize
						= (False,zero,atts)
					| otherwise
						# size1			= {w=max size.w 0,h=max size.h 0}
						# (_,_,atts)	= remove isControlOuterSize undef atts
						# (_,_,atts)	= remove isControlViewSize  undef atts
						= (True,size1,[ControlViewSize size1:atts])
				where
					(hasSize,sizeAtt)	= cselect (\att->isControlViewSize att || isControlOuterSize att) undef atts
					size				= if (isControlViewSize sizeAtt) (getControlViewSizeAtt sizeAtt) (getControlOuterSizeAtt sizeAtt)

		getLayoutWItem _ _ _ _ _ _ _ _ _ _
			= controllayoutFatalError "getLayoutWItem" "unmatched control implementation alternative"
		
		getLayoutItemPos :: Id ![ControlAttribute .st] -> ItemPos
		getLayoutItemPos prevId atts
			= (itemLoc1,offset)
		where
			(itemLoc,offset)	= getControlPosAtt (snd (cselect isControlPos (ControlPos (RightTo prevId,NoOffset)) atts))
			itemLoc1			= if (isRelativeToPrev itemLoc) (setRelativeTo prevId itemLoc) itemLoc
	
	getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId cId (WListLSHandle itemHs) tb
		# (itPoss,prevIds,prevId,cId,itemHs,tb)	= getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemHs tb
		= (itPoss,prevIds,prevId,cId,WListLSHandle itemHs,tb)
	
	getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId cId (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
		# (itPoss,prevIds,prevId,cId,itemHs,tb)	= getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemHs tb
		= (itPoss,prevIds,prevId,cId,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
	
	getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId cId (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
		# (itPoss,prevIds,prevId,cId,itemHs,tb)	= getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemHs tb
		= (itPoss,prevIds,prevId,cId,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)

getLayoutItems _ _ _ _ _ prevIds prevId cId [] tb
	= ([],prevIds,prevId,cId,[],tb)

	
getLayoutItems` :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
							 [Id] Id !Int ![WElementHandle`] !*OSToolbox
				-> (![LayoutItem],[Id],Id,!Int,![WElementHandle`],!*OSToolbox)
getLayoutItems` wMetrics hMargins vMargins spaces orientations prevIds prevId cId [itemH:itemHs] tb
	# (itPoss1,prevIds,prevId,cId,itemH, tb) = getLayoutItem`  wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH  tb
	# (itPoss2,prevIds,prevId,cId,itemHs,tb) = getLayoutItems` wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemHs tb
	= (itPoss1++itPoss2,prevIds,prevId,cId,[itemH:itemHs],tb)
where
	getLayoutItem` :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
									   [Id] Id !Int !WElementHandle` !*OSToolbox
					 -> (![LayoutItem],[Id],Id,!Int,!WElementHandle`,!*OSToolbox)
	getLayoutItem` wMetrics hMargins vMargins spaces orientations prevIds prevId cId (WItemHandle` itemH=:{wItemVirtual`}) tb
		| wItemVirtual`
			= ([],prevIds,prevId,cId,WItemHandle` itemH,tb)
		| otherwise
			# (itPos,prevIds,prevId,cId,itemH,tb)	= getLayoutWItem` wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH tb
			= ([itPos],prevIds,prevId,cId,WItemHandle` itemH,tb)
	where
		getLayoutWItem` :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
										[Id] Id !Int !WItemHandle` !*OSToolbox
						-> (!LayoutItem,[Id],Id,!Int,!WItemHandle`,!*OSToolbox)
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsButtonControl,wItemInfo`,wItemAtts`,wItemSize`} tb
			# itPos					= newLayoutItem id pos wItemSize`
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts`
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsCustomButtonControl,wItemAtts`,wItemSize`} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts`
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			itPos					= newLayoutItem id pos (checkCustomSize wItemSize`)
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsTextControl,wItemInfo`,wItemAtts`,wItemSize`} tb
			# itPos					= newLayoutItem id pos wItemSize`
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts`
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsEditControl,wItemInfo`,wItemAtts`,wItemSize`} tb
			# itPos					= newLayoutItem id pos wItemSize`
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			pos						= getLayoutItemPos prevId wItemAtts`
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsPopUpControl,wItemInfo`,wItemAtts`,wItemSize`} tb
			# itPos					= newLayoutItem id pos wItemSize`
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			pos						= getLayoutItemPos prevId wItemAtts`
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsRadioControl,wItemInfo`,wItemAtts`} tb
			| isEmpty items
				= controllayoutError "RadioControl definition" "Empty list of RadioItem-s"
			| otherwise
				# colitemsizes		= toColumns layout items
				  colwidths			= map (map (\{radioItemSize`={w}}->w)) colitemsizes
				  colmaxwidths		= map listmax colwidths
				  width				= sum colmaxwidths
				  height			= itemHeight*(length (hd colwidths))
				  itPos				= newLayoutItem id pos {w=width,h=height}
				  collaynoutitems	= position_items (\pos item->{item & radioItemPos`=pos}) itemHeight 0 colmaxwidths colitemsizes
				  laynoutitems		= fromColumns layout collaynoutitems
				  info				= RadioInfo` {info & radioItems`=laynoutitems}
				= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`],wItemInfo`=info},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts`
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			itemHeight				= osGetRadioControlItemHeight wMetrics
			info					= getWItemRadioInfo` wItemInfo`
			items					= info.radioItems`
			layout					= info.radioLayout`
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsCheckControl,wItemInfo`,wItemAtts`} tb
			| isEmpty items
				= controllayoutError "CheckControl definition" "Empty list of CheckItem-s"
			| otherwise
				# colitemsizes		= toColumns layout items
				  colwidths			= map (map (\{checkItemSize`={w}}->w)) colitemsizes
				  colmaxwidths		= map listmax colwidths
				  width				= sum colmaxwidths
				  height			= itemHeight*(length (hd colwidths))
				  itPos				= newLayoutItem id pos {w=width,h=height}
				  collaynoutitems	= position_items (\pos item->{item & checkItemPos`=pos}) itemHeight 0 colmaxwidths colitemsizes
				  laynoutitems		= fromColumns layout collaynoutitems
				  info				= CheckInfo` {info & checkItems`=laynoutitems}
				= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`],wItemInfo`=info},tb)
		where
			pos						= getLayoutItemPos prevId wItemAtts`
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			itemHeight				= osGetCheckControlItemHeight wMetrics
			info					= getWItemCheckInfo` wItemInfo`
			items					= info.checkItems`
			layout					= info.checkLayout`
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsCustomControl,wItemAtts`,wItemSize`} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			pos						= getLayoutItemPos prevId wItemAtts`
			itPos					= newLayoutItem id pos (checkCustomSize wItemSize`)
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsSliderControl,wItemInfo`,wItemAtts`,wItemSize`} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			pos						= getLayoutItemPos prevId wItemAtts`
			itPos					= newLayoutItem id pos wItemSize`
		
		getLayoutWItem` wMetrics hMargins vMargins spaces _ prevIds prevId cId itemH=:{wItemKind`=IsOtherControl _,wItemAtts`,wItemSize`} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:wItemAtts`]},tb)
		where
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			pos						= getLayoutItemPos prevId wItemAtts`
			itPos					= newLayoutItem id pos wItemSize`
		
		getLayoutWItem` wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH=:{wItemKind`=IsCompoundControl,wItemInfo`,wItemAtts`,wItems`} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:atts1],wItems`=items,wItemInfo`=CompoundInfo` info1},tb1)
		where
			info					= getWItemCompoundInfo` wItemInfo`
			(size,info1,items,atts1,tb1)
									= calcCompoundSize wMetrics hMargins vMargins spaces orientations info wItems` wItemAtts` tb
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			pos						= getLayoutItemPos prevId wItemAtts`
			itPos					= newLayoutItem id pos size
			
			calcCompoundSize :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
									  !CompoundInfo ![WElementHandle`] ![ControlAttribute`] !*OSToolbox
							-> (!Size,!CompoundInfo,![WElementHandle`],![ControlAttribute`],!*OSToolbox)
			calcCompoundSize wMetrics hMargins=:(lMargin,rMargin) vMargins=:(tMargin,bMargin) spaces orientations info itemHs atts tb
				= (okDerivedSize,info1,itemHs1,if hadSize atts2 [ControlViewSize` okDerivedSize:atts2],tb1)
			where
				origin					= info.compoundOrigin
				domainRect				= info.compoundDomain
				hasHScroll				= isJust info.compoundHScroll
				hasVScroll				= isJust info.compoundVScroll
				(minSize,atts1)			= validateMinSize atts
				(hadSize,reqSize,atts2)	= validateCompoundSize wMetrics (rectToRectangle domainRect) (hasHScroll,hasVScroll) atts1
				(_,hMarginAtt)			= cselect iscontrolhmargin` (ControlHMargin` lMargin rMargin) atts2
				newHMargins				= validateControlMargin (getcontrolhmargin` hMarginAtt)
				(_,vMarginAtt)			= cselect iscontrolvmargin` (ControlVMargin` tMargin bMargin) atts2
				newVMargins				= validateControlMargin (getcontrolvmargin` vMarginAtt)
				(_,spaceAtt)			= cselect iscontrolitemspace` (ControlItemSpace` (fst spaces) (snd spaces)) atts2
				newItemSpaces			= validateControlItemSpace (getcontrolitemspace` spaceAtt)
				domain					= rectToRectangle domainRect
				newOrientations			= [(domain,origin):orientations]
				(derSize,itemHs1,tb1)	= layoutControls` wMetrics newHMargins newVMargins newItemSpaces reqSize minSize newOrientations itemHs tb
				okDerivedSize			= validateDerivedCompoundSize` wMetrics domainRect (hasHScroll,hasVScroll) derSize reqSize
				info1					= layoutScrollbars wMetrics okDerivedSize info

				validateMinSize :: ![ControlAttribute`] -> (!Size,![ControlAttribute`])
				validateMinSize atts
					= (okMinSize,if hadMinSize [ControlMinimumSize` okMinSize:atts1] atts1)
				where
					(defMinW,defMinH)			= osMinCompoundSize
					(hadMinSize,minAtt,atts1)	= remove iscontrolminimumsize` (ControlMinimumSize` {w=defMinW,h=defMinH}) atts
					minSize						= getcontrolminimumsize` minAtt
					okMinSize					= {w=max defMinW minSize.w,h=max defMinH minSize.h}

			/*	validateCompoundSize wMetrics viewDomain (hasHScroll,hasVScroll) atts
					validates the Control(View/Outer)Size attribute. The Boolean result is True iff the attribute list contains
					the Control(View/Outer)Size attribute.
					The Booleans hasHScroll hasVScroll should be True iff the compound has the ControlHScroll, ControlVScroll
					attribute set respectively. 
					In addition, the ControlOuterSize attribute is mapped to ControlViewSize attribute.
			*/
				validateCompoundSize :: !OSWindowMetrics !ViewDomain !(!Bool,!Bool) ![ControlAttribute`]
																	-> (!Bool,!Size,![ControlAttribute`])
				validateCompoundSize wMetrics domain hasScrolls atts
					| not hasSize
						= (False,zero,atts)
					| iscontrolviewsize` sizeAtt
						= (True,size1,snd (creplace iscontrolviewsize` (ControlViewSize` size1) atts))
					with
						size			= getcontrolviewsize` sizeAtt
						size1			= {w=max size.w (fst minSize),h=max size.h (snd minSize)}
					| otherwise
						# (w,h)			= toTuple outerSize
						  visScrolls	= osScrollbarsAreVisible wMetrics (rectangleToRect domain) (w,h) hasScrolls
						  viewSize		= rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect {w=w,h=h}))
						# (_,_,atts)	= remove iscontroloutersize` undef atts
						# (_,_,atts)	= remove iscontrolviewsize` undef atts
						= (True,viewSize,[ControlViewSize` viewSize:atts])
					with
						outerSize		= getcontroloutersize` sizeAtt
				where
					(hasSize,sizeAtt)	= cselect (\att->iscontrolviewsize` att || iscontroloutersize` att) undef atts
					minSize				= osMinCompoundSize
				
				validateDerivedCompoundSize` :: !OSWindowMetrics !OSRect !(!Bool,!Bool) Size !Size -> Size
				validateDerivedCompoundSize` wMetrics domain hasScrolls derSize reqSize
					| reqSize==zero		= validateScrollbarSize wMetrics domain hasScrolls derSize
					| otherwise			= validateScrollbarSize wMetrics domain hasScrolls reqSize
				where
					validateScrollbarSize :: !OSWindowMetrics !OSRect !(!Bool,!Bool) !Size -> Size
					validateScrollbarSize wMetrics domainRect (hasHScroll,hasVScroll) size=:{w,h}
						| domainSize==zero			= size
						| visHScroll && visVScroll	= {w=w`,h=h`}
						| visHScroll				= {size & h=h`}
						| visVScroll				= {size & w=w`}
						| otherwise					= size
					where
						domainSize					= rectSize domainRect
						(visHScroll,visVScroll)		= osScrollbarsAreVisible wMetrics domainRect (w,h) (hasHScroll,hasVScroll)
						w`							= w+wMetrics.osmVSliderWidth
						h`							= h+wMetrics.osmHSliderHeight
		
		getLayoutWItem` wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemH=:{wItemKind`=IsLayoutControl,wItemAtts`,wItems`} tb
			= (itPos,prevIds1,id,cId1,{itemH & wItemAtts`=[ControlId` id:atts1],wItems`=items},tb1)
		where
			(size,items,atts1,tb1)	= calcLayoutSize wMetrics hMargins vMargins spaces orientations wItems` wItemAtts` tb
			(id,cId1,prevIds1)		= getLayoutItemId cId itemH.wItemId` prevIds
			pos						= getLayoutItemPos prevId wItemAtts`
			itPos					= newLayoutItem id pos size
			
			calcLayoutSize :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) ![(ViewDomain,Origin)]
									  ![WElementHandle`] ![ControlAttribute`] !*OSToolbox
							-> (!Size,![WElementHandle`],![ControlAttribute`],!*OSToolbox)
			calcLayoutSize wMetrics hMargins=:(lMargin,rMargin) vMargins=:(tMargin,bMargin) spaces orientations itemHs atts tb
				= (okDerivedSize,itemHs1,if hadSize atts2 [ControlViewSize` okDerivedSize:atts2],tb1)
			where
				(minSize,atts1)			= validateMinSize atts
				(hadSize,reqSize,atts2)	= validateLayoutSize wMetrics atts1
				(_,hMarginAtt)			= cselect iscontrolhmargin` (ControlHMargin` lMargin rMargin) atts2
				newHMargins				= validateControlMargin (getcontrolhmargin` hMarginAtt)
				(_,vMarginAtt)			= cselect iscontrolvmargin` (ControlVMargin` tMargin bMargin) atts2
				newVMargins				= validateControlMargin (getcontrolvmargin` vMarginAtt)
				(_,spaceAtt)			= cselect iscontrolitemspace` (ControlItemSpace` (fst spaces) (snd spaces)) atts2
				newItemSpaces			= validateControlItemSpace (getcontrolitemspace` spaceAtt)
				(derSize,itemHs1,tb1)	= layoutControls` wMetrics newHMargins newVMargins newItemSpaces reqSize minSize orientations itemHs tb
				okDerivedSize			= validateDerivedLayoutSize wMetrics derSize reqSize

				validateMinSize :: ![ControlAttribute`] -> (!Size,![ControlAttribute`])
				validateMinSize atts
					= (okMinSize,if hadMinSize [ControlMinimumSize` okMinSize:atts1] atts1)
				where
					(hadMinSize,minAtt,atts1)	= remove iscontrolminimumsize` (ControlMinimumSize` zero) atts
					minSize						= getcontrolminimumsize` minAtt
					okMinSize					= {w=max 0 minSize.w,h=max 0 minSize.h}

			/*	validateLayoutSize wMetrics atts
					validates the Control(View/Outer)Size attribute. The Boolean result is True iff the attribute list contains
					the Control(View/Outer)Size attribute.
					In addition, the ControlOuterSize attribute is mapped to ControlViewSize attribute (identical value).
			*/
				validateLayoutSize :: !OSWindowMetrics ![ControlAttribute`] -> (!Bool,!Size,![ControlAttribute`])
				validateLayoutSize wMetrics atts
					| not hasSize
						= (False,zero,atts)
					| otherwise
						# size1			= {w=max size.w 0,h=max size.h 0}
						# (_,_,atts)	= remove iscontroloutersize` undef atts
						# (_,_,atts)	= remove iscontrolviewsize`  undef atts
						= (True,size1,[ControlViewSize` size1:atts])
				where
					(hasSize,sizeAtt)	= cselect (\att->iscontrolviewsize` att || iscontroloutersize` att) undef atts
					size				= if (iscontrolviewsize` sizeAtt) (getcontrolviewsize` sizeAtt) (getcontroloutersize` sizeAtt)

		getLayoutWItem` _ _ _ _ _ _ _ _ _ _
			= controllayoutFatalError "getLayoutWItem`" "unmatched control implementation alternative"
		
		getLayoutItemPos :: Id ![ControlAttribute`] -> ItemPos
		getLayoutItemPos prevId atts
			= (itemLoc1,offset)
		where
			(itemLoc,offset)	= getcontrolpos` (snd (cselect iscontrolpos` (ControlPos` (RightTo prevId,NoOffset)) atts))
			itemLoc1			= if (isRelativeToPrev itemLoc) (setRelativeTo prevId itemLoc) itemLoc
	
	getLayoutItem` wMetrics hMargins vMargins spaces orientations prevIds prevId cId (WRecursiveHandle` itemHs kind) tb
		# (itPoss,prevIds,prevId,cId,itemHs,tb)	= getLayoutItems` wMetrics hMargins vMargins spaces orientations prevIds prevId cId itemHs tb
		= (itPoss,prevIds,prevId,cId,WRecursiveHandle` itemHs kind,tb)

getLayoutItems` _ _ _ _ _ prevIds prevId cId [] tb
	= ([],prevIds,prevId,cId,[],tb)


/*	Functions shared above.
*/
validateControlMargin :: !(!Int,!Int) -> (!Int,!Int)
validateControlMargin (a,b) = (max 0 a,max 0 b)

validateControlItemSpace :: !(!Int,!Int) -> (!Int,!Int)
validateControlItemSpace (hspace,vspace) = (max 0 hspace,max 0 vspace)

validateDerivedLayoutSize :: !OSWindowMetrics Size !Size -> Size
validateDerivedLayoutSize wMetrics derSize reqSize
	| reqSize==zero		= derSize
	| otherwise			= reqSize

layoutScrollbars :: !OSWindowMetrics !Size !CompoundInfo -> CompoundInfo
layoutScrollbars wMetrics size info=:{compoundHScroll,compoundVScroll}
	= {	info & compoundHScroll=mapMaybe (layoutScrollbar hRect) compoundHScroll
			 , compoundVScroll=mapMaybe (layoutScrollbar vRect) compoundVScroll
	  }
where
	hasScrolls	= (isJust compoundHScroll,isJust compoundVScroll)	// PA: this should actually become: (visHScroll,visVScroll)!!
	rect		= sizeToRect size
	hRect		= osGetCompoundHScrollRect wMetrics hasScrolls rect
	vRect		= osGetCompoundVScrollRect wMetrics hasScrolls rect
	
	layoutScrollbar :: OSRect !ScrollInfo -> ScrollInfo
	layoutScrollbar r=:{rleft,rtop} scrollInfo
		= {scrollInfo & scrollItemPos={x=rleft,y=rtop},scrollItemSize=rectSize r}

position_items :: !(Vector2 -> .x -> .x) !Int !Int ![Int] ![[.x]] -> [[.x]]
position_items setPosition itemHeight left [maxwidth:maxwidths] [col:cols]
	# col	= position_items` setPosition itemHeight {vx=left,vy=0} col
	  cols	= position_items  setPosition itemHeight (left+maxwidth) maxwidths cols
	= [col:cols]
where
	position_items` :: !(Vector2 -> .x -> .x) !Int !Vector2 ![.x] -> [.x]
	position_items` setPosition itemHeight pos [item:items]
		= [setPosition pos item:position_items` setPosition itemHeight {pos & vy=pos.vy+itemHeight} items]
	position_items` _ _ _ _
		= []
position_items _ _ _ _ _
	= []

toColumns :: !RowsOrColumns ![x] -> [[x]]
toColumns (Columns n) items
	= repeat_splitting perColumn items
where
	nrItems		= length items
	n`			= max 1 n
	perColumn	= if (nrItems rem n`==0) (nrItems/n`) (nrItems/n`+1)
	
	repeat_splitting :: !Int ![x] -> [[x]]
	repeat_splitting n items
		# (before,after)= split n items
		| isEmpty after	= [before]
		| otherwise		= [before:repeat_splitting n after]
toColumns (Rows n) items
	= repeat_spreading nrColumns items cols
where
	nrItems		= length items
	n`			= setBetween n 1 nrItems
	nrColumns	= if (nrItems rem n`==0) (nrItems/n`) (nrItems/n`+1)
	cols		= repeatn nrColumns []
	
	repeat_spreading :: !Int ![x] ![[x]] -> [[x]]
	repeat_spreading n items cols
		# (before,after)= split n items
		| isEmpty after	= spread before cols
		| otherwise		= spread before (repeat_spreading n after cols)
	where
		spread :: v:[u:a] w:[v:[u:a]] -> w:[v:[u:a]], [v<=u,w<=v]
		spread [x:xs] [ys:zs]	= [[x:ys]:spread xs zs]
		spread [] zs			= zs
	
fromColumns :: !RowsOrColumns ![[x]] -> [x]
fromColumns (Columns _) items
	= flatten items
fromColumns (Rows _) items
	= repeat_collecting items
where
	repeat_collecting :: ![[x]] -> [x]
	repeat_collecting items
		# (before,after)= collect items
		| isEmpty after	= before
		| otherwise		= before++repeat_collecting after
	where
		collect :: ![[x]] -> (![x],![[x]])
		collect [[x:xs]:ys]
			# (zs,ys)	= collect ys
			= ([x:zs],[xs:ys])
		collect _
			= ([],[])

listmax :: ![Int] -> Int
listmax [x:xs]	= foldr max x xs
listmax _		= 0

checkCustomSize :: !Size -> Size
checkCustomSize {w,h} = {w=max 0 w,h=max 0 h}

getLayoutItemId :: !Int !(Maybe Id) [Id] -> (Id,Int,[Id])
getLayoutItemId cId maybeId prevIds
	| isNothing maybeId		= (sysId cId,cId-1,prevIds)
	| isMember id prevIds	= (sysId cId,cId-1,prevIds)
	| otherwise				= (id,		 cId,  [id:prevIds])
where
	id						= fromJust maybeId

newLayoutItem :: !Id !ItemPos !Size -> LayoutItem
newLayoutItem id pos size
	= {liId=id,liItemPos=pos,liItemSize=size}

isRelativeToPrev :: !ItemLoc -> Bool
isRelativeToPrev itemLoc = itemLoc==LeftOfPrev || itemLoc==RightToPrev || itemLoc==AbovePrev || itemLoc==BelowPrev

setRelativeTo :: !Id !ItemLoc -> ItemLoc
setRelativeTo id LeftOfPrev		= LeftOf	id
setRelativeTo id RightToPrev	= RightTo	id
setRelativeTo id AbovePrev		= Above		id
setRelativeTo id BelowPrev		= Below		id


/*	After calculating the layout of the elements, the calculated control positions and sizes must be added 
	to the original WElementHandle(`) list. 
	In case of recursive WElementHandles (WList/WElim/WIntro/WExtend/WChange(LSHandle), and (Compound/Layout)Controls) 
	the recursively calculated positions must also be added.
	In case of (Radio/Check)Controls the already calculated (radio/check) control positions that are
	oriented at base zero need to be shifted to the calculated base position of the (Radio/Check)Control. 
	
	Note that setLayoutItems relies on the fact that the hd element of the attribute
	list contains the ControlId attribute, used for computing the layout (as provided by
	getLayoutItems)! It will remove this element from the attribute list.
*/
setLayoutItems :: ![Root] ![WElementHandle .ls .pst] -> (![Root],![WElementHandle .ls .pst])
setLayoutItems roots [itemH:itemHs]
	# (roots,itemH )	= setLayoutItem  roots itemH
	# (roots,itemHs)	= setLayoutItems roots itemHs
	= (roots,[itemH:itemHs])
where
	setLayoutItem :: ![Root] !(WElementHandle .ls .pst) -> (![Root],!WElementHandle .ls .pst)
	setLayoutItem roots (WItemHandle itemH=:{wItemVirtual})
		| wItemVirtual
			= (roots,WItemHandle itemH)
		| otherwise
			# (roots,itemH)	= setLayoutWItem roots itemH
			= (roots,WItemHandle itemH)
	where
		setLayoutWItem :: ![Root] !(WItemHandle .ls .pst) -> (![Root],!WItemHandle .ls .pst)
		setLayoutWItem roots itemH=:{wItemKind,wItemInfo,wItems,wItemAtts=[ControlId id:atts]}
			#! (layoutInfo,corner,size,roots)	= getLayoutItem id roots
		//	#! itemHs							= map (shiftCompounds layoutInfo corner) wItems		PA: not needed anymore, because positions are relative
		//	#! info								= shiftWItemInfo corner wItemInfo					PA: same
			= (	roots
			  ,	{	itemH	& wItemAtts			= atts
		//					, wItemInfo			= info
		//					, wItems			= itemHs
							, wItemPos			= corner//{x=corner.vx,y=corner.vy}
							, wItemSize			= size
							, wItemLayoutInfo	= layoutInfo
				}
			  )
/*		where
			shiftCompounds :: !LayoutInfo !Vector2 !(WElementHandle .ls .pst) -> WElementHandle .ls .pst
			shiftCompounds layoutInfo offset (WItemHandle itemH)
				#! itemH	= shiftCompound layoutInfo offset itemH
				= WItemHandle itemH
			where
				shiftCompound :: !LayoutInfo !Vector2 !(WItemHandle .ls .pst) -> WItemHandle .ls .pst
				shiftCompound layoutInfo offset itemH=:{wItemKind,wItemInfo,wItemPos,wItems,wItemLayoutInfo}
					#! layoutInfo				= if (layoutInfo==LayoutFix) layoutInfo wItemLayoutInfo
					#! itemHs					= map (shiftCompounds layoutInfo offset) wItems
					#! info						= shiftWItemInfo offset wItemInfo
					= {	itemH &	wItemPos		= movePoint offset wItemPos
							  ,	wItems			= itemHs
							  ,	wItemInfo		= info
							  , wItemLayoutInfo	= layoutInfo
					  }
			shiftCompounds layoutInfo offset (WListLSHandle itemHs)
				= WListLSHandle (map (shiftCompounds layoutInfo offset) itemHs)
			shiftCompounds layoutInfo offset (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				= WExtendLSHandle {wExH & wExtendItems=map (shiftCompounds layoutInfo offset) itemHs}
			shiftCompounds layoutInfo offset (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				= WChangeLSHandle {wChH & wChangeItems=map (shiftCompounds layoutInfo offset) itemHs}
			
			shiftWItemInfo :: !Vector2 !(WItemInfo .ls .pst) -> WItemInfo .ls .pst
			shiftWItemInfo offset (CheckInfo info=:{checkItems})
				= CheckInfo {info & checkItems=map shiftCheckItem checkItems}
			where
				shiftCheckItem :: !(CheckItemInfo .st) -> CheckItemInfo .st
				shiftCheckItem item=:{checkItemPos}
					= {item & checkItemPos=movePoint offset checkItemPos}
			shiftWItemInfo offset (RadioInfo info=:{radioItems})
				= RadioInfo {info & radioItems=map shiftRadioItem radioItems}
			where
				shiftRadioItem :: !(RadioItemInfo .st) -> RadioItemInfo .st
				shiftRadioItem item=:{radioItemPos}
					= {item & radioItemPos=movePoint offset radioItemPos}
			shiftWItemInfo offset (CompoundInfo info=:{compoundHScroll,compoundVScroll})
				= CompoundInfo {info & compoundHScroll=mapMaybe shiftScrollbar compoundHScroll
									 , compoundVScroll=mapMaybe shiftScrollbar compoundVScroll
							   }
			where
				shiftScrollbar :: !ScrollInfo -> ScrollInfo
				shiftScrollbar info=:{scrollItemPos}
					= {info & scrollItemPos=movePoint offset scrollItemPos}
			shiftWItemInfo _ info
				= info */
		setLayoutWItem _ _
			= controllayoutFatalError "setLayoutWItem" "WElementHandle has no ControlId"
	setLayoutItem roots (WListLSHandle itemHs)
		# (roots,itemHs)	= setLayoutItems roots itemHs
		= (roots,WListLSHandle itemHs)
	setLayoutItem roots (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (roots,itemHs)	= setLayoutItems roots itemHs
		= (roots,WExtendLSHandle {wExH & wExtendItems=itemHs})
	setLayoutItem roots (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (roots,itemHs)	= setLayoutItems roots itemHs
		= (roots,WChangeLSHandle {wChH & wChangeItems=itemHs})
setLayoutItems roots itemHs
	= (roots,itemHs)

setLayoutItems` :: ![Root] ![WElementHandle`] -> (![Root],![WElementHandle`])
setLayoutItems` roots [itemH:itemHs]
	# (roots,itemH)		= setLayoutItem`  roots itemH
	# (roots,itemHs)	= setLayoutItems` roots itemHs
	= (roots,[itemH:itemHs])
where
	setLayoutItem` :: ![Root] !WElementHandle` -> (![Root],!WElementHandle`)
	setLayoutItem` roots (WItemHandle` itemH=:{wItemVirtual`})
		| wItemVirtual`
			= (roots,WItemHandle` itemH)
		| otherwise
			# (roots,itemH)	= setLayoutWItem` roots itemH
			= (roots,WItemHandle` itemH)
	where
		setLayoutWItem` :: ![Root] !WItemHandle` -> (![Root],!WItemHandle`)
		setLayoutWItem` roots itemH=:{wItemKind`,wItemInfo`,wItems`,wItemAtts`=[ControlId` id:atts]}
			# (layoutInfo,corner,size,roots)	= getLayoutItem id roots
		//	  itemHs							= if (isRecursiveControl wItemKind`) (map (shiftCompounds layoutInfo corner) wItems`) wItems`	PA: see above
		//	  info								= shiftWItemInfo corner wItemInfo`																PA: see above
			= (	roots
			  ,	{	itemH	& wItemAtts`		= atts
		//					, wItemInfo`		= info
		//					, wItems`			= itemHs
							, wItemPos`			= corner//{x=corner.vx,y=corner.vy}
							, wItemSize`		= size
							, wItemLayoutInfo`	= layoutInfo
				}
			  )
/*		where
			shiftCompounds :: !LayoutInfo !Vector2 !WElementHandle` -> WElementHandle`
			shiftCompounds layoutInfo offset (WItemHandle` itemH)
				= WItemHandle` (shiftCompound layoutInfo offset itemH)
			where
				shiftCompound :: !LayoutInfo !Vector2 !WItemHandle` -> WItemHandle`
				shiftCompound layoutInfo offset itemH=:{wItemKind`,wItemInfo`,wItemPos`,wItems`,wItemLayoutInfo`}
					# layoutInfo				= if (layoutInfo==LayoutFix) layoutInfo wItemLayoutInfo`
					= {	itemH &	wItemPos`		= movePoint offset wItemPos`
							  ,	wItems`			= if (isRecursiveControl wItemKind`) (map (shiftCompounds layoutInfo offset) wItems`) wItems`
							  ,	wItemInfo`		= shiftWItemInfo offset wItemInfo`
							  , wItemLayoutInfo`= layoutInfo
					  }
			shiftCompounds layoutInfo offset (WRecursiveHandle` itemHs kind)
				= WRecursiveHandle` (map (shiftCompounds layoutInfo offset) itemHs) kind
			
			shiftWItemInfo :: !Vector2 !WItemInfo` -> WItemInfo`
			shiftWItemInfo offset (CheckInfo` info=:{checkItems`})
				= CheckInfo` {info & checkItems`=map shiftCheckItem checkItems`}
			where
				shiftCheckItem :: !CheckItemInfo` -> CheckItemInfo`
				shiftCheckItem item=:{checkItemPos`}
					= {item & checkItemPos`=movePoint offset checkItemPos`}
			shiftWItemInfo offset (RadioInfo` info=:{radioItems`})
				= RadioInfo` {info & radioItems`=map shiftRadioItem radioItems`}
			where
				shiftRadioItem :: !RadioItemInfo` -> RadioItemInfo`
				shiftRadioItem item=:{radioItemPos`}
					= {item & radioItemPos`=movePoint offset radioItemPos`}
			shiftWItemInfo offset (CompoundInfo` info=:{compoundHScroll,compoundVScroll})
				= CompoundInfo` {info & compoundHScroll=mapMaybe shiftScrollbar compoundHScroll
									  , compoundVScroll=mapMaybe shiftScrollbar compoundVScroll
							    }
			where
				shiftScrollbar :: !ScrollInfo -> ScrollInfo
				shiftScrollbar info=:{scrollItemPos}
					= {info & scrollItemPos=movePoint offset scrollItemPos}
			shiftWItemInfo _ info
				= info */
		setLayoutWItem` _ _
			= controllayoutFatalError "setLayoutWItem`" "WElementHandle` has no ControlId`"
	
	setLayoutItem` roots (WRecursiveHandle` itemHs kind)
		# (roots,itemHs)	= setLayoutItems` roots itemHs
		= (roots,WRecursiveHandle` itemHs kind)
setLayoutItems` roots itemHs
	= (roots,itemHs)
