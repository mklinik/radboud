implementation module controlaccess


import	StdBool, StdFunc, StdInt, StdList, StdMisc, StdTuple
import	commondef, id, windowaccess, wstateaccess
import	oswindow


eqfst3id :: !Id !(!Id,.x,.y) -> Bool
eqfst3id id1 (id2,_,_)
	= id1==id2


/*	Higher order access on [WElementHandle`].	*/

mapWElementHandles` :: (WItemHandle`->v:[u:x]) ![WElementHandle`] -> v:[u:x], [v<=u]
mapWElementHandles` f itemHs
	| isEmpty itemHs
		= []
	| otherwise
		# (itemH,itemHs)	= hdtl itemHs
		= mapWElementHandle` f itemH ++ mapWElementHandles` f itemHs
where
	mapWElementHandle` :: (WItemHandle`->v:[u:x]) !WElementHandle` -> v:[u:x], [v<=u]
	mapWElementHandle` f (WRecursiveHandle`	itemHs _)
		= mapWElementHandles` f itemHs
	mapWElementHandle` f (WItemHandle` itemH)
		= f itemH

seekmapWElementHandle` :: (WItemHandle`->(Bool,x)) x ![WElementHandle`] -> (!Bool,x)
seekmapWElementHandle` f x itemHs
	| isEmpty itemHs
		= (False,x)
	# (itemH,itemHs)	= hdtl itemHs
	  (found,x)			= seekmapWItemHandle` f x itemH
	| found
		= (found,x)
	| otherwise
		= seekmapWElementHandle` f x itemHs
where
	seekmapWItemHandle` :: (WItemHandle`->(Bool,x)) x !WElementHandle` -> (!Bool,x)
	seekmapWItemHandle` f x (WRecursiveHandle` itemHs _)
		= seekmapWElementHandle` f x itemHs
	seekmapWItemHandle` f x (WItemHandle` itemH)
		= f itemH

statemapWElementHandles` :: !(Cond x) (WItemHandle` x -> x) !x ![WElementHandle`] -> x
statemapWElementHandles` cond f s itemHs
	| cond s			= s
	| isEmpty itemHs	= s
	| otherwise
		# (itemH,itemHs)= hdtl itemHs
		# s				= statemapWElementHandle`  cond f s itemH
		# s				= statemapWElementHandles` cond f s itemHs
		= s
where
	statemapWElementHandle` :: (Cond x) (WItemHandle` x -> x) x !WElementHandle` -> x
	statemapWElementHandle` cond f s (WRecursiveHandle`	itemHs _)
		= statemapWElementHandles` cond f s itemHs
	statemapWElementHandle` cond f s (WItemHandle` itemH)
		= f itemH s


/*	Access operations on WElementHandle`.	*/

getcontrolstypes :: ![WElementHandle`] -> [(ControlType,Maybe Id)]
getcontrolstypes itemHs
	= mapWElementHandles` gettype itemHs
where
	gettype :: !WItemHandle` -> [(ControlType,Maybe Id)]
	gettype {wItemId`,wItemKind`}
		= [(getcontroltype wItemKind`,wItemId`)]
	where
		getcontroltype :: !ControlKind -> ControlType
		getcontroltype IsButtonControl			= "ButtonControl"
		getcontroltype IsCheckControl			= "CheckControl"
		getcontroltype IsCompoundControl		= "CompoundControl"
		getcontroltype IsCustomButtonControl	= "CustomButtonControl"
		getcontroltype IsCustomControl			= "CustomControl"
		getcontroltype IsEditControl			= "EditControl"
		getcontroltype IsLayoutControl			= "LayoutControl"
		getcontroltype IsPopUpControl			= "PopUpControl"
		getcontroltype IsRadioControl			= "RadioControl"
		getcontroltype IsSliderControl			= "SliderControl"
		getcontroltype IsTextControl			= "TextControl"
		getcontroltype (IsOtherControl type)	= type

getcompoundstypes :: !Id ![WElementHandle`] -> [(ControlType,Maybe Id)]
getcompoundstypes id itemHs
	= snd (getcompoundstypes` id itemHs)
where
	getcompoundstypes` :: !Id ![WElementHandle`] -> (Bool,[(ControlType,Maybe Id)])
	getcompoundstypes` id itemHs
		= seekmapWElementHandle` (getcompoundtypes id) [] itemHs
	where
		getcompoundtypes :: !Id !WItemHandle` -> (!Bool,[(ControlType,Maybe Id)])
		getcompoundtypes id {wItemId`,wItemKind`,wItems`}
			| wItemKind`<>IsCompoundControl	= (False,[])
			| isNothing wItemId`			= getcompoundstypes` id wItems`
			| id<>fromJust wItemId`			= getcompoundstypes` id wItems`
			| otherwise						= (True,getcontrolstypes wItems`)

getcontrolslayouts :: !Point2 ![WElementHandle`] !(![Id],![(Id,Bool,(Maybe ItemPos,Vector2))])
                                               -> (![Id],![(Id,Bool,(Maybe ItemPos,Vector2))])
getcontrolslayouts parentPos itemHs ids_layouts
	= statemapWElementHandles` (isEmpty o fst) (getlayouts parentPos) ids_layouts itemHs
where
	getlayouts :: !Point2 !WItemHandle` !(![Id],![(Id,Bool,(Maybe ItemPos,Vector2))])
	                                  -> (![Id],![(Id,Bool,(Maybe ItemPos,Vector2))])
	getlayouts parentPos itemH=:{wItemId`,wItemAtts`,wItems`,wItemPos`} ids_layouts=:(ids,layouts)
		| isNothing wItemId`= getcontrolslayouts absolutePos wItems` ids_layouts
		| not hadId			= getcontrolslayouts absolutePos wItems` ids_layouts
		| otherwise			= getcontrolslayouts absolutePos wItems` (ids1,layouts1)
		with
			itemPos			= if hasAtt (Just (getcontrolpos` posAtt)) Nothing
			(hasAtt,posAtt)	= cselect iscontrolpos` (ControlPos` (Left,NoOffset)) wItemAtts`
			layout			= (itemId,True,(itemPos,toVector absolutePos))
			(_,layouts1)	= creplace (eqfst3id itemId) layout layouts
	where
		absolutePos			= movePoint wItemPos` parentPos
		itemId				= fromJust wItemId`
		(hadId,ids1)		= removeCheck itemId ids

getcontrolsviewsizes :: !OSWindowMetrics ![WElementHandle`] !(![Id],![(Id,Bool,Size)]) -> (![Id],![(Id,Bool,Size)])
getcontrolsviewsizes wMetrics itemHs ids_sizes
	= statemapWElementHandles` (isEmpty o fst) (getsizes wMetrics) ids_sizes itemHs
where
	getsizes :: !OSWindowMetrics !WItemHandle` !(![Id],![(Id,Bool,Size)]) -> (![Id],![(Id,Bool,Size)])
	getsizes wMetrics itemH=:{wItemId`,wItemKind`,wItems`} ids_sizes=:(ids,sizes)
		| isNothing wItemId`= getcontrolsviewsizes wMetrics wItems` ids_sizes
		| not hadId			= getcontrolsviewsizes wMetrics wItems` ids_sizes
		| otherwise			= getcontrolsviewsizes wMetrics wItems` (ids1,sizes1)
		with
			itemSize		= itemH.wItemSize`
			info			= getWItemCompoundInfo` itemH.wItemInfo`
			(domainRect,hasScrolls)
							= (info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
			visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
			size			= if (wItemKind`<>IsCompoundControl)
 								 itemSize
								 (rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect itemSize)))
			(_,sizes1)		= creplace (eqfst3id itemId) (itemId,True,size) sizes
	where
		itemId				= fromJust wItemId`
		(hadId,ids1)		= removeCheck itemId ids

getcontrolsoutersizes :: !OSWindowMetrics ![WElementHandle`] !(![Id],![(Id,Bool,Size)]) -> (![Id],![(Id,Bool,Size)])
getcontrolsoutersizes wMetrics itemHs ids_sizes
	= statemapWElementHandles` (isEmpty o fst) (getsizes wMetrics) ids_sizes itemHs
where
	getsizes :: !OSWindowMetrics !WItemHandle` !(![Id],![(Id,Bool,Size)]) -> (![Id],![(Id,Bool,Size)])
	getsizes wMetrics itemH=:{wItemId`,wItemKind`,wItems`} ids_sizes=:(ids,sizes)
		| isNothing wItemId`= getcontrolsoutersizes wMetrics wItems` ids_sizes
		| not hadId			= getcontrolsoutersizes wMetrics wItems` ids_sizes
		| otherwise			= getcontrolsoutersizes wMetrics wItems` (ids1,sizes1)
		with
			size			= itemH.wItemSize`
			(_,sizes1)		= creplace (eqfst3id itemId) (itemId,True,size) sizes
	where
		itemId				= fromJust wItemId`
		(hadId,ids1)		= removeCheck itemId ids

getcontrolsselects :: ![WElementHandle`] !(![Id],![(Id,Bool,SelectState)]) -> (![Id],![(Id,Bool,SelectState)])
getcontrolsselects itemHs ids_selects
	= statemapWElementHandles` (isEmpty o fst) getselects ids_selects itemHs
where
	getselects :: !WItemHandle` !(![Id],![(Id,Bool,SelectState)]) -> (![Id],![(Id,Bool,SelectState)])
	getselects itemH=:{wItemId`,wItemSelect`,wItems`} ids_selects=:(ids,selects)
		| isNothing wItemId`	= getcontrolsselects wItems` ids_selects
		| not hadId				= getcontrolsselects wItems` ids_selects
		| otherwise				= getcontrolsselects wItems` (ids1,selects1)
								with
									selectstate	= if wItemSelect` Able Unable
									select		= (itemId,True,selectstate)
									(_,selects1)= creplace (eqfst3id itemId) select selects
	where
		itemId					= fromJust wItemId`
		(hadId,ids1)			= removeCheck itemId ids

getcontrolsshowstates :: ![WElementHandle`] !(![Id],![(Id,Bool,Bool)]) -> (![Id],![(Id,Bool,Bool)])
getcontrolsshowstates itemHs ids_shows
	= statemapWElementHandles` (isEmpty o fst) getshowstates ids_shows itemHs
where
	getshowstates :: !WItemHandle` !(![Id],![(Id,Bool,Bool)]) -> (![Id],![(Id,Bool,Bool)])
	getshowstates itemH=:{wItemId`,wItemShow`,wItems`} ids_shows=:(ids,shows)
		| isNothing wItemId`	= getcontrolsshowstates wItems` ids_shows
		| not hadId				= getcontrolsshowstates wItems` ids_shows
		| otherwise				= getcontrolsshowstates wItems` (ids1,shows1)
								with
									show		= (itemId,True,wItemShow`)
									(_,shows1)	= creplace (eqfst3id itemId) show shows
	where
		itemId					= fromJust wItemId`
		(hadId,ids1)			= removeCheck itemId ids

getcontrolstexts :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe String)]) -> (![Id],![(Id,Bool,Maybe String)])
getcontrolstexts itemHs ids_texts
	= statemapWElementHandles` (isEmpty o fst) gettext ids_texts itemHs
where
	gettext :: !WItemHandle` !(![Id],![(Id,Bool,Maybe String)]) -> (![Id],![(Id,Bool,Maybe String)])
	gettext itemH=:{wItemId`,wItemKind`,wItems`,wItemInfo`} ids_texts=:(ids,texts)
		| isNothing wItemId`	= getcontrolstexts wItems` ids_texts
		| not hadId				= getcontrolstexts wItems` ids_texts
		| not hastext			= getcontrolstexts wItems` (ids1,texts )
		| otherwise				= getcontrolstexts wItems` (ids1,texts1)
								with
									(_,texts1)	= creplace (eqfst3id itemId) (itemId,True,Just textline) texts
	where
		itemId					= fromJust wItemId`
		(hadId,ids1)			= removeCheck itemId ids
		(hastext,textline)		= case wItemKind` of
									IsPopUpControl	-> (True,getPopUpText (getWItemPopUpInfo`  wItemInfo`))
									IsTextControl	-> (True,(getWItemTextInfo`   wItemInfo`).textInfoText)
									IsEditControl	-> (True,(getWItemEditInfo`   wItemInfo`).editInfoText)
									IsButtonControl	-> (True,(getWItemButtonInfo` wItemInfo`).buttonInfoText)
									_				-> (False,undef)
		
		getPopUpText :: !PopUpInfo` -> String
		getPopUpText popUpInfo=:{popUpInfoEdit`,popUpInfoItems`}
			| isNothing popUpInfoEdit`
				| isEmpty popUpInfoItems`
					= ""
				// otherwise
					= popUpInfoItems`!!(max 0 (popUpInfo.popUpInfoIndex`-1))
			| otherwise
				= (fromJust popUpInfoEdit`).popUpEditText

getcontrolsnrlines :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe Int)]) -> (![Id],![(Id,Bool,Maybe Int)])
getcontrolsnrlines itemHs ids_nrlines
	= statemapWElementHandles` (isEmpty o fst) getnrlines ids_nrlines itemHs
where
	getnrlines :: !WItemHandle` !(![Id],![(Id,Bool,Maybe Int)]) -> (![Id],![(Id,Bool,Maybe Int)])
	getnrlines itemH=:{wItemId`,wItemKind`,wItems`} ids_nrlines=:(ids,nrlines)
		| isNothing wItemId`		= getcontrolsnrlines wItems` ids_nrlines
		| not hadId					= getcontrolsnrlines wItems` ids_nrlines
		| wItemKind`<>IsEditControl	= getcontrolsnrlines wItems` (ids1,nrlines )
		| otherwise					= getcontrolsnrlines wItems` (ids1,nrlines1)
									with
										info		= getWItemEditInfo` itemH.wItemInfo`
										nrline		= (itemId,True,Just info.editInfoNrLines)
										(_,nrlines1)= creplace (eqfst3id itemId) nrline nrlines
	where
		itemId						= fromJust wItemId`
		(hadId,ids1)				= removeCheck itemId ids

getcontrolslooks :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe (Bool,Look))]) -> (![Id],![(Id,Bool,Maybe (Bool,Look))])
getcontrolslooks itemHs ids_looks
	= statemapWElementHandles` (isEmpty o fst) getlooks ids_looks itemHs
where
	getlooks :: !WItemHandle` !(![Id],![(Id,Bool,Maybe (Bool,Look))]) -> (![Id],![(Id,Bool,Maybe (Bool,Look))])
	getlooks itemH=:{wItemId`,wItemKind`,wItems`,wItemInfo`} ids_looks=:(ids,looks)
		| isNothing wItemId`= getcontrolslooks wItems` ids_looks
		| not hadId			= getcontrolslooks wItems` ids_looks
		| not haslook		= getcontrolslooks wItems` (ids1,looks )
		| otherwise			= getcontrolslooks wItems` (ids1,looks1)
							with
								(_,looks1)	= creplace (eqfst3id itemId) (itemId,True,look) looks
	where
		itemId				= fromJust wItemId`
		(hadId,ids1)		= removeCheck itemId ids
		(haslook,look)		= getlook wItemKind` wItemInfo`
		
		getlook :: !ControlKind !WItemInfo` -> (!Bool,Maybe (Bool,Look))
		getlook IsCustomButtonControl info
			= (True,Just (lookInfo.lookSysUpdate,lookInfo.lookFun))
		where
			lookInfo	= (getWItemCustomButtonInfo` info).cButtonInfoLook
		getlook IsCustomControl info
			= (True,Just (lookInfo.lookSysUpdate,lookInfo.lookFun))
		where
			lookInfo	= (getWItemCustomInfo` info).customInfoLook
		getlook IsCompoundControl info
			= (True,Just (lookInfo.lookSysUpdate,lookInfo.lookFun))
		where
			lookInfo	= (getWItemCompoundInfo` info).compoundLookInfo.compoundLook
		getlook _ _
			= (False,undef)

getcontrolsminsizes :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe Size)]) -> (![Id],![(Id,Bool,Maybe Size)])
getcontrolsminsizes itemHs ids_sizes
	= statemapWElementHandles` (isEmpty o fst) getsizes ids_sizes itemHs
where
	getsizes :: !WItemHandle` !(![Id],![(Id,Bool,Maybe Size)]) -> (![Id],![(Id,Bool,Maybe Size)])
	getsizes itemH=:{wItemId`,wItemAtts`,wItems`} ids_sizes=:(ids,sizes)
		| isNothing wItemId`	= getcontrolsminsizes wItems` ids_sizes
		| not hadId				= getcontrolsminsizes wItems` ids_sizes
		| otherwise				= getcontrolsminsizes wItems` (ids1,sizes1)
								with
									(has_minsize,minatt)= cselect iscontrolminimumsize` (dummy "getcontrolsminsizes") wItemAtts`
									size				= (itemId,True,if has_minsize (Just (getcontrolminimumsize` minatt)) Nothing)
									(_,sizes1)			= creplace (eqfst3id itemId) size sizes
	where
		itemId					= fromJust wItemId`
		(hadId,ids1)			= removeCheck itemId ids

getcontrolsresizes :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe ControlResizeFunction)])
									   -> (![Id],![(Id,Bool,Maybe ControlResizeFunction)])
getcontrolsresizes itemHs ids_resizes
	= statemapWElementHandles` (isEmpty o fst) getresizes ids_resizes itemHs
where
	getresizes :: !WItemHandle` !(![Id],![(Id,Bool,Maybe ControlResizeFunction)])
			  				  -> (![Id],![(Id,Bool,Maybe ControlResizeFunction)])
	getresizes itemH=:{wItemId`,wItemAtts`,wItems`} ids_resizes=:(ids,resizes)
		| isNothing wItemId`	= getcontrolsresizes wItems` ids_resizes
		| not hadId				= getcontrolsresizes wItems` ids_resizes
		| not hasResize			= getcontrolsresizes wItems` (ids1,resizes )
		| otherwise				= getcontrolsresizes wItems` (ids1,resizes1)
								with
									resize		= (itemId,True,Just (getcontrolresize` resizeAtt))
									(_,resizes1)= creplace (eqfst3id itemId) resize resizes1
	where
		itemId					= fromJust wItemId`
		(hadId,ids1)			= removeCheck itemId ids
		(hasResize,resizeAtt)	= cselect iscontrolresize` (dummy "getresizes") wItemAtts`

getpopupitems :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe [String])]) -> (![Id],![(Id,Bool,Maybe [String])])
getpopupitems itemHs ids_titles
	= statemapWElementHandles` (isEmpty o fst) getpopuptitle ids_titles itemHs
where
	getpopuptitle :: !WItemHandle` !(![Id],![(Id,Bool,Maybe [String])]) -> (![Id],![(Id,Bool,Maybe [String])])
	getpopuptitle itemH=:{wItemId`,wItemKind`,wItems`,wItemInfo`} ids_titles=:(ids,titles)
		| isNothing wItemId`			= getpopupitems wItems` ids_titles
		| not hadId						= getpopupitems wItems` ids_titles
		| wItemKind`<>IsPopUpControl	= getpopupitems wItems` (ids1,titles )
		| otherwise						= getpopupitems wItems` (ids1,titles1)
										with
											info		= getWItemPopUpInfo` wItemInfo`
											title		= (itemId,True,Just info.popUpInfoItems`)
											(_,titles1)	= creplace (eqfst3id itemId) title titles
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getselectedpopupitems :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe Index)]) -> (![Id],![(Id,Bool,Maybe Index)])
getselectedpopupitems itemHs ids_indices
	= statemapWElementHandles` (isEmpty o fst) getselectedpopup ids_indices itemHs
where
	getselectedpopup :: !WItemHandle` !(![Id],![(Id,Bool,Maybe Index)]) -> (![Id],![(Id,Bool,Maybe Index)])
	getselectedpopup itemH=:{wItemId`,wItemKind`,wItems`,wItemInfo`} ids_indices=:(ids,indices)
		| isNothing wItemId`			= getselectedpopupitems wItems` ids_indices
		| not hadId						= getselectedpopupitems wItems` ids_indices
		| wItemKind`<>IsPopUpControl	= getselectedpopupitems wItems` (ids1,indices )
		| otherwise						= getselectedpopupitems wItems` (ids1,indices1)
										with
											info		= getWItemPopUpInfo` wItemInfo`
											index		= (itemId,True,Just info.popUpInfoIndex`)
											(_,indices1)= creplace (eqfst3id itemId) index indices
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getradioitems :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe [String])]) -> (![Id],![(Id,Bool,Maybe [String])])
getradioitems itemHs ids_titles
	= statemapWElementHandles` (isEmpty o fst) getradiotitle ids_titles itemHs
where
	getradiotitle :: !WItemHandle` !(![Id],![(Id,Bool,Maybe [String])]) -> (![Id],![(Id,Bool,Maybe [String])])
	getradiotitle itemH=:{wItemId`,wItemKind`,wItems`,wItemInfo`} ids_titles=:(ids,titles)
		| isNothing wItemId`			= getradioitems wItems` ids_titles
		| not hadId						= getradioitems wItems` ids_titles
		| wItemKind`<>IsRadioControl	= getradioitems wItems` (ids1,titles )
		| otherwise						= getradioitems wItems` (ids1,titles1)
										with
											info		= getWItemRadioInfo` wItemInfo`
											title		= (itemId,True,Just [fst item.radioItem` \\ item<-info.radioItems`])
											(_,titles1)	= creplace (eqfst3id itemId) title titles
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getradiocontrolsmarks :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe Index)]) -> (![Id],![(Id,Bool,Maybe Index)])
getradiocontrolsmarks itemHs ids_marks
	= statemapWElementHandles` (isEmpty o fst) getmarks ids_marks itemHs
where
	getmarks :: !WItemHandle` !(![Id],![(Id,Bool,Maybe Index)]) -> (![Id],![(Id,Bool,Maybe Index)])
	getmarks itemH=:{wItemId`,wItemKind`,wItemInfo`,wItems`} ids_marks=:(ids,marks)
		| isNothing wItemId`			= getradiocontrolsmarks wItems` ids_marks
		| not hadId						= getradiocontrolsmarks wItems` ids_marks
		| wItemKind`<>IsRadioControl	= getradiocontrolsmarks wItems` (ids1,marks )
		| otherwise						= getradiocontrolsmarks wItems` (ids1,marks1)
										with
											info		= getWItemRadioInfo` wItemInfo`
											mark		= (itemId,True,Just info.radioIndex`)
											(_,marks1)	= creplace (eqfst3id itemId) mark marks
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getcheckitems :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe [String])]) -> (![Id],![(Id,Bool,Maybe [String])])
getcheckitems itemHs ids_titles
	= statemapWElementHandles` (isEmpty o fst) getchecktitle ids_titles itemHs
where
	getchecktitle :: !WItemHandle` !(![Id],![(Id,Bool,Maybe [String])]) -> (![Id],![(Id,Bool,Maybe [String])])
	getchecktitle itemH=:{wItemId`,wItemKind`,wItems`,wItemInfo`} ids_titles=:(ids,titles)
		| isNothing wItemId`			= getcheckitems wItems` ids_titles
		| not hadId						= getcheckitems wItems` ids_titles
		| wItemKind`<>IsCheckControl	= getcheckitems wItems` (ids1,titles )
		| otherwise						= getcheckitems wItems` (ids1,titles1)
										with
											info		= getWItemCheckInfo` wItemInfo`
											title		= (itemId,True,Just [fst3 item.checkItem` \\ item<-info.checkItems`])
											(_,titles1)	= creplace (eqfst3id itemId) title titles
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getcheckcontrolsmarks :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe [Index])]) -> (![Id],![(Id,Bool,Maybe [Index])])
getcheckcontrolsmarks itemHs ids_marks
	= statemapWElementHandles` (isEmpty o fst) getmarks ids_marks itemHs
where
	getmarks :: !WItemHandle` !(![Id],![(Id,Bool,Maybe [Index])]) -> (![Id],![(Id,Bool,Maybe [Index])])
	getmarks itemH=:{wItemId`,wItemKind`,wItemInfo`,wItems`} ids_marks=:(ids,marks)
		| isNothing wItemId`			= getcheckcontrolsmarks wItems` ids_marks
		| not hadId						= getcheckcontrolsmarks wItems` ids_marks
		| wItemKind`<>IsCheckControl	= getcheckcontrolsmarks wItems` (ids1,marks )
		| otherwise						= getcheckcontrolsmarks wItems` (ids1,marks1)
										with
											info		= getWItemCheckInfo` wItemInfo`
											indices		= getMarkIndices 1 info.checkItems`
											mark		= (itemId,True,Just indices)
											(_,marks1)	= creplace (eqfst3id itemId) mark marks
											
											getMarkIndices :: !Index ![CheckItemInfo`] -> [Index]
											getMarkIndices index [{checkItem`=(_,_,mark)}:items]
												# indexs		= getMarkIndices (index+1) items
												| marked mark	= [index:indexs]
												| otherwise		= indexs
											getMarkIndices _ _
												= []
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getslidersdirections :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe Direction)]) -> (![Id],![(Id,Bool,Maybe Direction)])
getslidersdirections itemHs ids_sliders
	= statemapWElementHandles` (isEmpty o fst) getdirections ids_sliders itemHs
where
	getdirections :: !WItemHandle` !(![Id],![(Id,Bool,Maybe Direction)]) -> (![Id],![(Id,Bool,Maybe Direction)])
	getdirections itemH=:{wItemId`,wItemKind`,wItemInfo`,wItems`} ids_sliders=:(ids,sliders)
		| isNothing wItemId`			= getslidersdirections wItems` ids_sliders
		| not hadId						= getslidersdirections wItems` ids_sliders
		| wItemKind`<>IsSliderControl	= getslidersdirections wItems` (ids1,sliders )
		| otherwise						= getslidersdirections wItems` (ids1,sliders1)
										with
											info		= getWItemSliderInfo` wItemInfo`
											slider		= (itemId,True,Just info.sliderInfoDir`)
											(_,sliders1)= creplace (eqfst3id itemId) slider sliders
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getslidersstates :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe SliderState)]) -> (![Id],![(Id,Bool,Maybe SliderState)])
getslidersstates itemHs ids_states
	= statemapWElementHandles` (isEmpty o fst) getstates ids_states itemHs
where
	getstates :: !WItemHandle` !(![Id],![(Id,Bool,Maybe SliderState)]) -> (![Id],![(Id,Bool,Maybe SliderState)])
	getstates itemH=:{wItemId`,wItemKind`,wItemInfo`,wItems`} ids_states=:(ids,states)
		| isNothing wItemId`			= getslidersstates wItems` ids_states
		| not hadId						= getslidersstates wItems` ids_states
		| wItemKind`<>IsSliderControl	= getslidersstates wItems` (ids1,states )
		| otherwise						= getslidersstates wItems` (ids1,states1)
										with
											info		= getWItemSliderInfo` wItemInfo`
											state		= (itemId,True,Just info.sliderInfoState`)
											(_,states1)	= creplace (eqfst3id itemId) state states
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getcontrolsframes :: !OSWindowMetrics ![WElementHandle`] !(![Id],![(Id,Bool,Maybe ViewFrame)]) -> (![Id],![(Id,Bool,Maybe ViewFrame)])
getcontrolsframes wMetrics itemHs ids_frames
	= statemapWElementHandles` (isEmpty o fst) (getframes wMetrics) ids_frames itemHs
where
	getframes :: !OSWindowMetrics !WItemHandle` !(![Id],![(Id,Bool,Maybe ViewFrame)]) -> (![Id],![(Id,Bool,Maybe ViewFrame)])
	getframes wMetrics itemH=:{wItemId`,wItemKind`,wItemSize`,wItemInfo`,wItems`} ids_frames=:(ids,frames)
		| isNothing wItemId`			= getcontrolsframes wMetrics wItems` ids_frames
		| not hadId						= getcontrolsframes wMetrics wItems` ids_frames
		| wItemKind`<>IsCompoundControl	= getcontrolsframes wMetrics wItems` (ids1,frames )
		| otherwise						= getcontrolsframes wMetrics wItems` (ids1,frames1)
										with
											info		= getWItemCompoundInfo` wItemInfo`
											(origin,domainRect,hasScrolls)
														= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
											visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize`) hasScrolls
											itemRect	= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect origin wItemSize`)
											frame		= (itemId,True,Just (rectToRectangle itemRect))
											(_,frames1)	= creplace (eqfst3id itemId) frame frames
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getcontrolsdomains :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe ViewDomain)]) -> (![Id],![(Id,Bool,Maybe ViewDomain)])
getcontrolsdomains itemHs ids_domains
	= statemapWElementHandles` (isEmpty o fst) getdomains ids_domains itemHs
where
	getdomains :: !WItemHandle` !(![Id],![(Id,Bool,Maybe ViewDomain)]) -> (![Id],![(Id,Bool,Maybe ViewDomain)])
	getdomains itemH=:{wItemId`,wItemKind`,wItemInfo`,wItems`} ids_domains=:(ids,domains)
		| isNothing wItemId`			= getcontrolsdomains wItems` ids_domains
		| not hadId						= getcontrolsdomains wItems` ids_domains
		| wItemKind`<>IsCompoundControl	= getcontrolsdomains wItems` (ids1,domains )
		| otherwise						= getcontrolsdomains wItems` (ids1,domains1)
										with
											info		= getWItemCompoundInfo` wItemInfo`
											domain		= (itemId,True,Just (rectToRectangle info.compoundDomain))
											(_,domains1)= creplace (eqfst3id itemId) domain domains
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids

getscrollfunctions :: ![WElementHandle`] !(![Id],![(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
									   -> (![Id],![(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
getscrollfunctions itemHs ids_funcs
	= statemapWElementHandles` (isEmpty o fst) getfuncs ids_funcs itemHs
where
	getfuncs :: !WItemHandle` !(![Id],![(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
							-> (![Id],![(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
	getfuncs itemH=:{wItemId`,wItemKind`,wItemInfo`,wItems`} ids_funcs=:(ids,funcs)
		| isNothing wItemId`			= getscrollfunctions wItems` ids_funcs
		| not hadId						= getscrollfunctions wItems` ids_funcs
		| wItemKind`<>IsCompoundControl	= getscrollfunctions wItems` (ids1,funcs )
		| otherwise						= getscrollfunctions wItems` (ids1,funcs1)
										with
											info		= getWItemCompoundInfo` wItemInfo`
											hScroll		= info.compoundHScroll
											vScroll		= info.compoundVScroll
											func		= (itemId,True,Just ((Horizontal,mapMaybe getscrollfunction hScroll)
																			,(Vertical,  mapMaybe getscrollfunction vScroll)
																			))
											(_,funcs1)	= creplace (eqfst3id itemId) func funcs
	where
		itemId			= fromJust wItemId`
		(hadId,ids1)	= removeCheck itemId ids
		
		getscrollfunction :: !ScrollInfo -> ScrollFunction
		getscrollfunction {scrollFunction} = scrollFunction

getcontrolsspaces :: (Int,Int) ![WElementHandle`] !(![Id],![(Id,Bool,Maybe (Int,Int))]) -> (![Id],![(Id,Bool,Maybe (Int,Int))])
getcontrolsspaces initspaces itemHs ids_spaces
	= statemapWElementHandles` (isEmpty o fst) (getspaces initspaces) ids_spaces itemHs
where
	getspaces :: (Int,Int) !WItemHandle` !(![Id],![(Id,Bool,Maybe (Int,Int))]) -> (![Id],![(Id,Bool,Maybe (Int,Int))])
	getspaces curspaces itemH=:{wItemId`,wItemKind`,wItemAtts`} ids_spaces=:(ids,spaces)
		| wItemKind`<>IsCompoundControl && wItemKind`<>IsLayoutControl
								= (ids,spaces)
		| isNothing wItemId`	= getcontrolsspaces newspaces itemH.wItems` ids_spaces
		| not hadId				= getcontrolsspaces newspaces itemH.wItems` (ids1,spaces )
		| otherwise				= getcontrolsspaces newspaces itemH.wItems` (ids1,spaces1)
								with
									space		= (itemId,True,Just newspaces)
									(_,spaces1)	= creplace (eqfst3id itemId) space spaces
	where
		itemId					= fromJust wItemId`
		(hadId,ids1)			= removeCheck itemId ids
		newspaces				= getcontrolitemspace` (snd (cselect iscontrolitemspace` (ControlItemSpace` (fst curspaces) (snd curspaces)) wItemAtts`))

getcontrolsmargins :: ((Int,Int),(Int,Int)) ![WElementHandle`] !(![Id],![(Id,Bool,Maybe ((Int,Int),(Int,Int)))])
															 -> (![Id],![(Id,Bool,Maybe ((Int,Int),(Int,Int)))])
getcontrolsmargins initmargins itemHs ids_margins
	= statemapWElementHandles` (isEmpty o fst) (getmargins initmargins) ids_margins itemHs
where
	getmargins :: ((Int,Int),(Int,Int)) !WItemHandle` !(![Id],![(Id,Bool,Maybe ((Int,Int),(Int,Int)))])
													-> (![Id],![(Id,Bool,Maybe ((Int,Int),(Int,Int)))])
	getmargins curmargins itemH=:{wItemId`,wItemKind`,wItemAtts`} ids_margins=:(ids,margins)
		| wItemKind`<>IsCompoundControl || wItemKind`<>IsLayoutControl
									= (ids,margins)
		| isNothing wItemId`		= getcontrolsmargins newmargins itemH.wItems` ids_margins
		| not hadId					= getcontrolsmargins newmargins itemH.wItems` (ids1,margins )
		| otherwise					= getcontrolsmargins newmargins itemH.wItems` (ids1,margins1)
									with
										margin		= (itemId,True,Just newmargins)
										(_,margins1)= creplace (eqfst3id itemId) margin margins
	where
		itemId						= fromJust wItemId`
		(hadId,ids1)				= removeCheck itemId ids
		((left,right),(top,bottom))	= curmargins
		newHMargins					= getcontrolhmargin` (snd (cselect iscontrolhmargin` (ControlHMargin` left right) wItemAtts`))
		newVMargins					= getcontrolvmargin` (snd (cselect iscontrolvmargin` (ControlVMargin` top bottom) wItemAtts`))
		newmargins					= (newHMargins,newVMargins)
