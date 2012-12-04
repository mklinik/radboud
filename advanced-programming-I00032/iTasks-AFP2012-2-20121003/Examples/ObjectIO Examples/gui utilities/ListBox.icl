implementation module ListBox


/*	Definition of the list box control.
	This definition has been written in Clean 2.0 and uses the Object I/O library, version 1.2.2
	The list box control is constructed out of predefined control elements, and is therefore platform independent.
	In future versions it will be added as a standard library component.
*/


import StdBool, StdEnum, StdFunc, StdList, StdMisc, StdOrdList, StdTuple
import StdControl, StdControlReceiver, StdId, StdPicture, StdPSt, StdReceiver, StdWindow


::	ListBoxControl ls ps
	=	{	listboxState	:: ListBoxState
		,	listboxAtts		:: [ControlAttribute *(ls,ps)]
		}
::	ListBoxState
	=	{	maxNrItems		:: Int							// The maximum number of items (in future version superfluous)
		,	items			:: [String]						// All items to be displayed
		,	selection		:: [Index]						// The current selection
		,	size			:: Size							// The size of the custom control that displays all items
		,	listboxId		:: ListBoxId					// The ids related to this list box
		,	fontInfo		:: ListBoxFontInfo				// The font information used to display the control
		}
::	ListBoxId
	=	{	outerCompoundId	:: !Id							// The Id of the outmost CompoundControl
		,	innerCompoundId	:: !Id							// The Id of the innermost CompoundControl
		,	customId		:: !Id							// The Id of the CustomControl that renders the items
		,	r2Id			:: !R2Id MessageIn MessageOut	// The Id of the Receiver2Control that handles message passing
		}
::	MessageIn
	=	InGetSelection										// Request to retrieve current selection
	|	InSetSelection		[Index]							// Request to set the selection to the given index
	|	InGetItems											// Request to retrieve all current items
	|	InOpenItems			Index [String]					// Request to add items behind the element with the given index
	|	InCloseItems		[Index]							// Request to remove items at the given index positions
::	MessageOut
	=	OutGetSelection		[(String,Index)]				// Reply to retrieve the current selection
	|	OutSetSelection										// Reply to set the selection
	|	OutGetItems			[String]						// Reply to get all items
	|	OutOpenItems										// Reply to add items
	|	OutCloseItems										// Reply to remove items
::	ListBoxFontInfo
	=	{	font			:: Font							// The font to draw the items of a listbox
		,	metrics			:: FontMetrics					// The metrics of that font
		}

ListBoxControl :: Int [String] [Index] ListBoxId [ControlAttribute *(.ls,.ps)] !*env
				-> (!.ListBoxControl .ls .ps,!*env) | accScreenPicture env
ListBoxControl maxNrItems items selection listboxid atts env
	# (dialogFont,env)	= accScreenPicture openDialogFont env
	# (metrics,   env)	= accScreenPicture (getFontMetrics dialogFont) env
	# listboxfontinfo	= {	font		= dialogFont
						  ,	metrics		= metrics
						  }
	# maxNrItems		= max 1 maxNrItems
	# items				= items%(0,maxNrItems-1)
	# (itemWidths,env)	= accScreenPicture (getFontStringWidths dialogFont items) env
	# itemsSize			= {	w=if (isEmpty items) 100 (maxList itemWidths)
//						  ,	h=(length items)*(fontLineHeight metrics)	// the initial size
						  ,	h=maxNrItems*(fontLineHeight metrics)		// fix the maximum size of the control
						  }
	# nrItems			= length items
	# selection			= if (nrItems==0)
							 []
							 (removeDup (filter (isBetween 1 nrItems) selection))
	= (	{	listboxState= {	maxNrItems	= maxNrItems	// In future version with resizeable controls superfluous
						  ,	items		= items
						  ,	selection	= selection
						  ,	size		= itemsSize
						  ,	listboxId	= listboxid
						  ,	fontInfo	= listboxfontinfo
						  }
		,	listboxAtts	= filter isListBoxControlAttribute atts
		}
	  , env
	  )
where
	isListBoxControlAttribute :: !(ControlAttribute .ps) -> Bool
	isListBoxControlAttribute (ControlFunction _)		= True
	isListBoxControlAttribute ControlHide				= True
	isListBoxControlAttribute (ControlPos _)			= True
	isListBoxControlAttribute (ControlSelectState _)	= True
	isListBoxControlAttribute (ControlViewSize _)		= True
	isListBoxControlAttribute _							= False

instance Controls ListBoxControl where
	controlToHandles {listboxState=listboxState=:{items,size,listboxId,fontInfo},listboxAtts} pst
		= controlToHandles imp pst
	where
		imp = {	addLS	= newListBoxState listboxState
			  ,	addDef	= CompoundControl
			  				(	CompoundControl 
							(	CustomControl size (customlook listboxState)	[	ControlId		listboxId.customId
																				,	ControlMouse	mouseFilter Able (mouse customAtt)
																				,	ControlPos		(Fix,zero)
																				]
							)
							[	ControlId			listboxId.innerCompoundId
							,	ControlItemSpace	0 0							// No itemspaces
							,	ControlHMargin		0 0							// No horizontal margins
							,	ControlVMargin		0 0							// No vertical margins
							,	ControlHScroll		hscroll
							,	ControlVScroll		(vscroll (fontLineHeight fontInfo.metrics))
							,	ControlViewDomain	{corner1=zero,corner2={x=size.w,y=size.h}}
							:	innerCompoundAtts
							]
							)
							[	ControlLook			True (\_ {newFrame}->draw newFrame)
							,	ControlHMargin		1 1
							,	ControlVMargin		1 1
							,	ControlId			listboxId.outerCompoundId
							:	outerCompoundAtts
							]
						  :+:	Receiver2 listboxId.r2Id receiver []
			  }
		
		//	The look of the custom control lists all items and the current selection
		customlook :: ListBoxState SelectState UpdateState *Picture -> *Picture
		customlook {items,selection,fontInfo} _ {newFrame} picture
			# picture		= clearlines picture
			# picture		= setfont    picture
			# picture		= drawlines  picture
			# picture		= drawselection picture
			= picture
		where
			metrics			= fontInfo.metrics
			height			= fontLineHeight metrics
			lines			= items
			(x1,x2)			= (newFrame.corner1.x,newFrame.corner2.x)
			
			clearlines		= unfill newFrame
			
			setfont			= setPenFont fontInfo.font
			
			drawlines		= seq (fst (smap (\line y->(drawAt {x=0,y=y} line,y+height)) lines (metrics.fAscent+metrics.fLeading)))
			
			drawselection	= seq (map (\i->hilite {corner1={x=x1,y=(i-1)*height},corner2={x=x2,y=i*height-1}}) selection)
		
		
		//	The only attribute for the CustomControl is the ControlFunction.
		customAtt
			# (hasFunAtt,funAtt)	= select (\att->case att of (ControlFunction f) -> True; _ -> False) undef listboxAtts
			| hasFunAtt
				= case funAtt of (ControlFunction f) -> f; _ -> id
			| otherwise
				= id
			
		//	The only optional attribute for the inner CompoundControl is the size.
		innerCompoundAtts
			# (hasSizeAtt,sizeAtt)	= select (\att->case att of (ControlViewSize _) -> True; _ -> False) undef listboxAtts
			| hasSizeAtt
				= [case sizeAtt of (ControlViewSize s) -> ControlViewSize s; _ -> undef]
			| otherwise
				= []
		
		//	The optional attributes for the outer CompoundControl are ControlSelectState, ControlPos, and ControlHide.
		outerCompoundAtts
			# (hasIt,att)	= select (\att->case att of (ControlSelectState _) -> True; _ -> False) undef listboxAtts
			# selectState	= if hasIt [case att of (ControlSelectState s) -> ControlSelectState s; _ -> undef] []
			# (hasIt,att)	= select (\att->case att of (ControlPos _) -> True; _ -> False) undef listboxAtts
			# pos			= if hasIt [case att of (ControlPos s) -> ControlPos s; _ -> undef] []
			# hasIt			= any (\att->case att of ControlHide -> True; _ -> False) listboxAtts
			# hide			= if hasIt [ControlHide] []
			= flatten [selectState,pos,hide]
		
		//	Scrolling through the compound control horizontally.
		hscroll :: ViewFrame SliderState SliderMove -> Int
		hscroll {corner1,corner2} {sliderThumb} action
			= case action of
				SliderIncSmall	-> sliderThumb+10
				SliderDecSmall	-> sliderThumb-10
				SliderIncLarge	-> sliderThumb+width
				SliderDecLarge	-> sliderThumb-width
				SliderThumb x	-> x
		where
			width	= abs (corner2.x-corner1.x-10)
		
		//	Scrolling through the compound control vertically.
		vscroll :: Int ViewFrame SliderState SliderMove -> Int
		vscroll lineHeight {corner1,corner2} {sliderThumb} action
			= case action of
				SliderIncSmall	-> sliderThumb+lineHeight
				SliderDecSmall	-> sliderThumb-lineHeight
				SliderIncLarge	-> sliderThumb+height
				SliderDecLarge	-> sliderThumb-height
				SliderThumb x	-> x/lineHeight*lineHeight
		where
			height	= abs (corner2.y-corner1.y-lineHeight)
		
		
		//	The mouse responds only to MouseDowns:
		mouseFilter :: MouseState -> Bool
		mouseFilter (MouseDown _ _ ddown)	= ddown==1
		mouseFilter _						= False
		
		//	The mouse either sets, adds, or removes items to the selection:
		mouse :: (IdFun *(.x,PSt .l)) MouseState *(*(.ListBoxState,.x),PSt .l) -> *(*(*ListBoxState,.x),PSt .l)
		mouse f (MouseDown pos {shiftDown} _) ((listboxState,ls),pState)
			# listboxState	= {listboxState & selection=okSelection}
			# newLook		= customlook listboxState
			# pState		= appPIO (setControlLooks [(customId,True,(True,newLook))]) pState
			# (ls,pState)	= f (ls,pState)
			= ((newListBoxState listboxState,ls),pState)
		where
			items			= listboxState.items
			nrItems			= length items
			selection		= listboxState.selection
			metrics			= listboxState.fontInfo.metrics
			lineHeight		= fontLineHeight metrics
			newIndex		= pos.y/lineHeight+1
			newSelection	= if (not shiftDown)				[newIndex]
							 (if (isMember newIndex selection)	(removeMembers selection [newIndex])
																(merge [newIndex] selection))
			okSelection		= filter (isBetween 1 nrItems) newSelection
			customId		= listboxState.listboxId.customId
		
		
		//	The receiver function:
		receiver :: MessageIn *(*(*ListBoxState,.x),PSt .l) -> (MessageOut,*(*(*ListBoxState,.x),PSt .l))
		
		//	Return the current selection:
		receiver InGetSelection ((listboxState=:{items,selection},ls),pState)
			= (OutGetSelection (map (\index->(items!!(index-1),index)) selection),((listboxState,ls),pState))
		
		//	Set a new selection:
		receiver (InSetSelection newSelection) ((listboxState,ls),pState)
			# listboxState	= {listboxState & selection=newSelection}
			# newLook		= customlook listboxState
			# pState		= appPIO (setControlLooks [(customId,True,(True,newLook))]) pState
			= (OutSetSelection,((newListBoxState listboxState,ls),pState))
		where
			customId		= listboxState.listboxId.customId
		
		//	Return the current elements:
		receiver InGetItems ((listboxState=:{items},ls),pState)
			= (OutGetItems items,((listboxState,ls),pState))
		
		//	Insert elements:
		receiver (InOpenItems behindIndex newItems) ((listboxState=:{maxNrItems,items,selection},ls),pState)
			| nrNewItems==0
				= (OutOpenItems,((listboxState,ls),pState))
			# listboxState	= {listboxState & items=allItems, selection=newSelection}
			# newLook		= customlook listboxState
			# pState		= appPIO (setControlLooks [(customId,True,(True,newLook))]) pState
			| otherwise
				= (OutOpenItems,((newListBoxState listboxState,ls),pState))
		where
			customId				= listboxState.listboxId.customId
			nrCurItems				= length items
//			nrNewItems				= length newItems									// Add any number of new items
			nrNewItems				= min (maxNrItems-nrCurItems) (length newItems)		// Add only items upto maxNrItems
			okNewItems				= newItems%(0,nrNewItems-1)							// These are the proper new items
			okBehindIndex			= setBetween 0 (length items) behindIndex
			(itemsBefore,itemsAfter)= splitAt (okBehindIndex-1) items
			allItems				= if (okBehindIndex==0)
										 (okNewItems++items)
										 (itemsBefore++okNewItems++itemsAfter)
			(selecBefore,selecAfter)= span (\index->index<=okBehindIndex) (sort selection)
			newSelection			= selecBefore++map ((+) nrNewItems) selecAfter
		
		//	Remove elements:
		receiver (InCloseItems closeItems) ((listboxState=:{items,selection},ls),pState)
			| nrCloseItems==0
				= (OutCloseItems,((listboxState,ls),pState))
			# listboxState	= {listboxState & items=allItems, selection=newSelection}
			# newLook		= customlook listboxState
			# pState		= appPIO (setControlLooks [(customId,True,(True,newLook))]) pState
			| otherwise
				= (OutCloseItems,((newListBoxState listboxState,ls),pState))
		where
			customId				= listboxState.listboxId.customId
			nrCloseItems			= length closeItems
			allItems				= [ item \\ item <- items & i <- [1..] | not (isMember i closeItems) ]
			newSelection			= removeMembers selection closeItems
	
	getControlType _ = "ListBoxControl"

openListBoxId :: !*env -> (!ListBoxId,!*env)	| Ids env
openListBoxId env
	# (id1, env)	= openId env
	# (id2, env)	= openId env
	# (id3, env)	= openId env
	# (r2id,env)	= openR2Id env
	= ({outerCompoundId=id1,innerCompoundId=id2,customId=id3,r2Id=r2id},env)


//	The functions below take care of the proper communication with the receiver that
//	belongs to the listbox control.
getListBoxSelection :: !ListBoxId !(PSt *l) -> (!(!Bool,![(String,!Index)]),!PSt *l)
getListBoxSelection {r2Id} pState
	# ((_,maybe_out),pState)	= syncSend2 r2Id InGetSelection pState
	| isNothing maybe_out
		= ((False,[]),pState)
	# result					= case (fromJust maybe_out) of
									(OutGetSelection selection)	-> (True,selection)
									_							-> (False,[])
	| otherwise
		= (result,pState)

setListBoxSelection :: !ListBoxId ![Index] !(PSt *l) -> PSt *l
setListBoxSelection {r2Id} selection pState
	= snd (syncSend2 r2Id (InSetSelection selection) pState)

getListBoxItems :: !ListBoxId !(PSt *l) -> (!(!Bool,![String]),!PSt *l)
getListBoxItems {r2Id} pState
	# ((_,maybe_out),pState)	= syncSend2 r2Id InGetItems pState
	| isNothing maybe_out
		= ((False,[]),pState)
	# result					= case (fromJust maybe_out) of
									(OutGetItems items)	-> (True,items)
									_					-> (False,[])
	| otherwise
		= (result,pState)

openListBoxItems :: !ListBoxId !Index ![String] !(PSt *l) -> PSt *l
openListBoxItems {r2Id} index items pState
	= snd (syncSend2 r2Id (InOpenItems index items) pState)

closeListBoxItems :: !ListBoxId ![Index] !(PSt *l) -> PSt *l
closeListBoxItems {r2Id} items pState
	= snd (syncSend2 r2Id (InCloseItems items) pState)

showListBoxControl :: !ListBoxId !(IOSt .l) -> IOSt .l
showListBoxControl {outerCompoundId} ioState = showControls [outerCompoundId] ioState

hideListBoxControl :: !ListBoxId !(IOSt .l) -> IOSt .l
hideListBoxControl {outerCompoundId} ioState = hideControls [outerCompoundId] ioState

enableListBoxControl :: !ListBoxId !(IOSt .l) -> IOSt .l
enableListBoxControl {outerCompoundId} ioState = enableControls [outerCompoundId] ioState

disableListBoxControl :: !ListBoxId !(IOSt .l) -> IOSt .l
disableListBoxControl {outerCompoundId} ioState = disableControls [outerCompoundId] ioState


//	Auxiliary functions:

smap :: (x -> s -> (y, s)) [x] s -> ([y],s)
smap f [x:xs] s
	# (y,s)	= f x s
	# (ys,s)= smap f xs s
	= ([y:ys],s)
smap _ _ s
	= ([],s)

select :: (x -> Bool) x [x] -> (Bool,x)
select pred dummy [x:xs]
	| pred x
		= (True,x)
	| otherwise
		= select pred dummy xs
select _ dummy _
	= (False,dummy)

isBetween :: x x x -> Bool	| Ord x
isBetween low up x
	= low<=x && x<=up

setBetween :: x x x -> x | Ord x
setBetween low up x
	| x<low		= low
	| x<up		= x
	| otherwise	= up

newListBoxState :: !ListBoxState -> *ListBoxState
newListBoxState {maxNrItems,items,selection,size,listboxId,fontInfo}
	= {maxNrItems=maxNrItems,items=items,selection=selection,size=size,listboxId=listboxId,fontInfo=fontInfo}
