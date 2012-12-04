definition module StdControl


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdControl specifies all control operations.
//	********************************************************************************


import	StdControlClass


controlSize				:: !.(cdef .ls (PSt .l)) !Bool
						   !(Maybe (Int,Int)) !(Maybe (Int,Int)) !(Maybe (Int,Int))
						   !(PSt .l)
				  -> (!Size,!PSt .l) | Controls cdef
/*	controlSize calculates the size of the given control definition as it would be 
		opened as an element of a window/dialog.
	The Boolean argument determines whether a window (True) or a dialog (False) is
		intended.
	The Maybe arguments are the prefered horizontal margins, vertical margins, and 
		item spaces (see also the (Window/Control)(H/V)Margin and 
		(Window/Control)ItemSpace attributes). If Nothing is specified, their 
		default values with respect to the window/dialog are used.
*/


/*	Functions that change the set of controls in windows/dialogues.
*/

openControls			:: !Id .ls .(cdef .ls (PSt .l)) !(PSt .l)
									    -> (!ErrorReport,!PSt .l)
									    |  Controls cdef
openCompoundControls	:: !Id .ls .(cdef .ls (PSt .l)) !(PSt .l)
									    -> (!ErrorReport,!PSt .l)
									    |  Controls cdef
openRecursiveControls	:: !Id .ls .(cdef .ls (PSt .l)) !(PSt .l)
									    -> (!ErrorReport,!PSt .l)
									    |  Controls cdef
openPopUpControlItems	:: !Id !Index ![PopUpControlItem (PSt .l)]
									    !(IOSt .l) -> IOSt .l
/*	openControls
		adds the given controls argument to the indicated window or dialog. 
	openCompoundControls
		adds the given controls argument to the indicated compound control.
		THIS FUNCTION IS DEPRECATED. USE openRecursiveControls INSTEAD WITH
		EXACTLY THE SAME ARGUMENTS.
	openRecursiveControls
		adds the given controls argument to the indicated (Compound/Layout)Control.
	openPopUpControlItems
		adds the PopUpControlItems to the indicated PopUpControl behind the item at
		the given index position (counted from 1). 
	The window/dialog is not resized.
	These functions have no effect in case the indicated window, dialog, compound,
	or layout control could not be found (ErrorUnknownObject) or if controls are 
	opened with duplicate Ids (ErrorIdsInUse).
*/

closeControls			:: !Id [Id] !Bool	!(IOSt .l) -> IOSt .l
/*	closeControls removes the indicated controls (second argument) from the 
	indicated window (first argument) and recalculates the layout iff the Boolean 
	argument is True.
*/

closeAllControls		:: !Id !(IOSt .l) -> IOSt .l
/*	closeAllControls removes all controls from the indicated window.
*/

closePopUpControlItems	:: !Id ![Index] !(IOSt .l) -> IOSt .l
/*	closePopUpControlItems closes PopUpControlItems by their Index position of the 
	indicated PopUpControl. 
	If the currently checked element of a PopUpControl is closed, the first 
	remaining element of that PopUpControl will be checked. 
*/


setControlPos			:: !Id ![(Id,ItemPos)]	!(IOSt .l) -> (!Bool,!IOSt .l)
/*	setControlPos changes the current layout position of the indicated controls to 
	their new positions. 
	If there are relatively laynout controls, then their layout also changes. The 
	window is not resized.
	The Boolean result is False iff the window is unknown. 
*/


/*	Functions that change the state of controls.
	Only those Id arguments that refer to controls within the same interactive 
	process are used to change the corresponding controls.
*/

showControls			:: ![Id]						!(IOSt .l) -> IOSt .l
showControl				:: ! Id							!(IOSt .l) -> IOSt .l
hideControls			:: ![Id]						!(IOSt .l) -> IOSt .l
hideControl				:: ! Id							!(IOSt .l) -> IOSt .l
/*	(show/hide)Control(s) makes the indicated control(s) visible/invisible. 
	Hiding a control overrides the visibility of its elements, which become 
		invisible. 
	Showing a hidden control re-establishes the visibility state of its elements.
*/

enableControls			:: ![Id]						!(IOSt .l) -> IOSt .l
enableControl			:: ! Id							!(IOSt .l) -> IOSt .l
disableControls			:: ![Id]						!(IOSt .l) -> IOSt .l
disableControl			:: ! Id							!(IOSt .l) -> IOSt .l
/*	(en/dis)ableControl(s) (en/dis)ables the indicated control(s).
	Disabling a control overrides the SelectStates of its elements, which become 
		unselectable.
	Enabling a disabled control re-establishes the SelectStates of its elements.
*/

markCheckControlItems	:: !Id ![Index]					!(IOSt .l) -> IOSt .l
unmarkCheckControlItems	:: !Id ![Index]					!(IOSt .l) -> IOSt .l
/*	(unm/m)arkCheckControlItems unmarks/marks the indicated check items of the given
	CheckControl. Indices range from 1 to the number of check items. Illegal indices
	are ignored.
*/

selectRadioControlItem	:: !Id  !Index					!(IOSt .l) -> IOSt .l
/*	selectRadioControlItem marks the indicated radio item of a RadioControl, causing
	the mark of the previously marked radio item to disappear. The item is given by 
	the Id of the RadioControl and its index position (counted from 1). 
*/

selectPopUpControlItem	:: !Id  !Index					!(IOSt .l) -> IOSt .l
/*	selectPopUpControlItem marks the indicated popup item of a PopUpControl, causing
	the mark of the previously marked popup item to disappear. The item is given by 
	the Id of the PopUpControl and its index position (counted from 1).
*/

moveControlViewFrame	:: !Id Vector2					!(IOSt .l) -> IOSt .l
/*	moveControlViewFrame moves the orientation of the CompoundControl over the given
	vector, and updates the control if necessary. The control frame is not moved 
	outside the ViewDomain of the control. MoveControlViewFrame has no effect if the
	indicated control has no ControlDomain attribute.
*/

setControlViewDomain	:: !Id ViewDomain				!(IOSt .l) -> IOSt .l
/*	setControlViewDomain sets the view domain of the indicated CompoundControl as 
	given. The control view frame is moved such that a maximum portion of the view 
	domain is visible. The control is not resized.
	In case of unknown Ids, or non CompoundControls, setControlViewDomain has no 
	effect.
*/

setControlScrollFunction:: !Id Direction ScrollFunction !(IOSt .l) -> IOSt .l
/*	setControlScrollFunction sets the ScrollFunction of the indicated CompoundControl
	in the given Direction if it has one.
	In all other cases, setControlScrollFunction has no effect.
*/

setControlOuterSize		:: !Id Size			Bool		!(IOSt .l) -> IOSt .l
setControlWidth			:: !Id ControlWidth	Bool		!(IOSt .l) -> IOSt .l
/*	setControlOuterSize sets the outer size of the indicated 
	(Compound/Custom(Button)/Layout)Control.
	setControlWidth sets the width of the indicated (Button/Edit/PopUp/Text)Control.
	For both functions the Boolean argument indicates if the layout needs to be
	recalculated after setting the size (True) or not (False).
	In case of unknown Ids, or non-matching control types, these functions have no
	effect.
*/

setControlTexts			:: ![(Id,String)]				!(IOSt .l) -> IOSt .l
setControlText			:: !Id !String					!(IOSt .l) -> IOSt .l
/*	setControlText(s) sets the text of the indicated (Text/Edit/Button)Control(s). 
	If the indicated control is a (Text/Button)Control, then AltKey are interpreted 
	by the system.
	If the indicated control is an EditControl, then the text is taken as it is.
*/

setEditControlCursor	:: !Id !Int						!(IOSt .l) -> IOSt .l
/*	setEditControlCursor sets the cursor at position @2 of the current content of 
	the EditControl.
	In case @2<0, then the cursor is set at the start of the current content.
	In case @2>size content, then the cursor is set at the end of the current 
	content.
*/

setEditControlSelection	:: !Id !Int !Int				!(IOSt .l) -> IOSt .l
/*	setEditControlSelection sets the new selection starting at position @2 until
	@3 in the current content of the indicated EditControl. 
	In case @2<=@3, then the selection starts at @2 and ends at @3, with @2
		taken to be atleast 0, and @3 taken to be at most (size content). 
	In case @2> @3, then the whole text is selected.
	The indicated control must be the active EditControl (use setActiveControl in
	StdWindow), otherwise this function has no effect.
*/

setControlLooks			:: ![(Id, Bool,(Bool,Look))]	!(IOSt .l) -> IOSt .l
setControlLook			::   !Id !Bool (Bool,Look)		!(IOSt .l) -> IOSt .l
/*	setControlLook(s) sets the (render,look) attribute of the indicated 
	(Custom(Button)/Compound)Control(s). If this concerns a transparant 
	CompoundControl then it becomes non-transparant.
	An indicated control is only redrawn if the first Boolean is True. 
*/

setSliderStates			:: ![(Id, IdFun SliderState)]	!(IOSt .l) -> IOSt .l
setSliderState			::   !Id (IdFun SliderState)	!(IOSt .l) -> IOSt .l
setSliderThumbs			:: ![(Id,Int)]					!(IOSt .l) -> IOSt .l
setSliderThumb			::   !Id Int					!(IOSt .l) -> IOSt .l
/*	setSliderState(s)
		applies the function to the current SliderState of the indicated 
		SliderControl(s) and redraws the settings if necessary.
	setSliderThumb(s)
		sets the new thumb value of the indicated SliderControl(s) and redraws the 
		settings if necessary.
*/

appControlPicture		:: !Id !.(IdFun *Picture)		!(IOSt .l) -> IOSt .l
accControlPicture		:: !Id !.(St *Picture .x)		!(IOSt .l)
										   -> (!Maybe .x,!IOSt .l)
/*	(app/acc)ControlPicture applies the given drawing function to the Picture of
	the indicated (Custom(Button)/Compound)Control. If the CompoundControl is 
	transparant, or the indicated control could not be found then this operation 
	has no effect. In that case, accControlPicture also returns Nothing.
*/


updateControl			:: !Id !(Maybe ViewFrame)		!(IOSt .l) -> IOSt .l
/*	updateControl applies the Look attribute function of the indicated 
	(Compound/Custom(Button))Control.
	The Look attribute function is applied to the following arguments:
	The current SelectState of the control, and
	the UpdateState argument
		{oldFrame=viewframe,newFrame=viewframe,updArea=[frame]}
	where viewframe is the current ViewFrame of the control;
	and frame depends on the optional ViewFrame argument:
		in case of (Just rectangle):
			the intersection of viewframe and rectangle.
		in case of Nothing:
			viewframe. 
	updateControl has no effect in case of unknown controls, or if the indicated 
	control is not a (Compound/Custom(Button))Control, or the optional viewframe 
	argument is an empty rectangle.
*/


/*	Access functions on WState. To read the state of a control, a WState is 
	required which can be obtained by the getWindow function. The WState value 
	represents the state of a window or dialogue at that particular moment.
*/

::	WState

getWindow				:: !Id !(IOSt .l) -> (!Maybe WState, !IOSt .l)
getParentWindow			:: !Id !(IOSt .l) -> (!Maybe WState, !IOSt .l)
/*	getWindow returns a read-only WState for the indicated window.
		In case the indicated window does not exist Nothing is returned.
	getParentWindow returns a read-only WState for the parent window/dialogue
		of the indicated control. In case the Id does not correspond with a
		control, Nothing is returned. 
*/

getControlTypes			::		!WState -> [(ControlType,Maybe Id)]
getCompoundTypes		:: !Id	!WState -> [(ControlType,Maybe Id)]
/*	getControlTypes
		yields the list of ControlTypes of the component controls of this window. 
	getCompoundTypes
		yields the list of ControlTypes of the component controls of this 
		CompoundControl. 
	For both functions (Just id) is yielded if the component control has a 
	(ControlId id) attribute, and Nothing otherwise. Component controls are not 
	collected recursively through CompoundControls.
	If the indicated CompoundControl is not a CompoundControl, then [] is yielded.
*/

/*	Functions that return the current state of controls. 
	For each access there is one singular and one plural version. In case of the
	plural version the result list is of equal length as the argument Id list. Each 
	result list element corresponds in order with the argument Id list. 
	In both versions the first Boolean result is False in case of invalid Ids (if so
	dummy values are returned - see comment).
	Important: controls with no ControlId attribute, or illegal ids, can not be 
	found in the WState!
*/


getControlLayouts		:: ![Id] !WState -> [(Bool,(Maybe ItemPos,Vector2))]
getControlLayout		:: ! Id  !WState ->  (Bool,(Maybe ItemPos,Vector2))
/*	getControlLayout(s) yields (Just ControlPos) if the indicated control had a 
	ControlPos attribute and Nothing otherwise. The Vector2 offset is the exact 
	current location of the indicated control (LeftTop,OffsetVector offset). 
*/

getControlViewSizes		:: ![Id] !WState -> [(Bool,Size)]
getControlViewSize		:: ! Id  !WState ->  (Bool,Size)
getControlOuterSizes	:: ![Id] !WState -> [(Bool,Size)]
getControlOuterSize		:: ! Id  !WState ->  (Bool,Size)
/*	getControlViewSize(s) yields the current ViewFrame size of the indicated 
		control. Note that this is the exact size of the control for any control 
		other than the CompoundControl. In case of unknown Ids zero is returned.
	getControlOuterSize(s) yields the current ControlOuterSize of the indicated
		control. Note that this is the exact size of the control. In case of unknown
		Ids zero is returned. 
*/

getControlSelectStates	:: ![Id] !WState -> [(Bool,SelectState)]
getControlSelectState	:: ! Id  !WState ->  (Bool,SelectState)
/*	getControlSelectState(s) yields the current SelectState of the indicated 
	control. In case of unknown Ids Able is returned.
*/

getControlShowStates	:: ![Id] !WState -> [(Bool,Bool)]
getControlShowState		:: ! Id  !WState ->  (Bool,Bool)
/*	getControlShowState(s) yields True if the indicated control is visible, and 
	False otherwise. The latter is also returned in case of unknown Ids.
*/

getControlTexts			:: ![Id] !WState -> [(Bool,Maybe String)]
getControlText			:: ! Id  !WState ->  (Bool,Maybe String)
/*	getControlText(s) yields (Just text) of the indicated (PopUp/Text/Edit/Button)
	Control. If the control is not such a control, then Nothing is yielded. 
*/

getControlNrLines		:: ![Id] !WState -> [(Bool,Maybe NrLines)]
getControlNrLine		:: ! Id  !WState ->  (Bool,Maybe NrLines)
/*	getControlNrLine(s) yields (Just nrlines) of the indicated EditControl. 
	If the control is not such a control, then Nothing is yielded.
*/

getControlLooks			:: ![Id] !WState -> [(Bool,Maybe (Bool,Look))]
getControlLook			:: ! Id  !WState ->  (Bool,Maybe (Bool,Look))
/*	getControlLook(s) yields the (render/look) of the indicated 
	(Custom/CustomButton/Compound)Control. If the control is not such a control, or
	is a transparant CompoundControl, then Nothing is yielded.
*/

getControlMinimumSizes	:: ![Id] !WState -> [(Bool,Maybe Size)]
getControlMinimumSize	:: ! Id  !WState ->  (Bool,Maybe Size)
/*	getControlMinimumSize(s) yields (Just minimumsize) if the indicated control had
	a ControlMinimumSize attribute and Nothing otherwise. 
*/

getControlResizes		:: ![Id] !WState -> [(Bool,Maybe ControlResizeFunction)]
getControlResize		:: ! Id  !WState ->  (Bool,Maybe ControlResizeFunction)
/*	getControlResize(s) yields (Just resizefunction) if the indicated control had a
	ControlResize attribute and Nothing otherwise.
*/

getRadioControlItems	:: ![Id] !WState -> [(Bool,Maybe [String])]
getRadioControlItem		:: ! Id  !WState ->  (Bool,Maybe [String])
/*	getRadioControlItem(s) yields the TextLines of the items of the indicated 
	RadioControl. If the control is not such a control, then Nothing is yielded.
*/

getRadioControlSelections:: ![Id] !WState -> [(Bool,Maybe Index)]
getRadioControlSelection :: ! Id  !WState ->  (Bool,Maybe Index)
/*	getRadioControlSelection(s) yields the index of the selected radio item of the 
	indicated RadioControl. If the control is not such a control, then Nothing is 
	yielded.
*/

getCheckControlItems	:: ![Id] !WState -> [(Bool,Maybe [String])]
getCheckControlItem		:: ! Id  !WState ->  (Bool,Maybe [String])
/*	getCheckControlItem(s) yields the TextLines of the items of the indicated 
	CheckControl. If the control is not such a control, then Nothing is yielded.
*/

getCheckControlSelections:: ![Id] !WState -> [(Bool,Maybe [Index])]
getCheckControlSelection :: ! Id  !WState ->  (Bool,Maybe [Index])
/*	getCheckControlSelection(s) yields the indices of the selected checkitems of the
	indicated CheckControl. If the control is not such a control, then Nothing is 
	yielded.
*/

getPopUpControlItems	:: ![Id] !WState -> [(Bool,Maybe [String])]
getPopUpControlItem		:: ! Id  !WState ->  (Bool,Maybe [String])
/*	getPopUpControlItem(s) yields the TextLines of the items of the indicated 
	PopUpControl. If the control is not such a control, then Nothing is yielded.
*/

getPopUpControlSelections:: ![Id] !WState -> [(Bool,Maybe Index)]
getPopUpControlSelection :: ! Id  !WState ->  (Bool,Maybe Index)
/*	getPopUpControlSelection(s) yields the Index of the indicated PopUpControl.
	If the control is not such a control, then Nothing is yielded.
*/

getSliderDirections		:: ![Id] !WState -> [(Bool,Maybe Direction)]
getSliderDirection		:: ! Id  !WState ->  (Bool,Maybe Direction)
/*	getSliderDirection(s) yields (Just Direction) of the indicated SliderControl. 
	If the control is not such a control, then Nothing is yielded.
*/

getSliderStates			:: ![Id] !WState -> [(Bool,Maybe SliderState)]
getSliderState			:: ! Id  !WState ->  (Bool,Maybe SliderState)
/*	getSliderState(s) yields (Just SliderState) of the indicated SliderControl. 
	If the control is not such a control, then Nothing is yielded. 
*/

getControlViewFrames	:: ![Id] !WState -> [(Bool,Maybe ViewFrame)]
getControlViewFrame		:: ! Id  !WState ->  (Bool,Maybe ViewFrame)
/*	getControlViewFrame(s) yields (Just ViewFrame) of the indicated CompoundControl.
	If the control is not such a control, then Nothing is yielded.
*/

getControlViewDomains	:: ![Id] !WState -> [(Bool,Maybe ViewDomain)]
getControlViewDomain	:: ! Id  !WState ->  (Bool,Maybe ViewDomain)
/*	getControlViewDomain(s) yields (Just ViewDomain) of the indicated 
	CompoundControl. If the control is not such a control, then Nothing is yielded.
*/

getControlScrollFunctions
						:: ![Id] !WState
						-> [(Bool,Maybe ((Direction,Maybe ScrollFunction)
										,(Direction,Maybe ScrollFunction)
										))]
getControlScrollFunction:: ! Id  !WState
						->  (Bool,Maybe ((Direction,Maybe ScrollFunction)
										,(Direction,Maybe ScrollFunction)
										))
/*	getControlScrollFunction(s) yields the ScrollFunctions of the indicated
	CompoundControl. If the control is not such a control, then Nothing is yielded.
*/

getControlItemSpaces	:: ![Id] !WState -> [(Bool,Maybe (Int,Int))]
getControlItemSpace		:: ! Id  !WState ->  (Bool,Maybe (Int,Int))
/*	getControlItemSpace(s) yields (Just (horizontal space,vertical space)) of the 
	indicated (Compound/Layout)Control. If the control is not such a control, then 
	Nothing is yielded. 
*/

getControlMargins		:: ![Id] !WState -> [(Bool,Maybe ((Int,Int),(Int,Int)))]
getControlMargin		:: ! Id  !WState ->  (Bool,Maybe ((Int,Int),(Int,Int)))
/*	getControlMargins yields (Just (ControlHMargin,ControlVMargin)) of the 
	indicated (Compound/Layout)Control. If the control is not such a control, then 
	Nothing is yielded.
*/
