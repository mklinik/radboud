definition module oswindow


//	Clean Object I/O library, version 1.2


import	StdMaybe, StdOverloaded, StdString
import	osdocumentinterface, osevent, ostypes
from	StdIOCommon			import :: CursorShape
from	osfont				import :: Font
from	osrgn				import :: OSRgnHandle
from	ossystem			import :: OSWindowMetrics
from	ostoolbox			import :: OSToolbox
from	ospicture			import :: OSPictContext


/*	Initialisation:
*/
osInitialiseWindows	:: !*OSToolbox -> *OSToolbox


/*	System dependent constants:
*/
OSControlTitleSpecialChars :== []					// Special prefix characters that should be removed


/*	System dependent metrics:
*/
osMinWindowSize					:: (!Int,!Int)
osMinCompoundSize				:: (!Int,!Int)

/*	Window frame dimensions:
*/
osWindowFrameWidth		:: Int
osWindowTitleBarHeight	:: Int


//	Calculating the view frame of window/compound with visibility of scrollbars.
osGetCompoundContentRect:: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetCompoundHScrollRect:: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetCompoundVScrollRect:: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect

osGetWindowContentRect	:: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetWindowHScrollRect	:: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetWindowVScrollRect	:: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect


/*	Determine the size of controls:
	osGetButtonControlSize windowmetrics title
		returns the size(height) of the ButtonControl that has the given title.
	osGetTextControlSize windowmetrics title
		returns the size of the TextControl   that has the given title.
	osGetEditControlSize windowmetrics width nr
		returns the size of the EditControl that has the given width and should show nr of lines.
	osGetPopUpControlSize windowmetrics items
		returns the size of the PopUpControl that thas the given list of items.
	osGet(Radio/Check)ControlItemSize windowmetrics title
		returns the size of the (Radio/Check)ControlItem that has the given title.
	osGet(Radio/Check)ControlItemHeight windowmetrics
		returns the height of an individual (Radio/Check)ControlItem.
	osGetSliderControlSize windowmetrics isHorizontal length
		returns the correct size of the SliderControl given its direction (True iff Horizontal) and length.
*/
osGetButtonControlSize		:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetTextControlSize		:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetEditControlSize		:: !OSWindowMetrics !Int !Int	!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetPopUpControlSize		:: !OSWindowMetrics ![String]	!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetRadioControlItemSize	:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetCheckControlItemSize	:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetSliderControlSize		:: !OSWindowMetrics !Bool !Int -> (!Int,!Int)

/*	Determine the height of controls.
*/
osGetButtonControlHeight	:: !OSWindowMetrics			-> Int
osGetTextControlHeight		:: !OSWindowMetrics			-> Int
osGetEditControlHeight		:: !OSWindowMetrics !Int	-> Int
osGetPopUpControlHeight		:: !OSWindowMetrics			-> Int
osGetRadioControlItemHeight	:: !OSWindowMetrics			-> Int
osGetCheckControlItemHeight	:: !OSWindowMetrics			-> Int

/*	Determine the minimum width of controls.
*/
osGetButtonControlMinWidth		:: !OSWindowMetrics -> Int
osGetTextControlMinWidth		:: !OSWindowMetrics -> Int
osGetEditControlMinWidth		:: !OSWindowMetrics -> Int
osGetPopUpControlMinWidth		:: !OSWindowMetrics -> Int
osGetRadioControlItemMinWidth	:: !OSWindowMetrics -> Int
osGetCheckControlItemMinWidth	:: !OSWindowMetrics -> Int
osGetSliderControlMinWidth		:: !OSWindowMetrics -> Int


/*	Window creation functions.
	osCreateDialog	isModal
					isClosable title pos size behindPtr
					getcontrolfocus createcontrols updatecontrols osdocinfo controlinfo
					creates a dialog with the given title, position and size. 
					The isModal		argument is True iff the dialog is modal.
					The behindPtr	argument is OSNoWindowPtr if the dialog must be created topmost;
									it is an OSWindowPtr if it must be placed behind a given window.
	osCreateWindow	isResizable hScrollInfo vScrollInfo minSize maxSize
					isClosable title pos size
					getcontrolfocus createcontrols updatecontrols osdocinfo behindPtr controlinfo
					creates a window with the given title, position and size. 
					The isResizable	argument is True iff the window is user resizable.
					The hScrollInfo	argument represents the horizontal scrollbar of the window.
					The vScrollInfo	argument represents the vertical   scrollbar of the window.
					The minSize		argument is the minimum size of the window.
					The maxSize		argument is the maximum size of the window.
					The return OSWindowPtrs (result 3,4) are the OSWindowPtrs of the scrollbars.
	The isClosable		argument is True iff the window/dialog is user closeable.
	The title			argument is the title of the window/dialog.
	The pos				argument is the position of the window/dialog.
	The size			argument is the size of the window/dialog, including scrollbars, excluding title bar and frame.
	The getcontrolfocus	argument function returns the handle to the control that 
						has the input focus.
	The createcontrols	argument function creates the controls of the window/dialog, 
						given the handle to the created window/dialog and the proper control information.
	The updatecontrols	argument function updates the customised controls of the window/dialog.
	The osdocinfo		argument gives the document interface of the parent process.
	The return [DelayActivationInfo] are the OSWindowPtrs of windows/dialogs that have become (in)active (in that order).
	The return OSWindowPtr is the OSWindowPtr of the created window/dialog.
	The return OSDInfo is the validated OSDInfo of the parent process.
*/

osCreateDialog :: !Bool
				  !Bool !String !(!Int,!Int) !(!Int,!Int) !OSWindowPtr
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr-> u:s -> u:(*OSToolbox -> *(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!u:s,!*OSToolbox)
osCreateWindow :: !OSWindowMetrics !Bool !ScrollbarInfo !ScrollbarInfo !(!Int,!Int) !(!Int,!Int)
				  !Bool !String !(!Int,!Int) !(!Int,!Int)
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr-> u:s -> u:(*OSToolbox -> *(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !OSWindowPtr !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!OSDInfo,!u:s,!*OSToolbox)

/*	osCreateModalDialog wMetrics isCloseable title osdocinfo currentModal size 
						dialogControls dialogInit handleOSEvents
						(getOSToolbox,setOSToolbox)
	creates a modal dialog and handles the events until either the dialog is closed or its parent process terminated.
	Events are handled according to handleOSEvents.
	Controls are created according to dialogControls                       (only if (not osModalDialogHandlesControlCreation)!).
	Before the event loop is entered, the dialogInit function is evaluated (only if (not osModalDialogHandlesWindowInit)!).
*/

::	OSModalEventHandling s
	=	OSModalEventCallback (s -> *(OSEvents,s)) (*(OSEvents,s) -> s) (OSEvent -> s -> *([Int],s))
	|	OSModalEventLoop     (s -> s)

osModalDialogHandlesMenuSelectState	:== True
osModalDialogHandlesWindowInit		:== True
osModalDialogHandlesControlCreation	:== True
osModalDialogHandlesEvents			:== True

osCreateModalDialog ::  !OSWindowMetrics !Bool !String !OSDInfo !(Maybe OSWindowPtr) !(!Int,!Int)
					    !(OSWindowPtr u:s -> u:s)
					    !(OSWindowPtr u:s -> u:s)
					    !(OSModalEventHandling u:s)
						!(!u:s -> *(*OSToolbox,u:s), !*OSToolbox -> *(u:s -> u:s))
					    !u:s
			  -> (!Bool,!u:s)


/*	Control creation functions:
	osCreateRadioControl parentWindow parentPos title able pos size selected isfirst
		creates a RadioControl in the window identified by parentWindow. 
	osCreateCheckControl parentWindow parentPos title able pos size selected isfirst
		creates a CheckControl in the window identified by parentWindow.
	osCreateEmptyPopUpControl parentWindow parentPos able pos nrItems keySensitive
		creates an initially empty PopUpControl that will display nrItems elements. 
		The boolean keySensitive holds iff the PopUpControl should respond to keyboard input (is editable).
		The first result OSWindowPtr is the PopUpControl, the second OSWindowPtr is the EditControl (if editable).
	osCreatePopUpControlItem parentPopUp pos able title selected
		adds an item title to the parentPopUp PopUpControl. The pos argument determines the location of 
		the item title. If (-1), the item is appended, otherwise it is created behind the item with the
		given pos index. The return Int is its zero based index.
	osCreateSliderControl parentWindow parentPos show able horizontal pos size range
		creates a horizontal (True) or vertical (False) SliderControl in the window identified by parentWindow.
	osCreateTextControl parentWindow parentPos text pos size
		creates a TextControl in the window identified by parentWindow.
	osCreateEditControl parentWindow parentPos text able isKeySensitive pos size
		creates an EditControl in the window identified by parentWindow.
	osCreateButtonControl parentWindow parentPos title able pos size okOrCancel
		creates a ButtonControl in the window identified by parentWindow.
	osCreateCustomButtonControl parentWindow parentPos able pos size okOrCancel
		creates a CustomButtonControl in the window identified by parentWindow.
	osCreateCustomControl parentWindow parentPos able pos size 
		creates a CustomControl in the window identified by parentWindow.
	osCreateCompoundControl parentWindow parentPos show able isTransparent pos size hScrollInfo vScrollInfo
		creates a CompoundControl in the window identified by parentWindow.
		The Boolean isTransparent should be True iff the CompoundControl has no ControlLook attribute.
*/
::	OKorCANCEL
	=	OK | CANCEL | NORMAL

osCreateRadioControl		:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																					 -> (!OSWindowPtr,!*OSToolbox)
osCreateCheckControl		:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																					 -> (!OSWindowPtr,!*OSToolbox)
osCreateEmptyPopUpControl	:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Int !Bool !*OSToolbox
																			-> (!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
osCreatePopUpControlItem	:: !OSWindowPtr !(Maybe OSWindowPtr) !Int !Bool !String !Bool !Int !*OSToolbox -> (!Int,!*OSToolbox)
osCreatePopUpControlItems	:: !OSWindowPtr !(Maybe OSWindowPtr) !Bool ![String] !Int !*OSToolbox -> *OSToolbox
osCreateSliderControl		:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !(!Int,!Int,!Int,!Int) !*OSToolbox
																										   -> (!OSWindowPtr,!*OSToolbox)
osCreateTextControl			:: !OSWindowPtr !(!Int,!Int) !String !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox
																   -> (!OSWindowPtr,!*OSToolbox)
osCreateEditControl			:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox
																							-> (!OSWindowPtr,!*OSToolbox)
osCreateButtonControl		:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox
																			   -> (!OSWindowPtr,!*OSToolbox)
osCreateCustomButtonControl	:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox
																	   -> (!OSWindowPtr,!*OSToolbox)
osCreateCustomControl		:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox
																 -> (!OSWindowPtr,!*OSToolbox)

::	ScrollbarInfo
	=	{	cbiHasScroll	:: !Bool				// The scrollbar exists
		,	cbiPos			:: (Int,Int)			// Its position within the parent
		,	cbiSize			:: (Int,Int)			// Its size within the parent
		,	cbiState		:: (Int,Int,Int,Int)	// Its (min,thumb,max,thumbsize) settings
		}

osCreateCompoundControl		:: !OSWindowMetrics !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int)
										 !ScrollbarInfo !ScrollbarInfo !*OSToolbox
							-> (!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!*OSToolbox)


/*	Window destruction operations.
	osDestroyWindow isModal isWindow window
		destroys the window identified by window. 
		The first  Boolean isModal is True iff the window is Modal.
		The second Boolean isWindow is True iff the window is a Window.
*/
osDestroyWindow :: !Bool !Bool !OSWindowPtr !(OSEvent -> .s -> ([Int],.s)) !OSDInfo !.s !*OSToolbox
												-> (![DelayActivationInfo],!OSDInfo, .s,!*OSToolbox)


/*	Control destruction operations.
*/
osDestroyRadioControl		:: !OSWindowPtr	!*OSToolbox -> *OSToolbox
osDestroyCheckControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyPopUpControl		:: !OSWindowPtr !(Maybe OSWindowPtr) !*OSToolbox -> *OSToolbox
osDestroySliderControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyTextControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyEditControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyButtonControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCustomButtonControl:: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCustomControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCompoundControl	:: !OSWindowPtr !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox


/*	Control update operations.
	osUpdateRadioControl	area pos parentWindow theControl updates the area of theControl in parentWindow
	osUpdateCheckControl	area pos parentWindow theControl updates the area of theControl in parentWindow
	osUpdatePopUpControl	area pos parentWindow theControl updates the area of theControl in parentWindow
	osUpdateSliderControl	area pos parentWindow theControl updates the area of theControl in parentWindow
	osUpdateTextControl		area pos parentWindow theControl updates the area of theControl in parentWindow
	osUpdateEditControl		area pos parentWindow theControl updates the area of theControl in parentWindow
	osUpdateButtonControl	area pos parentWindow theControl updates the area of theControl in parentWindow
	osUpdateCompoundControl area pos parentWindow theControl updates the area of theControl in parentWindow
	
	Both area and pos must in window coordinates (zero at left-top). 
*/
osUpdateRadioControl		:: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateCheckControl		:: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdatePopUpControl		:: !OSRect !OSWindowPtr !OSWindowPtr !(Maybe OSWindowPtr) !(!Int,!Int) !(!Int,!Int) !Bool !String !*OSToolbox -> *OSToolbox
osUpdateSliderControl		:: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateTextControl			:: !OSRect !OSRect !String !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateEditControl			:: !OSRect !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateButtonControl		:: !OSRect !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateCompoundControl		:: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox


/*	Control clipping operations.
	osClipRadioControl        parentWindow parentPos area pos size generates the clipping region of a radio control within area.
	osClipCheckControl        parentWindow parentPos area pos size generates the clipping region of a check control within area.
	osClipPopUpControl        parentWindow parentPos area pos size generates the clipping region of a pop up control within area.
	osClipSliderControl       parentWindow parentPos area pos size generates the clipping region of a slider control within area.
	osClipTextControl         parentWindow parentPos area pos size generates the clipping region of a text control within area.
	osClipEditControl         parentWindow parentPos area pos size generates the clipping region of a edit control within area.
	osClipButtonControl       parentWindow parentPos area pos size generates the clipping region of a button control within area.
	osClipCustomButtonControl parentWindow parentPos area pos size generates the clipping region of a custom button control within area.
	osClipCustomControl       parentWindow parentPos area pos size generates the clipping region of a custom control within area.
	osClipCompoundControl     parentWindow parentPos area pos size generates the clipping region of a compound control within area.
*/
osClipRadioControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCheckControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipPopUpControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipSliderControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipTextControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipEditControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipButtonControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCustomButtonControl	:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCustomControl			:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCompoundControl		:: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)


/*	Window graphics context access operations.
	osGrabWindowPictContext theWindow
		returns the graphics context that must be used to update the window.
	osReleaseWindowPictContext theWindow theContext
		releases the graphics context.
*/
osGrabWindowPictContext		:: !OSWindowPtr					!*OSToolbox -> (!OSPictContext,!*OSToolbox)
osReleaseWindowPictContext	:: !OSWindowPtr !OSPictContext	!*OSToolbox -> *OSToolbox


/*	osBeginUpdate theWindow
		makes additional preparations to do updates. Dummy on Windows.
	osEndUpdate theWindow
		administrates and ends the update. Dummy on Windows.
*/
osBeginUpdate :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osEndUpdate   :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osSetUpdate   :: !OSWindowPtr !*OSToolbox -> *OSToolbox


/*	(acc/app)Grafport theWindow f
		applies f to the graphics context of theWindow (dummy on Windows).
	(acc/app)Clipport theWindow clipRect f
		applies f to the graphics context of theWindow while clipping clipRect (dummy on Windows).
*/
accGrafport :: !OSWindowPtr         !.(St *OSToolbox .x)         !*OSToolbox -> (!.x, !*OSToolbox)
appGrafport :: !OSWindowPtr         !.(*OSToolbox -> *OSToolbox) !*OSToolbox ->        *OSToolbox
accClipport :: !OSWindowPtr !OSRect !.(St *OSToolbox .x)         !*OSToolbox -> (!.x, !*OSToolbox)
appClipport :: !OSWindowPtr !OSRect !.(*OSToolbox -> *OSToolbox) !*OSToolbox ->        *OSToolbox


/*	Scrollbar operations.
*/

/*	toOSscrollbarRange (domainMin,viewMin,domainMax) viewSize
		maps the (domainMin,viewMin,domainMax) viewSize values to proper OS values (osRangeMin,osThumb,osRangeMax,osThumbSize).
	fromOSscrollbarRange (domainMin,domainMax) osThumb
		maps the osThumb value between the (domainMin,domainMax) values.
	These values are also valid for CompoundControls.
	Both functions assume that:domainMin<=viewMin<= domainMax 
						  and osRangeMin<=osThumb<=osRangeMax.
	osScrollbarIsVisible (domainMin,domainMax) viewSize
		determines whether the scrollbar is visible given these settings.
	osScrollbarsAreVisible wMetrics windowDomain size (hasHScroll,hasVScroll)
		determines the visibility of the horizontal/vertical scrollbars given the domain, size, and presence.
*/
toOSscrollbarRange		:: !(!Int,!Int,!Int) !Int -> (!Int,!Int,!Int,!Int)
fromOSscrollbarRange	:: !(!Int,!Int) !Int -> Int
osScrollbarIsVisible	:: !(!Int,!Int) !Int -> Bool
osScrollbarsAreVisible	:: !OSWindowMetrics !OSRect !(!Int,!Int) !(!Bool,!Bool) -> (!Bool,!Bool)


/*	Window access operations.
*/

/*	osSetWindowSliderThumb theWindow isHorizontal thumb redraw
		sets the thumb value of the horizontal/vertical slider of the given window.
	osSetWindowSliderThumbSize theWindow isHorizontal size redraw
		sets the view size of the horizontal/vertical slider of the given window.
*/
osSetWindowSliderThumb		:: !OSWindowMetrics !OSWindowPtr !Bool !Int !(Maybe OSWindowPtr) !(Maybe OSWindowPtr) !OSRect !OSRect !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetWindowSliderThumbSize	:: !OSWindowMetrics !OSWindowPtr !OSWindowPtr !Bool !Int !Int !Int !(!Int,!Int) !OSRect !Bool !Bool !*OSToolbox -> *OSToolbox
osSetWindowSliderPosSize	:: !OSWindowPtr !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox


/*	osInvalidateWindow theWindow
		invalidates the window identified by theWindow, forcing an update event for the entire contents.
	osInvalidateWindowRect theWindow part
		invalidates the part of the window identified by theWindow, forcing an update event for that part.
	osValidateWindowRect theWindow part
		validates the OSRect part of the window identified by theWindow, eliminating the need to update that part.
	osValidateWindowRgn theWindow part
		validate the Rgn part of the window identified by the theWindow, eliminating the need to update that part.
*/
osInvalidateWindow		:: !OSWindowPtr					!*OSToolbox -> *OSToolbox
osInvalidateWindowRect	:: !OSWindowPtr !OSRect			!*OSToolbox -> *OSToolbox
osValidateWindowRect	:: !OSWindowPtr !OSRect			!*OSToolbox -> *OSToolbox
osValidateWindowRgn		:: !OSWindowPtr !OSRgnHandle	!*OSToolbox -> *OSToolbox
osWindowHasUpdateRect	:: !OSWindowPtr					!*OSToolbox -> (!Bool,!*OSToolbox)


/*	os(Dis/En)ableWindow theWindow
		(dis/en)able the window identified by theWindow.
		The Boolean tuple indicates whether the window has a horizontal/vertical scrollbar.
		The last Boolean argument indicates whether the window is (dis/en)abled because of a modal dialogue.
*/
osDisableWindow			:: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
osEnableWindow			:: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox

/*	osActivateWindow osdInfo thisWindow handleEvents info
		activates thisWindow. The handleEvents function is applied when updates are required.
	osActivateControl parentWindow theControl
		activates theControl which is in parentWindow. 
	osStackWindow thisWindow behindWindow
		moves the window identified by thisWindow behind the window identified by behindWindow.
		osStackWindow assumes that thisWindow and behindWindow are valid values.
*/
osActivateWindow	:: !OSDInfo !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
															   -> (![DelayActivationInfo],!.s,!*OSToolbox)
osActivateControl	:: !OSWindowPtr !OSWindowPtr	!*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osStackWindow		:: !OSWindowPtr !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
																   -> (![DelayActivationInfo],!.s,!*OSToolbox)

/*	osHideWindow thisWindow activate
		hides the window. If the Boolean activate is True then a new window is made the active window.
	osShowWindow thisWindow activate
		shows the window. If the Boolean activate is True then the window is also made the active 
		window. If the Boolean activate is False then the stacking order is not changed.
*/
osHideWindow	:: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osShowWindow	:: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)

/*	osSetWindowCursor sets the new cursor shape.
*/
osSetWindowCursor :: !OSWindowPtr !CursorShape !*OSToolbox -> *OSToolbox

/*	osGetWindowPos           returns the current position of the window.
	osGetWindowViewFrameSize returns the current size of the window view frame.
	osGetWindowSize          returns the current size of the window including bounds.
	osSetWindowPos           sets the position of the window.
	osSetWindowViewFrameSize sets the size of the window view frame.
	osSetWindowSize          sets the size of the window.
*/
osGetWindowPos			:: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowViewFrameSize:: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowSize         :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osSetWindowPos			:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !*OSToolbox -> *OSToolbox
osSetWindowViewFrameSize:: !OSWindowPtr !(!Int,!Int)             !*OSToolbox -> *OSToolbox
osSetWindowSize			:: !OSWindowPtr !(!Int,!Int) !Bool       !*OSToolbox -> *OSToolbox

/*	osSetWindowTitle sets the title of the window.
*/
osSetWindowTitle:: !OSWindowPtr !String !*OSToolbox -> *OSToolbox


/*	Control access operations.
*/

/*	On compound controls:
	osInvalidateCompound compoundPtr
		invalidates the compound control, forcing an update event for the entire contents.
	osInvalidateCompoundRect compoundPtr part
		invalidates the part of the compound control, forcing an update event for that part.
	osSetCompoundSliderThumb compoundPtr isHorizontal thumb (maxx,maxy) redraw
		sets the thumb value of the horizontal/vertical slider of the given compound control.
		(maxx,maxy) are the maximum x and y coordinates of the enclosing rectangle of the slider.
	osSetCompoundSliderThumbSize compoundPtr isHorizontal size (maxx,maxy) redraw
		sets the view size of the horizontal/vertical slider of the given compound control.
		(maxx,maxy) are the maximum x and y coordinates of the enclosing rectangle of the slider.
	osSetCompoundSlider compoundPtr isHorizontal (osRangeMin,osThumb,osRangeMax,osThumbSize)
		sets all slider values of the horizontal/vertical slider of the given compound control.
	osSetCompoundSelect parentWindow compoundPtr clipRect (hasHScroll,hasVScroll) toAble
		enables the compound control (if toAble), or disables the compound control (if (not toAble)), while clipping.
	osSetCompoundShow parentWindow compoundPtr clipRect show
		shows the compound control (if show), or hides the compound control (if (not show)), while clipping.
	osSetCompoundPos parentWindow parentPos compoundPtr pos size update
		sets the new position of the compound control and updates the control if update holds. 
	osSetCompoundSize parentWindow parentPos compoundPtr pos size update
		sets the new size of the compound control and updates the control if update holds.
	osUpdateCompoundScroll parentWindow compoundPtr scrollRect
		updates the compound control. 
	osCompoundMovesControls
		is True iff moving a compound control also moves its elements.
	osCompoundControlHasOrigin
		is True iff compound control has a private origin; otherwise related to its item position.
*/
osInvalidateCompound			:: !OSWindowPtr																			!*OSToolbox -> *OSToolbox
//osInvalidateCompoundRect		:: !OSWindowPtr !OSRect																	!*OSToolbox -> *OSToolbox	PA: not used
//osSetCompoundSliderThumb		:: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool							!*OSToolbox -> *OSToolbox
osSetCompoundSliderThumb		:: !OSWindowMetrics !OSWindowPtr !OSWindowPtr !OSWindowPtr !OSRect !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
//osSetCompoundSliderThumbSize	:: !OSWindowMetrics !OSWindowPtr !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool				!*OSToolbox -> *OSToolbox
osSetCompoundSliderThumbSize	:: !OSWindowMetrics !OSWindowPtr !OSWindowPtr !OSWindowPtr !Int !Int !Int !OSRect !Bool !Bool !Bool !*OSToolbox -> *OSToolbox
//osSetCompoundSlider			:: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int)				!*OSToolbox -> *OSToolbox	PA: not used
osSetCompoundSelect				:: !OSWindowPtr !OSWindowPtr !OSRect !(!Bool,!Bool) !(!OSWindowPtr,!OSWindowPtr) !Bool	!*OSToolbox -> *OSToolbox
osSetCompoundShow				:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool										!*OSToolbox -> *OSToolbox
osSetCompoundPos				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool				!*OSToolbox -> *OSToolbox
osSetCompoundSize				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool				!*OSToolbox -> *OSToolbox
osUpdateCompoundScroll			:: !OSWindowPtr !OSWindowPtr !OSRect													!*OSToolbox -> *OSToolbox
osCompoundMovesControls			:== True
osCompoundControlHasOrigin		:== True

/*	On slider controls:
	osSetSliderControlThumb parentWindow sliderPtr clipRect redraw (min,thumb,max)
		sets the thumb value of the slider control, while clipping.
	osSetSliderControlSelect parentWindow sliderPtr clipRect toAble
		enables the slider control (if toAble), or disables the slider control (if (not toAble)), while clipping.
	osSetSliderControlShow parentWindow sliderPtr clipRect show
		shows the slider control (if show), or hides the slider control (if (not show)), while clipping.
	osSetSliderControlPos parentWindow parentPos sliderPtr pos size update
		sets the new position of the slider control and updates the control if update holds.
	osSetSliderControlSize parentWindow parentPos sliderPtr pos size update
		sets the new size of the slider control and updates the control if update holds.
*/
osSetSliderControlThumb			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool !(!Int,!Int,!Int,!Int)			!*OSToolbox -> *OSToolbox
osSetSliderControlSelect		:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetSliderControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetSliderControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osSetSliderControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On radio controls:
	osSetRadioControl parentWindow current new clipRect
		removes the selection from current and sets the selection to new, while clipping.
	osSetRadioControlSelect parentWindow radioPtr clipRect toAble
		enables the radio control (if toAble), or disables the radio control (if (not toAble)), while clipping.
	osSetRadioControlShow parentWindow radioPtr clipRect show
		shows the radio control (if show), or hides the radio control (if (not show)), while clipping.
	osSetRadioControlPos parentWindow parentPos radioPtr pos size update
		sets the new position of the radio control and updates the control if update holds.
	osSetRadioControlSize parentWindow parentPos radioPtr pos size update
		sets the new size of the radio control and updates the control if update holds.
*/
osSetRadioControl				:: !OSWindowPtr !OSWindowPtr !OSWindowPtr !OSRect							!*OSToolbox -> *OSToolbox
osSetRadioControlSelect			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetRadioControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetRadioControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osSetRadioControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On check controls:
	osSetCheckControl parentWindow checkPtr clipRect marked
		sets the check mark (if marked) or removes the check mark (if not marked) of the check control, while clipping.
	osSetCheckControlSelect parentWindow checkPtr clipRect toAble
		enables the check control (if toAble), or disables the check control (if (not toAble)), while clipping.
	osSetCheckControlShow parentWindow checkPtr clipRect show
		shows the check control (if show), or hides the check control (if (not show)), while clipping.
	osSetCheckControlPos parentWindow parentPos checkPtr pos size update
		sets the new position of the check control and updates the control if update holds.
	osSetCheckControlSize parentWindow parentPos checkPtr pos size update
		sets the new size of the check control and updates the control if update holds.
*/
osSetCheckControl				:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetCheckControlSelect			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetCheckControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetCheckControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int)	!Bool	!*OSToolbox -> *OSToolbox
osSetCheckControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int)	!Bool	!*OSToolbox -> *OSToolbox

/*	On pop up controls:
	osSetPopUpControl parentWindow popupPtr clipRect current new newtext shown
		removes the selection from current and sets the selection to new, while clipping. Both indices are zero based!
	osSetPopUpControlSelect parentWindow popupPtr clipRect toAble
		enables the pop up control (if toAble), or disables the pop up control (if (not toAble)), while clipping.
	osSetPopUpControlShow parentWindow popupPtr clipRect show
		shows the pop up control (if show), or hides the pop up control (if (not show)), while clipping.
	osSetPopUpControlPos parentWindow parentPos popupPtr pos size update
		sets the new position of the pop up control and updates the control if update holds.
	osSetPopUpControlSize parentWindow parentPos popupPtr pos size update
		sets the new size of the pop up control and updates the control if update holds.
*/
osSetPopUpControl				:: !OSWindowPtr !OSWindowPtr !(Maybe OSWindowPtr) !OSRect !OSRect !Int !Int !String !Bool	!*OSToolbox -> *OSToolbox
osSetPopUpControlSelect			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool												!*OSToolbox -> *OSToolbox
osSetPopUpControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool												!*OSToolbox -> *OSToolbox
osSetPopUpControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool				!*OSToolbox -> *OSToolbox
osSetPopUpControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool				!*OSToolbox -> *OSToolbox
osGetPopUpControlText			:: !OSWindowPtr !OSWindowPtr															!*OSToolbox -> (!String,!*OSToolbox) 

/*	On edit controls:
	osSetEditControlText parentWindow editPtr clipRect itemRect shown text 
		sets the text of the shown edit control while clipping.
	osGetEditControlText parentWindow editPtr 
		returns the current content of the edit control.
	osSetEditControlCursor parentWindow editPtr clipRect itemRect pos
		sets the cursor position at pos of the edit control while clipping.
	osSetEditControlSelection parentWindow editPtr clipRect itemRect start end
		sets the selection of the text in the edit control while clipping.
	osSetEditControlSelect parentWindow editPtr clipRect toAble
		enables the edit control (if toAble), or disables the edit control (if (not toAble)), while clipping.
	osSetEditControlShow parentWindow editPtr clipRect show
		shows the edit control (if show), or hides the edit control (if (not show)), while clipping.
	osSetEditControlPos parentWindow parentPos editPtr pos size update
		sets the new position of the edit control and updates the control if update holds.
	osSetEditControlSize parentWindow parentPos editPtr pos size update
		sets the new size of the edit control and updates the control if update holds.
*/
osSetEditControlText			:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !String					!*OSToolbox -> *OSToolbox
osGetEditControlText			:: !OSWindowPtr !OSWindowPtr												!*OSToolbox -> (!String,!*OSToolbox) 
osSetEditControlCursor			:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Int							!*OSToolbox -> *OSToolbox
osSetEditControlSelection		:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Int !Int						!*OSToolbox -> *OSToolbox
osSetEditControlSelect			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetEditControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetEditControlPos				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osSetEditControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osIdleEditControl				:: !OSWindowPtr !OSRect !OSWindowPtr										!*OSToolbox -> *OSToolbox

/*	On text controls:
	osSetTextControlText parentWindow textPtr clipRect itemRect shown text
		sets the text of the shown edit control while clipping.
	osSetTextControlSelect parentWindow textPtr clipRect toAble
		enables the text control (if toAble), or disables the text control (if (not toAble)), while clipping.
	osSetTextControlShow parentWindow textPtr clipRect show
		shows the text control (if show), or hides the text control (if (not show)), while clipping.
	osSetTextControlPos parentWindow parentPos textPtr pos size update
		sets the new position of the text control and updates the control if update holds.
	osSetTextControlSize parentWindow parentPos textPtr pos size update
		sets the new size of the text control and updates the control if update holds.
*/
osSetTextControlText			:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !String					!*OSToolbox -> *OSToolbox
osSetTextControlSelect			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetTextControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !String					!*OSToolbox -> *OSToolbox
osSetTextControlPos				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osSetTextControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On button controls:
	osSetButtonControlText parentWindow buttonPtr clipRect text
		sets the text of the button control while clipping.
	osSetButtonControlSelect parentWindow buttonPtr clipRect toAble
		enables the button control (if toAble), or disables the button control (if (not toAble)), while clipping.
	osSetButtonControlShow parentWindow buttonPtr clipRect show
		shows the button control (if show), or hides the button control (if (not show)), while clipping.
	osSetButtonControlPos parentWindow parentPos buttonPtr pos size update
		sets the new position of the button control and updates the control if update holds.
	osSetButtonControlSize parentWindow parentPos buttonPtr pos size update
		sets the new size of the button control and updates the control if update holds.
*/
osSetButtonControlText			:: !OSWindowPtr !OSWindowPtr !OSRect !String								!*OSToolbox -> *OSToolbox
osSetButtonControlSelect		:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetButtonControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetButtonControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osSetButtonControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On custom button controls:
	osSetCustomButtonControlSelect parentWindow buttonPtr clipRect toAble
		enables the custom button control (if toAble), or disables the custom button control (if (not toAble)), while clipping.
	osSetCustomButtonControlShow parentWindow buttonPtr clipRect show
		shows the custom button control (if show), or hides the custom button control (if (not show)), while clipping.
	osSetCustomButtonControlPos parentWindow parentPos buttonPtr pos size update
		sets the new position of the custom button control and updates the custom button if update holds.
	osSetCustomButtonControlSize parentWindow parentPos buttonPtr pos size update
		sets the new size of the custom button control and updates the custom button if update holds.
	osCustomButtonControlHasOrigin
		is True iff the control has a private origin; otherwise related to its item position.
*/
osSetCustomButtonControlSelect	:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetCustomButtonControlShow	:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool							!*OSToolbox -> *OSToolbox
osSetCustomButtonControlPos		:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osSetCustomButtonControlSize	:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osCustomButtonControlHasOrigin	:== True

/*	On custom controls:
	osSetCustomControlSelect parentWindow controlPtr clipRect toAble
		enables the custom control (if toAble), or disables the custom control (if (not toAble)), while clipping.
	osSetCustomControlShow parentWindow controlPtr clipRect show
		shows the custom control (if show), or hides the custom control (if (not show)), while clipping.
	osSetCustomControlPos parentWindow parentPos controlPtr pos size update
		sets the new position of the custom control and updates the control if update holds.
	osSetCustomControlSize parentWindow parentPos controlPtr pos size update
		sets the new size of the custom control and updates the control if update holds.
	osCustomControlHasOrigin
		is True iff the control has a private origin; otherwise related to its item position.
*/
osSetCustomControlSelect		:: !OSWindowPtr !OSWindowPtr !OSRect !Bool									!*OSToolbox -> *OSToolbox
osSetCustomControlShow			:: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool							!*OSToolbox -> *OSToolbox
osSetCustomControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osSetCustomControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
osCustomControlHasOrigin		:== True

//--

osSetCursorShape				:: !CursorShape !*OSToolbox -> *OSToolbox
