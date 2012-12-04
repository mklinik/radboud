implementation module oswindow


import	StdBool, StdInt, StdReal, StdClass, StdOverloaded, StdList, StdMisc, StdTuple
from	StdIOCommon			import :: CursorShape(StandardCursor,BusyCursor,IBeamCursor,CrossCursor,FatCrossCursor,ArrowCursor,HiddenCursor)
import	clCrossCall_12, clCCall_12, windowCCall_12, windowCrossCall_12
import	osdocumentinterface, osevent, osfont, ospicture, osrgn, ossystem, ostypes
from	menuCrossCall_12	import :: HMENU
from	commondef			import fatalError, intersectRects, rectSize, stateMap2,
								class fromTuple(..), instance fromTuple Vector2,
								class toTuple4(..),  instance toTuple4  OSRect,
								class subVector(..), instance subVector OSRect,
								instance zero OSRect
//import	StdDebug, tracetypes	// PA: for debugging purposes


oswindowFatalError :: String String -> .x
oswindowFatalError function error
	= fatalError function "oswindow" error


/*	Initialisation:
*/
osInitialiseWindows :: !*OSToolbox -> *OSToolbox
osInitialiseWindows tb
	= winInitialiseWindows tb


/*	System dependent constants:
*/
OSControlTitleSpecialChars
	:== []											// Special prefix characters that should be removed


/*	System dependent metrics:
*/

osMinWindowSize :: (!Int,!Int)
osMinWindowSize = winMinimumWinSize

osMinCompoundSize :: (!Int,!Int)
osMinCompoundSize = (0,0)	// PA: (0,0)<--winMinimumWinSize (Check if this safe)


/*	Window frame dimensions: (PA: were defined as constants in windowvalidate. Moved here.)
*/
osWindowFrameWidth     :: Int;	osWindowFrameWidth     = 0;
osWindowTitleBarHeight :: Int;	osWindowTitleBarHeight = 0;


//	Calculating the view frame of window/compound with visibility of scrollbars.

osGetCompoundContentRect :: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetCompoundContentRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| visHScroll && visVScroll	= {itemRect & rright=r`,rbottom=b`}
	| visHScroll				= {itemRect &           rbottom=b`}
	| visVScroll				= {itemRect & rright=r`           }
	| otherwise					= itemRect
where
	r`							= rright -osmVSliderWidth
	b`							= rbottom-osmHSliderHeight

osGetCompoundHScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetCompoundHScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| not visHScroll	= zero
	| otherwise			= {itemRect & rtop=b`,rright=if visVScroll r` rright}
where
	r`					= rright -osmVSliderWidth
	b`					= rbottom-osmHSliderHeight

osGetCompoundVScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetCompoundVScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| not visVScroll	= zero
	| otherwise			= {itemRect & rleft=r`,rbottom=if visHScroll b` rbottom}
where
	r`					= rright -osmVSliderWidth
	b`					= rbottom-osmHSliderHeight


osGetWindowContentRect :: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetWindowContentRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| visHScroll && visVScroll	= {itemRect & rright=r`,rbottom=b`}
	| visHScroll				= {itemRect &           rbottom=b`}
	| visVScroll				= {itemRect & rright=r`           }
	| otherwise					= itemRect
where
	r`							= rright -osmVSliderWidth //+1
	b`							= rbottom-osmHSliderHeight//+1

osGetWindowHScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetWindowHScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) {rleft,rtop,rright,rbottom}
	| not visHScroll	= zero
	| otherwise			= {rleft=rleft-1,rtop=b`,rright=if visVScroll (r`+1) (rright+1),rbottom=rbottom+1}
where
	r`					= rright -osmVSliderWidth  + 1
	b`					= rbottom-osmHSliderHeight + 1

osGetWindowVScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !OSRect -> OSRect
osGetWindowVScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) {rleft,rtop,rright,rbottom}
	| not visVScroll	= zero
	| otherwise			= {rleft=r`,rtop=rtop-1,rright=rright+1,rbottom=if visHScroll (b`+1) (rbottom+1)}
where
	r`					= rright -osmVSliderWidth  + 1
	b`					= rbottom-osmHSliderHeight + 1


/*	Determine the size of controls.
*/
osGetButtonControlSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetButtonControlSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((2*osmHeight+width,osGetButtonControlHeight wMetrics),tb)

osGetButtonControlHeight :: !OSWindowMetrics -> Int
osGetButtonControlHeight {osmHeight} = 2*osmHeight

osGetTextControlSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetTextControlSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+osmHeight/4,osGetTextControlHeight wMetrics),tb)

osGetTextControlHeight :: !OSWindowMetrics -> Int
osGetTextControlHeight {osmHeight} = osmHeight+osmHeight/2

osGetEditControlSize :: !OSWindowMetrics !Int !Int !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetEditControlSize wMetrics width nrlines tb
	= ((width,osGetEditControlHeight wMetrics nrlines),tb)

osGetEditControlHeight :: !OSWindowMetrics !Int -> Int
osGetEditControlHeight {osmHeight} nrlines = osmHeight/2+osmHeight*nrlines

osGetPopUpControlSize :: !OSWindowMetrics ![String] !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetPopUpControlSize wMetrics=:{osmFont,osmHeight} items tb
	# (widths,tb)	= osGetfontstringwidths False 0 items osmFont tb
	  maxwidth		= listmax widths
	= ((maxwidth+2*osmHeight+osmHeight/2,osGetPopUpControlHeight wMetrics),tb)
where
	listmax :: ![Int] -> Int
	listmax [x:xs]	= foldr max x xs
	listmax _		= 0

osGetPopUpControlHeight :: !OSWindowMetrics -> Int
osGetPopUpControlHeight {osmHeight} = osmHeight+osmHeight/2+2

osGetRadioControlItemSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetRadioControlItemSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+2*osmHeight+osmHeight/2,osGetRadioControlItemHeight wMetrics),tb)

osGetRadioControlItemHeight :: !OSWindowMetrics -> Int
osGetRadioControlItemHeight {osmHeight}
	= osmHeight+osmHeight/2

osGetCheckControlItemSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetCheckControlItemSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+2*osmHeight+osmHeight/2,osGetCheckControlItemHeight wMetrics),tb)

osGetCheckControlItemHeight :: !OSWindowMetrics -> Int
osGetCheckControlItemHeight {osmHeight}
	= osmHeight+osmHeight/2

osGetSliderControlSize :: !OSWindowMetrics !Bool !Int -> (!Int,!Int)
osGetSliderControlSize wMetrics isHorizontal length
	| isHorizontal	= (length,wMetrics.osmHSliderHeight)
	| otherwise		= (wMetrics.osmVSliderWidth,length)



/*	Determine the minimum width of controls.
*/
osGetButtonControlMinWidth :: !OSWindowMetrics -> Int
osGetButtonControlMinWidth {osmHeight} = 2*osmHeight

osGetTextControlMinWidth :: !OSWindowMetrics -> Int
osGetTextControlMinWidth {osmHeight} = osmHeight/4

osGetEditControlMinWidth :: !OSWindowMetrics -> Int
osGetEditControlMinWidth _ = 0

osGetPopUpControlMinWidth :: !OSWindowMetrics -> Int
osGetPopUpControlMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

osGetRadioControlItemMinWidth :: !OSWindowMetrics -> Int
osGetRadioControlItemMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

osGetCheckControlItemMinWidth :: !OSWindowMetrics -> Int
osGetCheckControlItemMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

osGetSliderControlMinWidth :: !OSWindowMetrics -> Int
osGetSliderControlMinWidth _ = 0


/*	Window creation functions.
*/
osCreateDialog :: !Bool !Bool !String !(!Int,!Int) !(!Int,!Int) !OSWindowPtr
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!u:s,!*OSToolbox)
osCreateDialog isModal isClosable title pos size behindPtr get_focus create_controls update_controls osdinfo control_info tb
	# (textPtr,tb)	= winMakeCString title tb
	  createcci		= Rq4Cci CcRqCREATEDIALOG textPtr parentptr (if (behindPtr==OSNoWindowPtr) 0 behindPtr) (toInt isModal)
	# (returncci,(control_info,delay_info),tb)
					= issueCleanRequest (osCreateDialogCallback get_focus create_controls update_controls)
										createcci
										(control_info,[]) tb
	# tb			= winReleaseCString textPtr tb
	  wPtr			= case returncci.ccMsg of
	  					CcRETURN1	-> returncci.p1
	  					CcWASQUIT	-> OSNoWindowPtr
	  					_			-> oswindowCreateError 1 "osCreateDialog"
	= (reverse delay_info,wPtr,control_info,tb)
where
	parentptr		= case (getOSDInfoOSInfo osdinfo) of
						Nothing        -> 0
						Just {osFrame} -> osFrame
	
	osCreateDialogCallback :: !(u:s->*(OSWindowPtr,u:s))
							  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
							  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
							  !CrossCallInfo !*(u:s,[DelayActivationInfo]) !*OSToolbox
						  -> (!CrossCallInfo,!*(u:s,[DelayActivationInfo]),!*OSToolbox)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmPAINT,p1=hwnd} s tb
		= //trace_n "osCreateDialogCallback CcWmPAINT" 
		  (return0Cci, s, winFakePaint hwnd tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmACTIVATE,p1=hwnd} (control_info,delay_info) tb
		= //trace_n "osCreateDialogCallback CcWmACTIVATE" 
		  (return0Cci, (control_info,[DelayActivatedWindow hwnd:delay_info]), tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmDEACTIVATE,p1=hwnd} (control_info,delay_info) tb
		= //trace_n "osCreateDialogCallback CcWmDEACTIVATE" 
		  (return0Cci, (control_info,[DelayDeactivatedWindow hwnd:delay_info]), tb)
	osCreateDialogCallback get_focus create_controls _ {ccMsg=CcWmINITDIALOG,p1=hwnd} (control_info,delay_info) tb
		# (control_info,tb)			= create_controls hwnd control_info tb
		# (defhandle,control_info)	= get_focus control_info
		  (x,y)						= pos
		  (w,h)						= size
		  r5cci						= return5Cci x y w h (if (defhandle==OSNoWindowPtr) 0 defhandle)
		= (r5cci, (control_info,delay_info), tb)
	osCreateDialogCallback _ _ update_controls {ccMsg=CcWmDRAWCONTROL,p1=hdlog,p2=hctrl,p3=hdc} (control_info,delay_info) tb
		# (control_info,tb)			= update_controls hdlog hctrl hdc control_info tb
		= (return0Cci, (control_info,delay_info), tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmKEYBOARD} s tb
		= //trace_n "osCreateDialogCallback CcWmKEYBOARD"
		  (return0Cci, s, tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmSETFOCUS} s tb
		= //trace_n "osCreateDialogCallback CcWmSETFOCUS"
		  (return0Cci, s, tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmKILLFOCUS} s tb
		= //trace_n "osCreateDialogCallback CcWmKILLFOCUS"
		  (return0Cci, s, tb)
	osCreateDialogCallback _ _ _ {ccMsg} s tb
		= oswindowFatalError "osCreateDialogCallback" ("unknown message type ("+++toString ccMsg+++")")

osCreateWindow :: !OSWindowMetrics !Bool !ScrollbarInfo !ScrollbarInfo !(!Int,!Int) !(!Int,!Int)
				  !Bool !String !(!Int,!Int) !(!Int,!Int)
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !OSWindowPtr !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!OSDInfo,!u:s,!*OSToolbox)
osCreateWindow	wMetrics isResizable hInfo=:{cbiHasScroll=hasHScroll} vInfo=:{cbiHasScroll=hasVScroll} minSize maxSize
				isClosable title pos size
				get_focus
				create_controls
				update_controls
				osdInfo behindPtr control_info tb
	| di==MDI
		# (textPtr,tb)	= winMakeCString title tb
		  styleFlags	= WS_SYSMENU
		  					bitor WS_OVERLAPPED
		  					bitor (if hasHScroll  WS_HSCROLL    0)
		  					bitor (if hasVScroll  WS_VSCROLL    0)
		  					bitor (if isResizable WS_THICKFRAME 0)
		  				//	bitor WS_CLIPCHILDREN
		  createcci		= Rq6Cci CcRqCREATEMDIDOCWINDOW textPtr osinfo.osClient behindPtr (x<<16+(y bitand 0xffff)) (w<<16+(h bitand 0xffff)) styleFlags
		# (returncci,(control_info,delay_info),tb)
						= issueCleanRequest (osCreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[]) tb
		# tb			= winReleaseCString textPtr tb
		  wPtr			= case returncci.ccMsg of
		  					CcRETURN1	-> returncci.p1
		  					CcWASQUIT	-> OSNoWindowPtr
		  					_			-> oswindowCreateError 1 "osCreateWindow (MDI)"
		# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) wPtr tb
		# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) wPtr tb
		= (reverse delay_info,wPtr,OSNoWindowPtr,OSNoWindowPtr,osdInfo,control_info,tb)

	| di==SDI
		# (textPtr,tb)	= winMakeCString title tb		// PA+++
		  styleFlags	= (if hasHScroll WS_HSCROLL 0) bitor (if hasVScroll WS_VSCROLL 0)
		  createcci		= Rq6Cci CcRqCREATESDIDOCWINDOW textPtr osFrame (x<<16+(y bitand 0xffff)) w h styleFlags
		# (returncci,(control_info,delay_info),tb)
						= issueCleanRequest (osCreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[]) tb
		# tb			= winReleaseCString textPtr tb	// PA+++
		  clientPtr		= case returncci.ccMsg of
		  					CcRETURN1	-> returncci.p1
		  					CcWASQUIT	-> OSNoWindowPtr
		  					_			-> oswindowCreateError 1 "osCreateWindow (SDI)"
		  osdInfo		= setOSDInfoOSInfo {osinfo & osClient=clientPtr} osdInfo
		# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) clientPtr tb
		# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) clientPtr tb
	//	# tb			= osSetWindowTitle osFrame title tb
		= (reverse delay_info,clientPtr,OSNoWindowPtr,OSNoWindowPtr,osdInfo,control_info,tb)
	
	| otherwise
		= oswindowFatalError "osCreateWindow" "unexpected OSDInfo (OSNoInfo) argument"
where
	(x,y)			= pos	// packed into one 32-bit integer
	(w,h)			= size
	di				= getOSDInfoDocumentInterface osdInfo
	osinfo			= fromJust (getOSDInfoOSInfo  osdInfo)
	osFrame			= osinfo.osFrame

osCreateWindowCallback :: !Bool !(!Int,!Int) !(!Int,!Int) 
						  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
						  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
						  !CrossCallInfo !*(u:s,[DelayActivationInfo]) !*OSToolbox
					  -> (!CrossCallInfo,!*(u:s,[DelayActivationInfo]),!*OSToolbox)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmPAINT,p1=hwnd} s tb
	= //trace "osCreateWindowCallback CcWmPAINT"
	  (return0Cci, s, winFakePaint hwnd tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmACTIVATE,p1=hwnd} (control_info,delay_info) tb
	= //trace "osCreateWindowCallback CcWmACTIVATE" 
	  (return0Cci, (control_info,[DelayActivatedWindow hwnd:delay_info]), tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmDEACTIVATE,p1=hwnd} (control_info,delay_info) tb
	= //trace "osCreateWindowCallback CcWmDEACTIVATE" 
	  (return0Cci, (control_info,[DelayDeactivatedWindow hwnd:delay_info]), tb)
osCreateWindowCallback _ _ _ create_controls _ {ccMsg=CcWmCREATE,p1=hwnd} (control_info,deactivates) tb
	# (control_info,tb)			= create_controls hwnd control_info tb
	= (return0Cci, (control_info,deactivates), tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmNEWHTHUMB,p1=hwnd,p2=thumb} s tb
	= //trace "osCreateWindowCallback CcWmNEWHTHUMB" 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmNEWVTHUMB,p1=hwnd,p2=thumb} s tb
	= //trace "osCreateWindowCallback CcWmNEWVTHUMB" 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmSIZE,p1=hwnd,p2=width,p3=height} s tb
	= //trace ("osCreateWindowCallback CcWmSIZE "+++toString (width,height)) 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ update_controls {ccMsg=CcWmDRAWCONTROL,p1=hwnd,p2=hctrl,p3=hdc} (control_info,delay_info) tb
	# (control_info,tb)			= update_controls hwnd hctrl hdc control_info tb
	= //trace ("osCreateWindowCallback CcWmDRAWCONTROL "+++toString (hwnd,hctrl,hdc))
	  (return0Cci, (control_info,delay_info), tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmKILLFOCUS} s tb
	= //trace "osCreateWindowCallback CcWmKILLFOCUS" 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmKEYBOARD,p1=hwnd,p2=hctrl,p3=char,p4=ks,p5=mods} s tb
	= //trace ("osCreateWindowCallback CcWmKEYBOARD "+++toString (hwnd,hctrl,char,ks,mods))
	  (return0Cci, s,tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg} s tb
	= oswindowFatalError "osCreateWindowCallback" ("unknown message type ("+++toString ccMsg+++")")


/*	osCreateModalDialog wMetrics isCloseable title osdocinfo currentModal size 
						dialogControls dialogInit handleOSEvents
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

osCreateModalDialog ::	!OSWindowMetrics !Bool !String !OSDInfo !(Maybe OSWindowPtr) !(!Int,!Int) 
						!(OSWindowPtr u:s -> u:s)
						!(OSWindowPtr u:s -> u:s)
						!(OSModalEventHandling u:s)
						!(!u:s -> *(*OSToolbox,u:s), !*OSToolbox -> *(u:s -> u:s))
						!u:s
			  -> (!Bool,!u:s)
osCreateModalDialog wMetrics isClosable title osdinfo currentActiveModal size 
					dialogControls	// evaluated iff not osModalDialogHandlesControlCreation
					dialogInit		// evaluated iff not osModalDialogHandlesWindowInit
					(OSModalEventCallback getOSEvents setOSEvents handleOSEvents)
					(getOSToolbox,setOSToolbox)
					s
	# (tb,s)			= getOSToolbox s
	# (textPtr,tb)		= winMakeCString title tb
	  createcci			= Rq2Cci CcRqCREATEMODALDIALOG textPtr parentptr
	# (returncci,s,tb)	= issueCleanRequest (osCreateModalDialogCallback getOSEvents setOSEvents handleOSEvents) createcci s tb
	# tb				= winReleaseCString textPtr tb
	  ok				= case returncci.ccMsg of
	  						CcRETURN1	-> returncci.p1==0
	  						CcWASQUIT	-> True
		  					_			-> oswindowCreateError 1 "osCreateModalDialog"
	# s					= setOSToolbox tb s
	= (ok,s)
where
	parentptr			= case currentActiveModal of
							Just wPtr -> wPtr
							nothing   -> case getOSDInfoOSInfo osdinfo of
											Just info -> info.osFrame
											nothing   -> 0
	
	osCreateModalDialogCallback :: !(u:s -> (OSEvents,u:s)) !((OSEvents,u:s)-> u:s) !(OSEvent -> u:s -> *([Int],u:s))
									!CrossCallInfo !u:s !*OSToolbox
								-> (!CrossCallInfo,!u:s,!*OSToolbox)
	osCreateModalDialogCallback getOSEvents setOSEvents handleOSEvents osEvent s tb
		# (replyToOS,s)				= handleOSEvents osEvent s
		| not (isEmpty replyToOS)	// information must be returned to OS
			= (setReplyInOSEvent replyToOS,s,tb)
		# (osEvents, s)				= getOSEvents s
		# (noDelayEvents,osEvents)	= osIsEmptyEvents osEvents
		| noDelayEvents
			= (setReplyInOSEvent replyToOS,setOSEvents (osEvents,s),tb)
		| otherwise
			# (osEvent,osEvents)	= osRemoveEvent osEvents
			# s						= setOSEvents (osEvents,s)
			= osCreateModalDialogCallback getOSEvents setOSEvents handleOSEvents osEvent s tb
osCreateModalDialog _ _ _ _ _ _ _ _ (OSModalEventLoop _) _ _
	= oswindowFatalError "osCreateModalDialog" "OSModalEventCallback argument expected instead of OSModalEventLoop"


/*	Control creation functions.
*/
oswindowCreateError :: Int String -> .x
oswindowCreateError arity function
	= oswindowFatalError function ("Expected CcRETURN"+++toString arity+++" value.\n")

osIgnoreCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osIgnoreCallback ccinfo=:{ccMsg=CcWmPAINT,p1=hwnd} tb
	= (return0Cci,winFakePaint hwnd tb)//WinEndPaint hwnd (WinBeginPaint hwnd tb))
osIgnoreCallback ccinfo tb 
	= (return0Cci,tb)

osIgnoreCallback` :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
osIgnoreCallback` {ccMsg=CcWmPAINT,p1=hwnd} s tb
	= (return0Cci,s,winFakePaint hwnd tb)//WinEndPaint hwnd (WinBeginPaint hwnd tb))
osIgnoreCallback` {ccMsg=CcWmACTIVATE,p1=hwnd} delayinfo tb
	= (return0Cci,[DelayActivatedWindow hwnd:delayinfo],tb)
osIgnoreCallback` {ccMsg=CcWmDEACTIVATE,p1=hwnd} delayinfo tb
	= (return0Cci,[DelayDeactivatedWindow hwnd:delayinfo],tb)
osIgnoreCallback` _ s tb
	= (return0Cci,s,tb)

/*	OKorCANCEL type is used to tell Windows that a (Custom)ButtonControl is 
	the OK, CANCEL, or normal button.
*/
::	OKorCANCEL
	=	OK | CANCEL | NORMAL

instance toInt OKorCANCEL where
	toInt OK     = ISOKBUTTON
	toInt CANCEL = ISCANCELBUTTON
	toInt NORMAL = ISNORMALBUTTON
instance toString OKorCANCEL where
	toString OK     = "OK"
	toString CANCEL = "CANCEL"
	toString NORMAL = "NORMAL"

osCreateRadioControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																						   -> (!OSWindowPtr,!*OSToolbox)
osCreateRadioControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATERADIOBUT parentWindow x y w h (toInt isfirst)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  radioPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateRadioControl"
	# tb			= winSetWindowTitle radioPtr title tb
	# tb			= winCheckControl   radioPtr selected tb
	# tb			= winEnableControl  radioPtr able tb
	# tb			= winShowControl	radioPtr show tb
	= (radioPtr,tb)

osCreateCheckControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																						   -> (!OSWindowPtr,!*OSToolbox)
osCreateCheckControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATECHECKBOX parentWindow x y w h (toInt isfirst)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  checkPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCheckControl"
	# tb			= winSetWindowTitle checkPtr title tb
	# tb			= winCheckControl   checkPtr selected tb
	# tb			= winEnableControl  checkPtr able tb
	# tb			= winShowControl	checkPtr show tb
	= (checkPtr,tb)

MaxComboboxWidth		:== 65535		// System maximum for width  of combo box
MaxComboboxHeight		:==	65535		// System maximum for height of combo box
MaxComboElementsVisible	:==	15			// If there are <=MaxComboElementsVisible then show all elements
MaxComboElementsScroll	:==	12			// otherwise, show MaxComboElementsScroll elements

osCreateEmptyPopUpControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Int !Bool !*OSToolbox
	-> (!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
osCreateEmptyPopUpControl parentWindow /*stackBehind*/ parentPos show able (x,y) (w,h) nrItems isEditable tb
	# (x,y)				= (x-fst parentPos,y-snd parentPos)
	# (screenRect,tb)	= osScreenrect tb
	# (wMetrics,tb)		= osDefaultWindowMetrics tb
	  screenSize		= rectSize screenRect
	  height			= wMetrics.osmHeight
	  okNrItems			= if (nrItems<=MaxComboElementsVisible) nrItems MaxComboElementsScroll
	  overall_h			= min screenSize.h (min MaxComboboxHeight (h + (okNrItems+1)*(height+2)))
	  overall_w			= min screenSize.w (min MaxComboboxWidth w)
	  createcci			= Rq6Cci CcRqCREATEPOPUP parentWindow x y overall_w overall_h (toInt isEditable)
	# (returncci,tb)	= issueCleanRequest2 osIgnoreCallback createcci tb
	  (popUpPtr,editPtr)= case returncci.ccMsg of
							CcRETURN2	-> (returncci.p1, returncci.p2)
							CcWASQUIT	-> (OSNoWindowPtr,OSNoWindowPtr)
							_			-> oswindowCreateError 2 "osCreateEmptyPopUpControl"
	# tb				= winEnableControl popUpPtr able tb
	# tb				= winShowControl   popUpPtr show tb
//	# (_,_,tb)			= osStackWindow    popUpPtr stackBehind k` 0 tb		PA: parameter not passed anymore
						// PA: for control delayinfo can be ignored (this call has been moved from controlinternal to oswindow to ensure the control
						//		is placed at the proper stacking order.)
	= (popUpPtr,editPtr,tb)

osCreatePopUpControlItem :: !OSWindowPtr !(Maybe OSWindowPtr) !Int !Bool !String !Bool !Int !*OSToolbox -> (!Int,!*OSToolbox)
osCreatePopUpControlItem parentPopUp _ pos able title selected _ tb
	# (textPtr,tb)	= winMakeCString title tb
	  addcci		= Rq5Cci CcRqADDTOPOPUP parentPopUp textPtr (toInt able) (toInt selected) pos
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback addcci tb
	# tb			= winReleaseCString textPtr tb
	  index			= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> 0
						_			-> oswindowCreateError 1 "osCreatePopUpControlItem"
	= (index,tb)

osCreatePopUpControlItems :: !OSWindowPtr !(Maybe OSWindowPtr) !Bool ![String] !Int !*OSToolbox -> *OSToolbox
osCreatePopUpControlItems newPopUpPtr maybeEditPtr ableContext newItems newIndex tb
	# (_,tb)			= stateMap2 (appendPopUp newPopUpPtr maybeEditPtr newIndex) newItems (1,tb)
	= tb
where
	appendPopUp :: !OSWindowPtr !(Maybe OSWindowPtr) !Index !String !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
	appendPopUp popUpPtr editPtr index title (itemNr,tb)
		# (_,tb)			= osCreatePopUpControlItem popUpPtr editPtr (-1) ableContext title (index==itemNr) itemNr tb
		= (itemNr+1,tb)

osCreateSliderControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !(!Int,!Int,!Int,!Int) !*OSToolbox
																									 -> (!OSWindowPtr,!*OSToolbox)
osCreateSliderControl parentWindow parentPos show able horizontal (x,y) (w,h) (min,thumb,max,thumbSize) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATESCROLLBAR parentWindow x y w h (toInt horizontal)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  sliderPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateSliderControl"
	# tb			= winSetScrollRange sliderPtr SB_CTL min max False tb
	# tb			= winSetScrollPos   sliderPtr SB_CTL thumb (x+w) (y+h) (if horizontal h w) tb
	# tb			= winSetScrollThumbSize sliderPtr SB_CTL thumbSize 0 0 0 tb		// PA: hint by Diederik to add this code to solve Maarten's bug
	# tb			= winEnableControl  sliderPtr able tb
	# tb			= winShowControl	sliderPtr show tb
	= (sliderPtr,tb)

osCreateTextControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateTextControl parentWindow parentPos text show (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq5Cci CcRqCREATESTATICTXT parentWindow x y w h
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  textPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateTextControl"
	# tb			= winSetWindowTitle textPtr text tb
	# tb			= winShowControl	textPtr show tb
	= (textPtr,tb)

osCreateEditControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateEditControl parentWindow parentPos text show able isKeySensitive (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	# (wMetrics,tb)	= osDefaultWindowMetrics tb
	  nrLines		= (h-wMetrics.osmHeight)/wMetrics.osmHeight		//toInt ((toReal h) / (1.5*(toReal wMetrics.osmHeight)))
	  isMultiLine	= nrLines>1
	  editflags		= (if isMultiLine EDITISMULTILINE 0) + (if isKeySensitive EDITISKEYSENSITIVE 0)
	  createcci		= Rq6Cci CcRqCREATEEDITTXT parentWindow x y w h editflags
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  editPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateEditControl"
	# tb			= winSetWindowTitle editPtr text tb
	# tb			= winEnableControl	editPtr able tb
	# tb			= winShowControl	editPtr show tb
	= (editPtr,tb)

osCreateButtonControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateButtonControl parentWindow parentPos title show able (x,y) (w,h) okOrCancel tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	# createcci		= Rq6Cci CcRqCREATEBUTTON parentWindow x y w h (toInt okOrCancel)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  buttonPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateButtonControl"
	# tb			= winSetWindowTitle buttonPtr title tb
	# tb			= winEnableControl  buttonPtr able tb
	# tb			= winShowControl	buttonPtr show tb
	= (buttonPtr,tb)

osCreateCustomButtonControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateCustomButtonControl parentWindow parentPos show able (x,y) (w,h) okOrCancel tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATEICONBUT parentWindow x y w h (toInt okOrCancel)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  buttonPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCustomButtonControl"
	# tb			= winEnableControl	buttonPtr able tb
	# tb			= winShowControl	buttonPtr show tb
	= (buttonPtr,tb)

osCreateCustomControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateCustomControl parentWindow parentPos show able (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq5Cci CcRqCREATECUSTOM parentWindow x y w h
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  customPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCustomControl"
	# tb			= winEnableControl	customPtr able tb
	# tb			= winShowControl	customPtr show tb
	= (customPtr,tb)

::	ScrollbarInfo
	=	{	cbiHasScroll	:: !Bool				// The scrollbar exists
		,	cbiPos			:: (Int,Int)			// Its position within the parent
		,	cbiSize			:: (Int,Int)			// Its size within the parent
		,	cbiState		:: (Int,Int,Int,Int)	// Its (min,thumb,max,thumbsize) settings
		}

osCreateCompoundControl ::  !OSWindowMetrics !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int)
							!ScrollbarInfo
							!ScrollbarInfo
							!*OSToolbox
						 -> (!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
osCreateCompoundControl wMetrics parentWindow parentPos show able isTransparent (x,y) (w,h)
						hInfo=:{cbiHasScroll=hasHScroll}
						vInfo=:{cbiHasScroll=hasVScroll} tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  scrollFlags	= (if hasHScroll WS_HSCROLL 0) bitor (if hasVScroll WS_VSCROLL 0)
	  createcci		= Rq6Cci CcRqCREATECOMPOUND parentWindow (x<<16+(y bitand 0xffff)) w h scrollFlags (toInt isTransparent)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  compoundPtr	= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCompoundControl"
	# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) compoundPtr tb
	# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) compoundPtr tb
	# tb			= winSetSelectStateWindow	compoundPtr (hasHScroll,hasVScroll) able False tb
	# tb			= winShowControl			compoundPtr show tb
	= (compoundPtr,OSNoWindowPtr,OSNoWindowPtr,tb)

setScrollRangeAndPos :: !Bool Bool OSWindowMetrics Int (Int,Int,Int,Int) (Int,Int) OSWindowPtr !*OSToolbox -> *OSToolbox
setScrollRangeAndPos hasScroll redraw wMetrics iBar state maxcoords wPtr tb
	| not hasScroll
		= tb
	# tb	= winSetScrollRange     wPtr iBar min max   False tb
	# tb	= winSetScrollPos       wPtr iBar thumb     0 0 0 tb
	| redraw
		= winSetScrollThumbSize wPtr iBar thumbsize maxx maxy extent tb
	| otherwise
		= winSetScrollThumbSize wPtr iBar thumbsize 0 0 0 tb
where
	(min,thumb,max,thumbsize)	= state
	(maxx,maxy)					= maxcoords
	horizontal					= iBar==SB_HORZ
	extent						= if horizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth


/*	Window destruction operations.
	PA: osDestroyWindow checks the process document interface and applies the appropriate destruction operation.
*/
/*	PA: previous implementation of osDestroyWindow without update handling.
osDestroyWindow :: !OSDInfo !Bool !Bool !OSWindowPtr !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osDestroyWindow (OSMDInfo {osmdFrame,osmdClient}) isModal isWindow wPtr tb
	# (_,delayInfo,tb)	= issueCleanRequest osDelayCallback destroycci [] tb
	= (reverse delayInfo,tb)
where
	destroycci	= if isWindow (Rq3Cci CcRqDESTROYMDIDOCWINDOW osmdFrame osmdClient wPtr)
				 (if isModal  (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							  (Rq1Cci CcRqDESTROYWINDOW wPtr))
osDestroyWindow (OSSDInfo _) isModal isWindow wPtr tb
	# (_,delayInfo,tb)	= issueCleanRequest osDelayCallback destroycci [] tb//(Rq1Cci CcRqDESTROYWINDOW wPtr) [] tb
	= (reverse delayInfo,tb)
where
	destroycci	= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							 (Rq1Cci CcRqDESTROYWINDOW wPtr)
osDestroyWindow OSNoInfo isModal isWindow wPtr tb
	| isWindow		/* This condition should never occur (NDI processes have only dialogues). */
		= oswindowFatalError "osDestroyWindow" "trying to destroy window of NDI process"
	| otherwise
		# (_,delayInfo,tb)= issueCleanRequest osDelayCallback destroycci [] tb//(Rq1Cci CcRqDESTROYWINDOW wPtr) [] tb
		= (reverse delayInfo,tb)
where
	destroycci	= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							 (Rq1Cci CcRqDESTROYWINDOW wPtr)

osDelayCallback :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox
			   -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
osDelayCallback {ccMsg=CcWmPAINT,p1=wPtr} s tb
	= (return0Cci,s,winFakePaint wPtr tb)
osDelayCallback {ccMsg=CcWmACTIVATE,p1=wPtr} delayinfo tb
	= (return0Cci,[DelayActivatedWindow wPtr:delayinfo],tb)
osDelayCallback {ccMsg=CcWmDEACTIVATE,p1=wPtr} delayinfo tb
	= (return0Cci,[DelayDeactivatedWindow wPtr:delayinfo],tb)
osDelayCallback {ccMsg} s tb
	| expected	= (return0Cci,s,tb)
	| otherwise	= oswindowFatalError "osDelayCallback" ("unexpected delay message "+++toString ccMsg)
where
	expected	= case ccMsg of
					CcWmCLOSE			-> True
					CcWmDRAWCONTROL		-> True
					CcWmIDLETIMER		-> True
					CcWmKEYBOARD		-> True
					CcWmKILLFOCUS		-> True
					CcWmMOUSE			-> True
					CcWmSETFOCUS		-> True
					CcWmSIZE			-> True
					_					-> False
*/
/*	PA: OSDInfo is now also returned by osDestroyWindow. 
		By - personal - convention its argument position has been moved to the end of osDestroyWindow.
		Note: on Windows platform OSDInfo is not modified.
*/
osDestroyWindow :: !Bool !Bool !OSWindowPtr !(OSEvent -> .s -> ([Int],.s)) !OSDInfo !.s !*OSToolbox
												-> (![DelayActivationInfo],!OSDInfo,.s,!*OSToolbox)
osDestroyWindow isModal isWindow wPtr handleOSEvent osdInfo state tb
	| di==MDI
		# destroycci				= if isWindow (Rq3Cci CcRqDESTROYMDIDOCWINDOW osFrame osClient wPtr)
									 (if isModal  (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												  (Rq1Cci CcRqDESTROYWINDOW wPtr))
		# (_,(delayInfo,state),tb)	= issueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,osdInfo,state,tb)
	| di==SDI
		# destroycci				= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												 (Rq1Cci CcRqDESTROYWINDOW wPtr)
		# (_,(delayInfo,state),tb)	= issueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,osdInfo,state,tb)
	// It's a NDI process
	| isWindow		/* This condition should never occur (NDI processes have only dialogues). */
		= oswindowFatalError "osDestroyWindow" "trying to destroy window of NDI process"
	| otherwise
		# destroycci				= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												 (Rq1Cci CcRqDESTROYWINDOW wPtr)
		# (_,(delayInfo,state),tb)	= issueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,osdInfo,state,tb)
where
	di								= getOSDInfoDocumentInterface osdInfo
	{osFrame,osClient}				= fromJust (getOSDInfoOSInfo  osdInfo)

osDelayCallback :: !(OSEvent -> .s -> ([Int],.s)) !CrossCallInfo !(![DelayActivationInfo],.s) !*OSToolbox
											  -> (!CrossCallInfo,!(![DelayActivationInfo],.s),!*OSToolbox)
osDelayCallback handleOSEvent osEvent=:{ccMsg} (delayinfo,s) tb
	| toBeHandled
		# (replyToOS,s)	= handleOSEvent osEvent s
		= (setReplyInOSEvent replyToOS,(delayinfo,s),tb)
	| ccMsg==CcWmACTIVATE
		= (return0Cci,([DelayActivatedWindow osEvent.p1:delayinfo],s),tb)
	| ccMsg==CcWmDEACTIVATE
		= (return0Cci,([DelayDeactivatedWindow osEvent.p1:delayinfo],s),tb)
	| toBeSkipped
		= (return0Cci,(delayinfo,s),tb)
	| otherwise
		= oswindowFatalError "osDelayCallback" ("unexpected delay message "+++toString ccMsg)
where
	toBeHandled	= case ccMsg of
					CcWmPAINT			-> True
					CcWmDRAWCONTROL		-> True
					CcWmKEYBOARD		-> True
					CcWmKILLFOCUS		-> True
					CcWmMOUSE			-> True
					CcWmSETFOCUS		-> True
					other				-> False
	toBeSkipped	= case ccMsg of
					CcWmCLOSE			-> True
					CcWmIDLETIMER		-> True
					CcWmSIZE			-> True
					other				-> False


/*	Control destruction operations.
*/
destroycontrol :: !OSWindowPtr !*OSToolbox -> *OSToolbox
destroycontrol wPtr tb
	= snd (issueCleanRequest2 osDestroyControlCallback (Rq1Cci CcRqDESTROYWINDOW wPtr) tb)
where
	osDestroyControlCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
	osDestroyControlCallback info=:{ccMsg} tb
		| ccMsg==CcWmPAINT
			= (return0Cci,winFakePaint info.p1 tb)//WinEndPaint info.p1 (WinBeginPaint info.p1 tb))
		| expected
			= (return0Cci,tb)
		| otherwise
			= oswindowFatalError "osDestroyControlCallback" ("unexpected message "+++toString ccMsg)
	where
		expected	= case ccMsg of
						CcWmACTIVATE		-> True
						CcWmBUTTONCLICKED	-> True
						CcWmCOMBOSELECT		-> True
						CcWmCOMMAND			-> True
						CcWmDEACTIVATE		-> True
						CcWmDRAWCONTROL		-> True
						CcWmIDLETIMER		-> True
						CcWmKEYBOARD		-> True
						CcWmKILLFOCUS		-> True
						CcWmSETFOCUS		-> True
						_					-> False

osDestroyRadioControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyRadioControl wPtr tb = destroycontrol wPtr tb

osDestroyCheckControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCheckControl wPtr tb = destroycontrol wPtr tb

osDestroyPopUpControl :: !OSWindowPtr !(Maybe OSWindowPtr) !*OSToolbox -> *OSToolbox
osDestroyPopUpControl wPtr _ tb = destroycontrol wPtr tb

osDestroySliderControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroySliderControl wPtr tb = destroycontrol wPtr tb

osDestroyTextControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyTextControl wPtr tb = destroycontrol wPtr tb

osDestroyEditControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyEditControl wPtr tb = destroycontrol wPtr tb

osDestroyButtonControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyButtonControl wPtr tb = destroycontrol wPtr tb

osDestroyCustomButtonControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCustomButtonControl wPtr tb = destroycontrol wPtr tb

osDestroyCustomControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCustomControl wPtr tb = destroycontrol wPtr tb

osDestroyCompoundControl :: !OSWindowPtr !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCompoundControl wPtr _ _ tb = destroycontrol wPtr tb


/*	Control update operations.
*/
osUpdateRadioControl :: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateRadioControl area pos parentWindow theControl tb = updatecontrol theControl (subVector (fromTuple pos) area) tb

osUpdateCheckControl :: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateCheckControl area pos parentWindow theControl tb = updatecontrol theControl (subVector (fromTuple pos) area) tb

osUpdatePopUpControl :: !OSRect !OSWindowPtr !OSWindowPtr !(Maybe OSWindowPtr) !(!Int,!Int) !(!Int,!Int) !Bool !String !*OSToolbox -> *OSToolbox
osUpdatePopUpControl area parentWindow theControl editControl pos size select text tb
	= updatecontrol theControl (subVector (fromTuple pos) area) tb

osUpdateSliderControl :: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateSliderControl area pos parentWindow theControl tb = updatecontrol theControl (subVector (fromTuple pos) area) tb

//OSupdateTextControl :: !OSRect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
//OSupdateTextControl area parentWindow theControl tb = updatecontrol theControl area tb
osUpdateTextControl :: !OSRect !OSRect !String !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateTextControl area _ _ pos parentWindow theControl tb
	= updatecontrol theControl (subVector (fromTuple pos) area) tb

//OSupdateEditControl :: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
//OSupdateEditControl area pos parentWindow theControl tb = updatecontrol theControl (subVector (fromTuple pos) area) tb
osUpdateEditControl :: !OSRect !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateEditControl area _ pos parentWindow theControl tb
	= updatecontrol theControl (subVector (fromTuple pos) area) tb

//OSupdateButtonControl :: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
//OSupdateButtonControl area pos parentWindow theControl tb = updatecontrol theControl (subVector (fromTuple pos) area) tb
osUpdateButtonControl :: !OSRect !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateButtonControl area _ pos parentWindow theControl tb
	= updatecontrol theControl (subVector (fromTuple pos) area) tb

//OSupdateCompoundControl :: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
//OSupdateCompoundControl area pos parentWindow theControl tb = updatecontrol theControl (subVector (fromTuple pos) area) tb
osUpdateCompoundControl :: !OSRect !(!Int,!Int) !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateCompoundControl area pos parentWindow theControl tb
	= updatecontrol theControl (subVector (fromTuple pos) area) tb

updatecontrol :: !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox
updatecontrol theControl rect tb = winUpdateWindowRect theControl (toTuple4 rect) tb


/*	Control clipping operations.
*/
oscliprectrgn :: !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
oscliprectrgn parent_pos=:(parent_x,parent_y) rect (x,y) (w,h) tb
	= osnewrectrgn (intersectRects area item) tb
where
	area	= subVector (fromTuple parent_pos) rect
	x`		= x-parent_x
	y`		= y-parent_y
	item	= {rleft=x`,rtop=y`,rright=x`+w,rbottom=y`+h}

osClipRadioControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipRadioControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCheckControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCheckControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipPopUpControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipPopUpControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipSliderControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipSliderControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipTextControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipTextControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipEditControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipEditControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipButtonControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipButtonControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCustomButtonControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCustomButtonControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCustomControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCustomControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCompoundControl :: !OSWindowPtr !(!Int,!Int) !OSRect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCompoundControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

/*	Window graphics context access operations.
*/
osGrabWindowPictContext :: !OSWindowPtr !*OSToolbox -> (!OSPictContext,!*OSToolbox)
osGrabWindowPictContext wPtr tb
	= winGetDC wPtr tb

osReleaseWindowPictContext :: !OSWindowPtr !OSPictContext !*OSToolbox -> *OSToolbox
osReleaseWindowPictContext wPtr hdc tb
	= winReleaseDC wPtr (hdc,tb)


/*	osBeginUpdate theWindow
		makes additional preparations to do updates. Dummy on Windows.
	osEndUpdate theWindow
		administrates and ends the update. Dummy on Windows.
*/
osBeginUpdate :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osBeginUpdate _ tb = tb

osEndUpdate :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osEndUpdate _ tb = tb

osSetUpdate :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osSetUpdate _ tb = tb


/*	(acc/app)Grafport theWindow f
		applies f to the graphics context of theWindow (dummy on Windows).
	(acc/app)Clipport theWindow clipRect f
		applies f to the graphics context of theWindow while clipping clipRect (dummy on Windows).
*/
accGrafport :: !OSWindowPtr !.(St *OSToolbox .x) !*OSToolbox -> (!.x, !*OSToolbox)
accGrafport _ f tb = f tb

appGrafport :: !OSWindowPtr !.(*OSToolbox -> *OSToolbox) !*OSToolbox -> *OSToolbox
appGrafport _ f tb = f tb

accClipport :: !OSWindowPtr !OSRect !.(St *OSToolbox .x) !*OSToolbox -> (!.x, !*OSToolbox)
accClipport _ _ f tb = f tb

appClipport :: !OSWindowPtr !OSRect !.(*OSToolbox -> *OSToolbox) !*OSToolbox -> *OSToolbox
appClipport _ _ f tb = f tb


/*	Window access operations.
*/
toOSscrollbarRange :: !(!Int,!Int,!Int) !Int -> (!Int,!Int,!Int,!Int)
toOSscrollbarRange (domainMin,viewMin,domainMax) viewSize
	= (osRangeMin,osThumb,osRangeMax,osThumbSize+1)
where
	(osRangeMin,osRangeMax)	= toOSRange (domainMin,domainMax)
	range					=  domainMax- domainMin
	osRange					= osRangeMax-osRangeMin
	osThumb					= inRange osRangeMin osRange (viewMin-domainMin) range
	osThumbSize				= if (viewSize>=range) osRange (toInt (((toReal viewSize)/(toReal range))*(toReal osRange)))

fromOSscrollbarRange :: !(!Int,!Int) !Int -> Int
fromOSscrollbarRange (domainMin,domainMax) osThumb
	= inRange domainMin range (osThumb-osRangeMin) osRange
where
	(osRangeMin,osRangeMax)	= toOSRange (domainMin,domainMax)
	range					=  domainMax- domainMin
	osRange					= osRangeMax-osRangeMin

osScrollbarIsVisible :: !(!Int,!Int) !Int -> Bool
osScrollbarIsVisible (domainMin,domainMax) viewSize
	= viewSize<domainMax-domainMin

osScrollbarsAreVisible :: !OSWindowMetrics !OSRect !(!Int,!Int) !(!Bool,!Bool) -> (!Bool,!Bool)
osScrollbarsAreVisible {osmHSliderHeight,osmVSliderWidth} {rleft=xMin,rtop=yMin,rright=xMax,rbottom=yMax} (width,height) (hasHScroll,hasVScroll)
	= visScrollbars (False,False)
					(hasHScroll && (osScrollbarIsVisible hRange width),hasVScroll && (osScrollbarIsVisible vRange height))
where
	hRange	= (xMin,xMax)
	vRange	= (yMin,yMax)
	
	visScrollbars :: !(!Bool,!Bool) !(!Bool,!Bool) -> (!Bool,!Bool)
	visScrollbars (showH1,showV1) (showH2,showV2)
		| showH1==showH2 && showV1==showV2
			= (showH1,showV1)
		| otherwise
			= visScrollbars (showH2,showV2) (showH,showV)
	where
		showH	= if showV2 (hasHScroll && osScrollbarIsVisible hRange (width -osmVSliderWidth )) showH2
		showV	= if showH2 (hasVScroll && osScrollbarIsVisible vRange (height-osmHSliderHeight)) showV2

toOSRange :: !(!Int,!Int) -> (!Int,!Int)
toOSRange (min,max)
	= (OSSliderMin,if (range<=OSSliderRange) (OSSliderMin+range) OSSliderMax)
where
	range = max-min

inRange :: !Int !Int !Int !Int -> Int
inRange destMin destRange sourceValue sourceRange
	| sourceRange == 0
		= 0		// DvA: avoid obscure windows bug for ide
	| otherwise
		= destMin + (toInt (((toReal sourceValue) / (toReal sourceRange)) * (toReal destRange)))

OSSliderMin		:== 0			// 0
OSSliderMax		:== 32767		// MaxSigned2ByteInt
OSSliderRange	:== 32767		// OSSliderMax-OSSliderMin


osSetWindowSliderThumb :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(Maybe OSWindowPtr) !(Maybe OSWindowPtr) !OSRect !OSRect !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetWindowSliderThumb wMetrics theWindow isHorizontal thumb _ _ _ _ (maxx,maxy) redraw tb
	= winSetScrollPos theWindow (if isHorizontal SB_HORZ SB_VERT) thumb maxx maxy extent tb
where
	extent	= if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth

osSetWindowSliderThumbSize :: !OSWindowMetrics !OSWindowPtr !OSWindowPtr !Bool !Int !Int !Int !(!Int,!Int) !OSRect !Bool !Bool !*OSToolbox -> *OSToolbox
osSetWindowSliderThumbSize wMetrics theWindow _ isHorizontal min max size (maxx,maxy) _ _ redraw tb
	# tb	= winSetScrollRange theWindow (if isHorizontal SB_HORZ SB_VERT) min max False tb
	= winSetScrollThumbSize theWindow (if isHorizontal SB_HORZ SB_VERT) size maxx maxy extent tb
where
	extent	= if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth

//	PA: dummy function, required only for Mac (moved from \OS Macintosh\osutil) and made type independent of WindowHandle.
osSetWindowSliderPosSize :: !OSWindowPtr !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox
osSetWindowSliderPosSize _ scrollPtr possize tb = tb

osInvalidateWindow :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osInvalidateWindow theWindow tb
	= winInvalidateWindow theWindow tb

osInvalidateWindowRect :: !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox
osInvalidateWindowRect theWindow rect tb
	= winInvalidateRect theWindow (toTuple4 rect) tb

osValidateWindowRect :: !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox
osValidateWindowRect theWindow rect tb
	= winValidateRect theWindow (toTuple4 rect) tb

osValidateWindowRgn :: !OSWindowPtr !OSRgnHandle !*OSToolbox -> *OSToolbox
osValidateWindowRgn theWindow rgn tb
	= winValidateRgn theWindow rgn tb

osWindowHasUpdateRect :: !OSWindowPtr !*OSToolbox -> (!Bool,!*OSToolbox)
osWindowHasUpdateRect wPtr tb = GetUpdateRect wPtr 0 0 tb
where
	GetUpdateRect :: !Int !Int !Int !*Int -> (!Bool,!*Int)
	GetUpdateRect _ _ _ _ = code {
		ccall GetUpdateRect@12 "PIII:I:I"
		}

osDisableWindow :: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
osDisableWindow theWindow scrollInfo modalContext tb
	= winSetSelectStateWindow theWindow scrollInfo False modalContext tb

osEnableWindow :: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
osEnableWindow theWindow scrollInfo modalContext tb
	= winSetSelectStateWindow theWindow scrollInfo True modalContext tb

osActivateWindow :: !OSDInfo !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
	-> (![DelayActivationInfo],!.s,!*OSToolbox)
osActivateWindow osdInfo thisWindow handleOSEvent state tb
	# (_,(delayinfo,state),tb)	= issueCleanRequest (osDelayActivationEventsCallback handleOSEvent) (Rq3Cci CcRqACTIVATEWINDOW (toInt isMDI) clientPtr thisWindow) ([],state) tb
	= (reverse delayinfo,state,tb)
where
	isMDI				= getOSDInfoDocumentInterface osdInfo==MDI
	clientPtr			= case (getOSDInfoOSInfo osdInfo) of
							Just {osClient} -> osClient
							nothing         -> oswindowFatalError "osActivateWindow" "illegal DocumentInterface context"

/*	osDelayActivationEventsCallback delays activate and deactivate events for windows/dialogues/controls.
	All other events are passed to the callback function.
	Note that the returned [DelayActivationInfo] is in reversed order.
	This function is also used by osActivateControl.
*/
osDelayActivationEventsCallback :: !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !CrossCallInfo !(![DelayActivationInfo],!.s) !*OSToolbox
								-> (!CrossCallInfo,!(![DelayActivationInfo],!.s),!*OSToolbox)
osDelayActivationEventsCallback handleOSEvent osEvent=:{ccMsg,p1,p2} (delayinfo,s) tb
	| isDelayEvent
		= (return0Cci,([delayEvent:delayinfo],s),tb)
	| otherwise
		# (s,tb)	= handleOSEvent osEvent (s,tb)
		= (return0Cci,(delayinfo,s),tb)
where
	(isDelayEvent,delayEvent)	= case ccMsg of
									CcWmACTIVATE   -> (True,DelayActivatedWindow    p1)
									CcWmDEACTIVATE -> (True,DelayDeactivatedWindow  p1)
									CcWmKILLFOCUS  -> (True,DelayDeactivatedControl p1 p2)
									CcWmSETFOCUS   -> (True,DelayActivatedControl   p1 p2)
									_              -> (False,undef)


osActivateControl :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osActivateControl parentWindow controlPtr tb
	# (_,delayinfo,tb)	= issueCleanRequest osIgnoreCallback` (Rq1Cci CcRqACTIVATECONTROL controlPtr) [] tb
	= (reverse delayinfo,tb)
where
	osIgnoreCallback` :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
	osIgnoreCallback` {ccMsg=CcWmPAINT,p1=hwnd} s tb
		= (return0Cci,s,winFakePaint hwnd tb)//winEndPaint hwnd (winBeginPaint hwnd tb))
	osIgnoreCallback` {ccMsg=CcWmKILLFOCUS,p1=hwnd,p2=cptr} delayinfo tb
		= (return0Cci,[DelayDeactivatedControl hwnd cptr:delayinfo],tb)
	osIgnoreCallback` {ccMsg=CcWmSETFOCUS,p1=hwnd,p2=cptr} delayinfo tb
		= (return0Cci,[DelayActivatedControl hwnd cptr:delayinfo],tb)
	osIgnoreCallback` _ s tb
		= (return0Cci,s,tb)

/*	PA: previous implementation of osStackWindow ignored window updates and resizes. This is fixed below.
osStackWindow :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osStackWindow thisWindow behindWindow tb
	= winRestackWindow thisWindow behindWindow tb

winRestackWindow :: !HWND !HWND !*OSToolbox -> *OSToolbox
winRestackWindow theWindow behindWindow tb
	= snd (issueCleanRequest2 (errorCallback2 "winRestackWindow") (Rq2Cci CcRqRESTACKWINDOW theWindow behindWindow) tb)
*/

osStackWindow :: !OSWindowPtr !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
	-> (![DelayActivationInfo],!.s,!*OSToolbox)
osStackWindow thisWindow behindWindow handleOSEvent state tb
	# (_,(delayinfo,state),tb)	= issueCleanRequest (osDelayActivationEventsCallback handleOSEvent) (Rq2Cci CcRqRESTACKWINDOW thisWindow behindWindow) ([],state) tb
	= (reverse delayinfo,state,tb)

osHideWindow :: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osHideWindow wPtr activate tb
	# (_,delayinfo,tb)	= issueCleanRequest osIgnoreCallback` (Rq3Cci CcRqSHOWWINDOW wPtr (toInt False) (toInt activate)) [] tb
	= (reverse delayinfo,tb)

osShowWindow :: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osShowWindow wPtr activate tb
	# (_,delayinfo,tb)	= issueCleanRequest osIgnoreCallback` (Rq3Cci CcRqSHOWWINDOW wPtr (toInt True) (toInt activate)) [] tb
	= (reverse delayinfo,tb)

osSetWindowCursor :: !OSWindowPtr !CursorShape !*OSToolbox -> *OSToolbox
osSetWindowCursor wPtr shape tb
	= winSetWindowCursor wPtr cursorCode tb
where
	cursorCode = toCursorCode shape
	
//	PA: moved from windowaccess.
	toCursorCode :: !CursorShape -> Int
	toCursorCode StandardCursor	= CURSARROW
	toCursorCode BusyCursor		= CURSBUSY
	toCursorCode IBeamCursor	= CURSIBEAM
	toCursorCode CrossCursor	= CURSCROSS
	toCursorCode FatCrossCursor	= CURSFATCROSS
	toCursorCode ArrowCursor	= CURSARROW
	toCursorCode HiddenCursor	= CURSHIDDEN

osGetWindowPos :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowPos wPtr tb
	= winGetWindowPos wPtr tb

osGetWindowViewFrameSize :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowViewFrameSize wPtr tb
	= winGetClientSize wPtr tb

osGetWindowSize :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowSize wPtr tb
	= winGetWindowSize wPtr tb

osSetWindowPos :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !*OSToolbox -> *OSToolbox
osSetWindowPos wPtr pos update inclScrollbars tb
	= winSetWindowPos wPtr pos update inclScrollbars tb

osSetWindowViewFrameSize :: !OSWindowPtr !(!Int,!Int) !*OSToolbox -> *OSToolbox
osSetWindowViewFrameSize wPtr size tb
	= winSetClientSize wPtr size tb

osSetWindowSize	:: !OSWindowPtr !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetWindowSize wPtr size update tb
	= winSetWindowSize wPtr size update tb

osSetWindowTitle :: !OSWindowPtr !String !*OSToolbox -> *OSToolbox
osSetWindowTitle wPtr title tb
	= winSetWindowTitle wPtr title tb


/*	Control access operations.
*/
//	On compound controls:

osInvalidateCompound :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osInvalidateCompound compoundPtr tb
	= winInvalidateWindow compoundPtr tb

/*	PA: not used
osInvalidateCompoundRect :: !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox
osInvalidateCompoundRect compoundPtr rect tb
	= winInvalidateRect compoundPtr (toTuple4 rect) tb
*/

osSetCompoundSliderThumb :: !OSWindowMetrics !OSWindowPtr !OSWindowPtr !OSWindowPtr !OSRect !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSliderThumb wMetrics _ compoundPtr _ _ isHorizontal thumb (maxx,maxy) redraw tb
	= winSetScrollPos compoundPtr (if isHorizontal SB_HORZ SB_VERT) thumb maxx` maxy` extent tb
where
	(maxx`,maxy`,extent)	= if redraw (maxx,maxy,if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth) (0,0,0)

osSetCompoundSliderThumbSize :: !OSWindowMetrics !OSWindowPtr !OSWindowPtr !OSWindowPtr !Int !Int !Int !OSRect !Bool !Bool !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSliderThumbSize wMetrics _ compoundPtr _ min max size rect isHorizontal _ redraw tb
	# tb	= winSetScrollRange compoundPtr (if isHorizontal SB_HORZ SB_VERT) min max False tb
	= winSetScrollThumbSize compoundPtr (if isHorizontal SB_HORZ SB_VERT) size maxx` maxy` extent tb
where
	(maxx`,maxy`,extent)	= if redraw (rect.rright,rect.rbottom,if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth) (0,0,0)
/*
osSetCompoundSlider :: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int) !*OSToolbox -> *OSToolbox
osSetCompoundSlider wMetrics compoundPtr isHorizontal state maxcoords tb
	= setScrollRangeAndPos True True wMetrics (if isHorizontal SB_HORZ SB_VERT) state maxcoords compoundPtr tb
*/
osSetCompoundSelect :: !OSWindowPtr !OSWindowPtr !OSRect !(!Bool,!Bool) !(!OSWindowPtr,!OSWindowPtr) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSelect _ compoundPtr _ scrollInfo _ select tb
	= winSetSelectStateWindow compoundPtr scrollInfo select False tb
//	= winEnableControl compoundPtr scrollInfo select tb

osSetCompoundShow :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCompoundShow _ compoundPtr _ _ show tb
	= winShowControl compoundPtr show tb

osSetCompoundPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundPos _ (parent_x,parent_y) compoundPtr (x,y) _ update tb
	= winSetWindowPos compoundPtr (x-parent_x,y-parent_y) update True tb

osSetCompoundSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSize _ _ compoundPtr _ size update tb
	= winSetWindowSize compoundPtr size update tb

//	PA: dummy function, required only for Mac
osUpdateCompoundScroll :: !OSWindowPtr !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox
osUpdateCompoundScroll _ _ _ tb
	= tb

osCompoundMovesControls :== True

osCompoundControlHasOrigin :== True


//	On slider controls:

osSetSliderControlThumb :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
osSetSliderControlThumb _ cPtr _ redraw (min,thumb,max,thumbsize) tb
	# tb	= winSetScrollRange cPtr SB_CTL min max False tb
	# tb	= winSetScrollThumbSize cPtr SB_CTL thumbsize 0 0 0 tb
	= winSetScrollPos cPtr SB_CTL thumb 0 0 0 tb

osSetSliderControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetSliderControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetSliderControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlPos _ (parent_x,parent_y) sliderPtr (x,y) _ update tb
	= winSetWindowPos sliderPtr (x-parent_x,y-parent_y) update False tb

osSetSliderControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlSize _ _ sliderPtr _ size update tb
	= winSetWindowSize sliderPtr size update tb


//	On radio controls:

osSetRadioControl :: !OSWindowPtr !OSWindowPtr !OSWindowPtr !OSRect !*OSToolbox -> *OSToolbox
osSetRadioControl _ current new _ tb
	= winCheckControl new True (winCheckControl current False tb)

osSetRadioControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetRadioControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetRadioControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlPos _ (parent_x,parent_y) radioPtr (x,y) _ update tb
	= winSetWindowPos radioPtr (x-parent_x,y-parent_y) update False tb

osSetRadioControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlSize _ _ radioPtr _ size update tb
	= winSetWindowSize radioPtr size update tb


//	On check controls:

osSetCheckControl :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCheckControl _ cPtr _ check tb
	= winCheckControl cPtr check tb

osSetCheckControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetCheckControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetCheckControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlPos _ (parent_x,parent_y) checkPtr (x,y) _ update tb
	= winSetWindowPos checkPtr (x-parent_x,y-parent_y) update False tb

osSetCheckControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlSize _ _ checkPtr _ size update tb
	= winSetWindowSize checkPtr size update tb


//	On pop up controls:

osSetPopUpControl :: !OSWindowPtr !OSWindowPtr !(Maybe OSWindowPtr) !OSRect !OSRect !Int !Int !String !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControl _ pPtr _ _ _ _ new _ _ tb
	= winSelectPopupItem pPtr (new-1) tb

osSetPopUpControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlSelect _ pPtr _ select tb
	= winEnableControl pPtr select tb

osSetPopUpControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlShow _ pPtr _ show tb
	= winShowControl pPtr show tb

osSetPopUpControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlPos _ (parent_x,parent_y) popupPtr (x,y) _ update tb
	= winSetWindowPos popupPtr (x-parent_x,y-parent_y) update False tb

osSetPopUpControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlSize _ _ popupPtr _ size update tb
	= winSetWindowSize popupPtr size update tb

osGetPopUpControlText :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!String,!*OSToolbox) 
osGetPopUpControlText _ ePtr tb
	= winGetWindowText ePtr tb


//	On edit controls:

osSetEditControlText :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !String !*OSToolbox -> *OSToolbox
osSetEditControlText _ ePtr _ _ _ text tb
	= winSetWindowTitle ePtr text tb

osGetEditControlText :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!String,!*OSToolbox) 
osGetEditControlText _ ePtr tb
	= winGetWindowText ePtr tb

osSetEditControlCursor :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Int !*OSToolbox -> *OSToolbox
osSetEditControlCursor _ ePtr _ _ pos tb
	= winSetEditSelection ePtr pos (pos+1) tb

osSetEditControlSelection :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Int !Int !*OSToolbox -> *OSToolbox
osSetEditControlSelection _ ePtr _ _ start end tb
	= winSetEditSelection ePtr start end tb

osSetEditControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetEditControlSelect _ ePtr _ select tb
	= winEnableControl ePtr select tb

osSetEditControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetEditControlShow _ ePtr _ show tb
	= winShowControl ePtr show tb

osSetEditControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetEditControlPos _ (parent_x,parent_y) editPtr (x,y) _ update tb
	= winSetWindowPos editPtr (x-parent_x,y-parent_y) update False tb

osSetEditControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetEditControlSize _ _ editPtr _ size update tb
	= winSetWindowSize editPtr size update tb

//	Dummy implementation; used on Mac only (windowevent.icl):
osIdleEditControl :: !OSWindowPtr !OSRect !OSWindowPtr !*OSToolbox -> *OSToolbox
osIdleEditControl _ _ _ tb = tb


//	On text controls:

osSetTextControlText :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !String !*OSToolbox -> *OSToolbox
osSetTextControlText _ tPtr _ _ _ text tb
	= winSetWindowTitle tPtr text tb

osSetTextControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetTextControlSelect _ tPtr _ select tb
	= winEnableControl tPtr select tb

osSetTextControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !String !*OSToolbox -> *OSToolbox
osSetTextControlShow _ tPtr _ _ show _ tb
	= winShowControl tPtr show tb

osSetTextControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetTextControlPos _ (parent_x,parent_y) textPtr (x,y) _ update tb
	= winSetWindowPos textPtr (x-parent_x,y-parent_y) update False tb

osSetTextControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetTextControlSize _ _ textPtr _ size update tb
	= winSetWindowSize textPtr size update tb


//	On button controls:

osSetButtonControlText :: !OSWindowPtr !OSWindowPtr !OSRect !String !*OSToolbox -> *OSToolbox
osSetButtonControlText _ bPtr _ text tb
	= winSetWindowTitle bPtr text tb

osSetButtonControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlSelect _ bPtr _ select tb
	= winEnableControl bPtr select tb

osSetButtonControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlShow _ bPtr _ show tb
	= winShowControl bPtr show tb

osSetButtonControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlPos _ (parent_x,parent_y) buttonPtr (x,y) _ update tb
	= winSetWindowPos buttonPtr (x-parent_x,y-parent_y) update False tb

osSetButtonControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlSize _ _ buttonPtr _ size update tb
	= winSetWindowSize buttonPtr size update tb


//	On custom button controls:

osSetCustomButtonControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetCustomButtonControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlShow _ cPtr _ _ show tb
	= winShowControl cPtr show tb

osSetCustomButtonControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlPos _ (parent_x,parent_y) cPtr (x,y) _ update tb
	= winSetWindowPos cPtr (x-parent_x,y-parent_y) update False tb

osSetCustomButtonControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlSize _ _ cPtr _ size update tb
	= winSetWindowSize cPtr size update tb

osCustomButtonControlHasOrigin	:== True


//	On custom controls:

osSetCustomControlSelect :: !OSWindowPtr !OSWindowPtr !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetCustomControlShow :: !OSWindowPtr !OSWindowPtr !OSRect !OSRect !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlShow _ cPtr _ _ show tb
	= winShowControl cPtr show tb

osSetCustomControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlPos _ (parent_x,parent_y) customPtr (x,y) _ update tb
	= winSetWindowPos customPtr (x-parent_x,y-parent_y) update False tb

osSetCustomControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlSize _ _ customPtr _ size update tb
	= winSetWindowSize customPtr size update tb

osCustomControlHasOrigin :== True


//--
//	PA: copied from OS Macintosh. I suppose this is to set the global cursor?
osSetCursorShape :: !CursorShape !*OSToolbox -> *OSToolbox
osSetCursorShape _ tb = tb
