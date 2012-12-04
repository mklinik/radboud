implementation module windowdraw

import	StdBool, StdFunc, StdInt, StdList
import	ospicture, osrgn, oswindow
import	commondef, controllayout, StdPicture, windowaccess, wstate

//import StdDebug,dodebug,memory

/*	drawwindowlook wPtr includeBackground window
		applies the Look function of window.
	The wPtr argument must be the OSWindowPtr of window.
	It is assumed that window refers to a Window with a valid ClipState.
	If includeBackground is True then also the background outside the WindowViewDomain is drawn.
*/
drawwindowlook :: !OSWindowMetrics !OSWindowPtr !(IdFun *Picture) !UpdateState !(WindowHandle .ls .pst) !*OSToolbox
																			-> (!WindowHandle .ls .pst, !*OSToolbox)
drawwindowlook wMetrics wPtr drawFirst updState wH=:{whSelect,whSize,whWindowInfo} tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture origin (copyPen look.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= drawFirst picture
	#! picture				= appClipPicture (toRegion (if look.lookSysUpdate updState.updArea [wFrame])) (look.lookFun select updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osValidateWindowRgn wPtr clipRgn tb		// PA: added to eliminate update of window (in drawing part)
	#! look					= {look & lookPen=pen}
	#! info					= {info & windowLook=look}
	= ({wH & whWindowInfo=WindowInfo info},tb)
where
	select					= if whSelect Able Unable
	info					= getWindowInfoWindowData whWindowInfo
	domainRect				= info.windowDomain
	origin					= info.windowOrigin
	look					= info.windowLook
	clip					= info.windowClip
	clipRgn					= clip.clipRgn
	hasScrolls				= (isJust info.windowHScroll,isJust info.windowVScroll)
	visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	{w,h}					= rectSize (osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize))
	wFrame					= {corner1=origin,corner2={x=origin.x+w,y=origin.y+h}}

drawwindow`look :: !OSWindowMetrics !OSWindowPtr !(IdFun *Picture) !UpdateState !WindowHandle` !*OSToolbox
																			-> (!WindowHandle`,!*OSToolbox)
drawwindow`look wMetrics wPtr drawFirst updState wH=:{whSelect`,whSize`,whWindowInfo`} tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture origin (copyPen look.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= drawFirst picture
	#! picture				= appClipPicture (toRegion (if look.lookSysUpdate updState.updArea [wFrame])) (look.lookFun select updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osValidateWindowRgn wPtr clipRgn tb		// PA: added to eliminate update of window (in drawing part)
	#! look					= {look & lookPen=pen}
	#! info					= {info & windowLook=look}
	= ({wH & whWindowInfo`=WindowInfo info},tb)
where
	select					= if whSelect` Able Unable
	info					= getWindowInfoWindowData whWindowInfo`
	domainRect				= info.windowDomain
	origin					= info.windowOrigin
	look					= info.windowLook
	clip					= info.windowClip
	clipRgn					= clip.clipRgn
	hasScrolls				= (isJust info.windowHScroll,isJust info.windowVScroll)
	visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize`) hasScrolls
	{w,h}					= rectSize (osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize`))
	wFrame					= {corner1=origin,corner2={x=origin.x+w,y=origin.y+h}}

drawwindowlook` :: !OSWindowMetrics !OSWindowPtr !(St *Picture [OSRect]) !UpdateState !(WindowHandle .ls .pst) !*OSToolbox
																				   -> (!WindowHandle .ls .pst, !*OSToolbox)
drawwindowlook` wMetrics wPtr drawFirst updState wH=:{whSelect,whSize,whWindowInfo} tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture origin (copyPen look.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! (additionalUpdateArea,picture)
							= drawFirst picture
	   updState				= {updState & updArea = [rectToRectangle r \\ r<-additionalUpdateArea | not (isEmptyRect r)] ++ updState.updArea}
	#! picture				= appClipPicture (toRegion (if look.lookSysUpdate updState.updArea [wFrame])) (look.lookFun select updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osValidateWindowRgn wPtr clipRgn tb		// PA: added to eliminate update of window (in drawing part)
// DvA: removed for now to avoid disagreement with movewindowviewframe && pictscroll...
	#! look					= {look & lookPen=pen}
	#! info					= {info & windowLook=look}
	= ({wH & whWindowInfo=WindowInfo info},tb)
where
	select					= if whSelect Able Unable
	info					= getWindowInfoWindowData whWindowInfo
	domainRect				= info.windowDomain
	origin					= info.windowOrigin
	look					= info.windowLook
	clip					= info.windowClip
	clipRgn					= clip.clipRgn
	hasScrolls				= (isJust info.windowHScroll,isJust info.windowVScroll)
	visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	{w,h}					= rectSize (osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize))
	wFrame					= {corner1=origin,corner2={x=origin.x+w,y=origin.y+h}}


/*	drawinwindow wPtr drawfun window
		applies the drawing function to the picture of the window.
	The wPtr argument must be the OSWindowPtr of the window.
	It is assumed that window refers to a Window with a valid ClipState.
*/
drawinwindow :: !OSWindowMetrics !OSWindowPtr !.(St *Picture .x) !(WindowHandle .ls .pst) !*OSToolbox
														 -> (.x, ! WindowHandle .ls .pst, !*OSToolbox)
drawinwindow wMetrics wPtr drawfun wH=:{whSize,whWindowInfo} tb
//	#! (size,grow,tb)		= MaxMem tb
//	#! tb = trace_n ("drawinwindow",size,grow) tb
	#! (domainRgn,tb)		= osnewrectrgn contentRect tb
	#! (clip,tb)			= ossectrgn domainRgn clipRgn tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture origin (copyPen windowLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clip picture
	#! (x,picture)			= drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= stateMap2 osdisposergn [domainRgn,clip] tb
	#! info					= {info & windowLook={windowLook & lookPen=pen}}
	= (x,{wH & whWindowInfo=WindowInfo info},tb)
where
	info					= getWindowInfoWindowData whWindowInfo
	domainRect				= info.windowDomain
	origin					= info.windowOrigin
	windowLook				= info.windowLook
	windowClip				= info.windowClip
	clipRgn					= windowClip.clipRgn
	hasScrolls				= (isJust info.windowHScroll,isJust info.windowVScroll)
	visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	contentRect				= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)
