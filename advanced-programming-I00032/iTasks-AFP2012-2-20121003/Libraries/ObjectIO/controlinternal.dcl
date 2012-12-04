definition module controlinternal


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	ossystem
import	wstate

enablecontrols			:: ![Id] !Bool               !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
disablecontrols			:: ![Id] !Bool               !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
setcontrolsshowstate	:: ![Id] !Bool               !OSWindowMetrics !WIDS        !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
setcontrolsmarkstate	:: !Id !MarkState ![Index]   !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
setcontroltexts			:: ![(Id,String)]            !OSWindowMetrics !OSWindowPtr !WindowHandle2           !*OSToolbox -> (!WindowHandle2,           !*OSToolbox)
seteditcontrolcursor	:: !Id !Int                  !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
seteditcontrolselection	:: !Id !Int !Int             !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
setcontrolslook			:: ![(Id,Bool,(Bool,Look))]  !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
drawincontrol			:: !Id !.(St *Picture .x)    !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> *(!Maybe .x,!WindowHandle`,!*OSToolbox)
setslidercontrolstates	:: ![(Id,IdFun SliderState)] !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
selectradiocontrol		:: !Id !Index                !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
selectpopupitem			:: !Id !Index                !OSWindowMetrics !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
openpopupitems			:: !Id !Index ![PopUpControlItem .pst]        !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,   !*OSToolbox)
closepopupitems			:: !Id ![Index]                               !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,   !*OSToolbox)
movecontrolviewframe	:: !Id !Vector2              !OSWindowMetrics !WIDS        !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
setcontrolviewdomain	:: !Id !ViewDomain           !OSWindowMetrics !WIDS        !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
setcontrolscrollfun		:: !Id !Direction ScrollFunction                           !WindowHandle`                       ->   WindowHandle`
setcontroloutersize		:: !Id !Size !Bool           !OSWindowMetrics !WIDS        !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
setcontrolwidth			:: !Id !ControlWidth !Bool   !OSWindowMetrics !WIDS        !WindowHandle`           !*OSToolbox -> (!WindowHandle`,           !*OSToolbox)
