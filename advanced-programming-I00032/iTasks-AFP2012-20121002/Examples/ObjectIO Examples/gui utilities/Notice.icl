implementation module Notice

//	**************************************************************************************************
//
//	A new instance of the Dialogs type constructor class to easily create simple notice dialogues.
//
//	This module has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import StdMisc, StdTuple
import StdId, StdPSt, StdWindow

/*  The data type that defines a notice.
*/
::  Notice    ls pst = Notice [String] (NoticeButton *(ls,pst)) [NoticeButton *(ls,pst)]
::  NoticeButton  st = NoticeButton String (IdFun st)

/*  Notices are defined as a new instance of the Dialogs type constructor class.
*/
instance Dialogs Notice where
    openDialog ls notice pst
        # (wId, pst) = accPIO openId pst
        # (okId,pst) = accPIO openId pst
        = openDialog ls (noticeToDialog wId okId notice) pst
    
    openModalDialog ls notice pst
        # (wId,pst)  = accPIO openId pst
        # (okId,pst) = accPIO openId pst
        = openModalDialog ls (noticeToDialog wId okId notice) pst
    
    getDialogType notice
        = "Notice"

/*  A specialised version that ignores the error report.
*/
openNotice :: !(Notice .ls (PSt .l)) !(PSt .l) -> PSt .l
openNotice notice pst
    = snd (openModalDialog undef notice pst)

/*  noticeToDialog converts a Notice expression into a Dialog expression.
*/
noticeToDialog :: Id Id (Notice .ls (PSt .l)) 
               -> *Dialog (:+: (LayoutControl (ListLS TextControl))
                          (:+:  ButtonControl
                               (ListLS ButtonControl)
                          )) .ls (PSt .l)
noticeToDialog wId okId (Notice texts (NoticeButton text f) buttons)
    = Dialog ""
        (   LayoutControl 
        (   ListLS
        [   TextControl text [ControlPos (Left,zero)]
        \\  text <- texts
        ]
        )   [ControlHMargin 0 0, ControlVMargin 0 0, ControlItemSpace 3 3]
        :+: ButtonControl text 
            [ControlFunction (noticefun f), ControlPos (Right,zero), ControlId okId]
        :+: ListLS
        [   ButtonControl text [ControlFunction (noticefun f),ControlPos (LeftOfPrev,zero)]
        \\  (NoticeButton text f) <- buttons
        ]
        )
        [   WindowId    wId
        ,   WindowOk    okId
        ]
where
    noticefun f (ls,pst) = f (ls,closeWindow wId pst)
