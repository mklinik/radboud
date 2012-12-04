implementation module StdProcessAttribute


import StdClass
import StdProcessDef


isProcessKindAttribute :: !DocumentInterface !(ProcessAttribute .st) -> Bool 
isProcessKindAttribute di (ProcessActivate         _) = True
isProcessKindAttribute di (ProcessClipboardChanged _) = di<>NDI
isProcessKindAttribute di (ProcessClose            _) = True
isProcessKindAttribute di (ProcessDeactivate       _) = True
isProcessKindAttribute di  ProcessNoWindowMenu	      = di==MDI
isProcessKindAttribute di (ProcessOpenFiles        _) = di<>NDI
isProcessKindAttribute di (ProcessToolbar          _) = di<>NDI
isProcessKindAttribute di (ProcessWindowPos        _) = di<>NDI
isProcessKindAttribute di (ProcessWindowResize     _) = di<>NDI
isProcessKindAttribute di (ProcessWindowSize       _) = di<>NDI


isProcessActivate :: !(ProcessAttribute .st) -> Bool
isProcessActivate (ProcessActivate _) = True
isProcessActivate _ = False

isProcessClipboardChanged :: !(ProcessAttribute .st) -> Bool
isProcessClipboardChanged (ProcessClipboardChanged _) = True
isProcessClipboardChanged _ = False

isProcessClose :: !(ProcessAttribute .st) -> Bool
isProcessClose (ProcessClose _) = True
isProcessClose _ = False

isProcessDeactivate :: !(ProcessAttribute .st) -> Bool
isProcessDeactivate (ProcessDeactivate _) = True
isProcessDeactivate _ = False

isProcessNoWindowMenu :: !(ProcessAttribute .st) -> Bool
isProcessNoWindowMenu ProcessNoWindowMenu = True
isProcessNoWindowMenu _ = False

isProcessOpenFiles :: !(ProcessAttribute .st) -> Bool
isProcessOpenFiles (ProcessOpenFiles _) = True
isProcessOpenFiles _ = False

isProcessToolbar :: !(ProcessAttribute .st) -> Bool
isProcessToolbar (ProcessToolbar _) = True
isProcessToolbar _ = False

isProcessWindowPos :: !(ProcessAttribute .st) -> Bool
isProcessWindowPos (ProcessWindowPos _) = True
isProcessWindowPos _ = False

isProcessWindowResize :: !(ProcessAttribute .st) -> Bool
isProcessWindowResize (ProcessWindowResize _) = True
isProcessWindowResize _ = False

isProcessWindowSize :: !(ProcessAttribute .st) -> Bool
isProcessWindowSize (ProcessWindowSize _) = True
isProcessWindowSize _ = False


getProcessActivateFun :: !(ProcessAttribute .st) -> IdFun .st
getProcessActivateFun (ProcessActivate f) = f

getProcessClipboardChangedFun :: !(ProcessAttribute .st) -> IdFun .st
getProcessClipboardChangedFun (ProcessClipboardChanged f) = f

getProcessCloseFun :: !(ProcessAttribute .st) -> IdFun .st
getProcessCloseFun (ProcessClose f) = f

getProcessDeactivateFun :: !(ProcessAttribute .st) -> IdFun .st
getProcessDeactivateFun (ProcessDeactivate f) = f

getProcessOpenFilesFun :: !(ProcessAttribute .st) -> ProcessOpenFilesFunction .st
getProcessOpenFilesFun (ProcessOpenFiles f) = f

getProcessToolbarAtt :: !(ProcessAttribute .st) -> [ToolbarItem .st]
getProcessToolbarAtt (ProcessToolbar t) = t

getProcessWindowPosAtt :: !(ProcessAttribute .st) -> ItemPos
getProcessWindowPosAtt (ProcessWindowPos pos) = pos

getProcessWindowResizeFun :: !(ProcessAttribute .st) -> ProcessWindowResizeFunction .st
getProcessWindowResizeFun (ProcessWindowResize f) = f

getProcessWindowSizeAtt :: !(ProcessAttribute .st) -> Size
getProcessWindowSizeAtt (ProcessWindowSize size) = size
