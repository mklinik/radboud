definition module menuwindowmenu


//	Clean object I/O library, version 1.2

//	The definition and implementation of the WindowMenu. 


from	iostate import :: PSt, :: IOSt
import	windowhandle


openWindowMenu							::										!( PSt .l) ->  PSt .l
addWindowToWindowMenu					:: !Id !Title							!( PSt .l) ->  PSt .l
removeWindowFromWindowMenu				:: !Id									!(IOSt .l) -> IOSt .l
validateWindowActivateForWindowMenu`	:: !Id !Bool ![WindowAttribute *(.ls,PSt .p)] -> [WindowAttribute *(.ls,PSt .p)]
//validateWindowActivateForWindowMenu		:: !Id !(WindowLSHandle .ls (PSt .l)) !(IOSt .l)
//											-> (!WindowLSHandle .ls (PSt .l), ! IOSt .l)
changeWindowInWindowMenu				:: !Id !String !(IOSt .l) -> IOSt .l
