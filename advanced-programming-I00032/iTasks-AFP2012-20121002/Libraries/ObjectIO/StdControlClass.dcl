definition module StdControlClass


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdControlClass define the standard set of controls instances.
//	********************************************************************************


import	StdControlDef
from	windowhandle	import :: ControlState
from	StdPSt			import :: PSt, :: IOSt


class Controls cdef where
	controlToHandles	:: !.(cdef      .ls (PSt .l)) !(PSt .l)
					 -> (![ControlState .ls (PSt .l)], !PSt .l)
	getControlType		::  .(cdef      .ls .pst)
					 -> ControlType

instance Controls (AddLS  c)			| Controls c
instance Controls (NewLS  c)			| Controls c
instance Controls (ListLS c)			| Controls c
instance Controls NilLS
instance Controls ((:+:) c1 c2)			| Controls c1 & Controls c2

instance Controls ButtonControl
instance Controls CheckControl
instance Controls (CompoundControl c)	| Controls c
instance Controls CustomButtonControl
instance Controls CustomControl
instance Controls EditControl
instance Controls (LayoutControl   c)	| Controls c
instance Controls PopUpControl
instance Controls RadioControl
instance Controls SliderControl
instance Controls TextControl
