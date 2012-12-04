definition module systemid


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import StdOverloaded


::	SystemId


worldSystemId	:: !Int			->	SystemId
worldChildId	:: !Int			->	SystemId
initSystemId	::	SystemId
nullSystemId	::  SystemId
incrSystemId	:: !SystemId	->	(!SystemId,!SystemId)

instance == SystemId
instance <  SystemId
