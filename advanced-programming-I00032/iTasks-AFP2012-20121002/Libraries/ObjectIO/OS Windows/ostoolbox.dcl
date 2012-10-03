definition module ostoolbox

//	Clean Object I/O library, version 1.2

::	OSToolbox 
	:==	Int

// OSNewToolbox :: *OSToolbox
OSNewToolbox :== 0

// RWS ??? add success bool
osInitToolbox :: !*OSToolbox -> *OSToolbox

// RWS ??? ugly
// OSDummyToolbox :: *OSToolbox
OSDummyToolbox :== 0

// PA: moved from world to ostoolbox
worldGetToolbox :: !*World -> (!*OSToolbox,!*World)
worldSetToolbox :: !*OSToolbox !*World -> *World
