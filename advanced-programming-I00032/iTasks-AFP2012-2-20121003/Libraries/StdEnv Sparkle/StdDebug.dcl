definition module StdDebug

// ********************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ********************************************************

import StdClass

// The following functions should only be used for debugging,
// because these functions have side effects

trace :: !msg .a -> .a | toString msg	// write toString msg to stderr
										// before evaluating a
trace_n :: !msg .a -> .a | toString msg	// write toString msg and newline to stderr
										// before evaluating a

trace_t :: !msg -> Bool | toString msg	// write toString msg to stderr
										// result is True
trace_tn :: !msg -> Bool | toString msg	// write toString msg and newline to stderr
										// result is True
