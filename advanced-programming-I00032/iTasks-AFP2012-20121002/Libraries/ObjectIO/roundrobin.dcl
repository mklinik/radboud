definition module roundrobin


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


::	RR x
	=	{	done:: !.[x]		// The elements that are done (in reverse order)
		,	todo:: !.[x]		// The current element and the elements to do (in order)
		}

emptyRR		::                      RR .x				// emptyRR                      = {[],[]}

toRR		:: !u:[.x] !u:[.x] -> u:RR .x				// toRR done todo               = {done,todo}
fromRR		::      !u:(RR .x) -> (!u:[.x],!u:[.x])		// fromRR {done,todo}           = (done,todo)

isEmptyRR	::      !u:(RR .x) -> (!Bool,!u:RR .x)		// isEmptyRR rr                 = rr==emptyRR
nodoneRR	::      !u:(RR .x) -> (!Bool,!u:RR .x)		// nodoneRR  rr                 = rr=={[],_}
notodoRR	::      !u:(RR .x) -> (!Bool,!u:RR .x)		// notodoRR  rr                 = rr=={_,[]}

resetRR		::      !u:(RR .x) -> u:RR .x				// resetRR        {done,todo}   = {[],(reverse done)++todo}
adddoneRR	:: .x   !u:(RR .x) -> u:RR .x				// adddoneRR    x {done,todo}   = {[x:done],todo}
inserttodoRR:: .x   !u:(RR .x) -> u:RR .x				// inserttodoRR x {done,todo}   = {done,[x:todo]}
appendtodoRR:: .x   !u:(RR .x) -> u:RR .x				// appendtodoRR x {done,todo}   = {done,todo++[x]}
getcurrentRR::      !u:(RR .x) -> (!.x,!u:RR .x)		// getcurrentRR {done,[x:todo]} = (x,{done,todo})
