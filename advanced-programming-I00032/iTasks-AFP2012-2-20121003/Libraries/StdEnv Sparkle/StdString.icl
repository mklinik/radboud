implementation module StdString

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance == {#Char}
where
 (==) :: !{#Char} !{#Char} -> Bool
 (==) a b
	= code {
		.inline ==;#
		.d 2 0
			jsr eqAC
		.o 0 1 b	
		.end
	}

instance < {#Char}
where
 (<) :: !{#Char} !{#Char} -> Bool
 (<) a b
	= code {
		.inline <;#
		.d 2 0
			jsr cmpAC
		.o 0 1 i
			pushI 0
			gtI
		.end
	}

instance toString Int
where
 toString a
	= code {
		.inline toString;i
		.d 0 1 i
			jsr ItoAC
		.o 1 0
		.end
	}

instance toString Char
where
 toString a
	= code {
		.inline toString;c
			CtoAC
		.end
	}

instance toString Real
where
 toString a
	= code {
		.inline toString;r
		.d 0 2 r
			jsr RtoAC
		.o 1 0
		.end
	}
instance toString Bool
where
 toString :: !Bool -> {#Char}
 toString a
	= code {
		.inline toString;b
		.d 0 1 b
			jsr BtoAC
		.o 1 0
		.end
	}

instance toString {#Char}
where
 toString :: !{#Char} -> {#Char} //	dummy
 toString a
	= code {
		.inline toString;#
			no_op
		.end
	}

instance fromString {#Char}
where
 fromString :: !{#Char} -> {#Char} //	dummy
 fromString a
	= code {
		.inline fromString;#
			no_op
		.end
	}

instance % {#Char}
where
 (%) ::!{#Char} !(!Int,!Int) -> {#Char}
 (%) str (a,b)
	= code {
		.inline %;#
		.d 1 2 ii
			jsr sliceAC
		.o 1 0
		.end
	}

instance +++ {#Char}
where
 (+++) :: !{#Char} !{#Char} -> {#Char}
 (+++) a b
	= code {
		.inline +++;#
		.d 2 0
			jsr catAC
		.o 1 0
		.end
	}

(+++.) infixr 5 :: !{#Char} !{#Char} -> .{#Char}
(+++.) a b
	= code {
		.inline +++.
		.d 2 0
			jsr catAC
		.o 1 0
		.end
	}

(:=) infixl 9 :: !{#Char} !(!Int,!Char) -> {#Char}	//	update i-th element
(:=) s (i,c)
	= code {
		.inline :=
		.d 1 2 ic
			jsr updateAC
		.o 1 0
		.end
	}
