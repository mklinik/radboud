implementation module StdReal

// ********************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ********************************************************
import StdClass
import StdOverloaded,StdInt,StdArray
from StdBool import &&,||,not
from StdChar import instance == Char, class ==(..)

instance + Real
where
 (+) :: !Real !Real -> Real
 (+) a b
	= code inline {
			addR
	}

instance - Real
where
 (-) :: !Real !Real -> Real
 (-) a b
	= code inline {
			subR
	}

instance zero Real
where
 zero:: Real
 zero
	= code inline {
			pushR 0.0
	}

instance * Real
where
 (*) :: !Real !Real -> Real
 (*) a b
	= code inline {
			mulR
	}

instance / Real
where
 (/) :: !Real !Real -> Real
 (/) a b
	= code inline {
			divR
	}

instance one Real
where
 one:: Real
 one
	= code inline {
			pushR 1.0
	}

instance ^ Real
where
 (^) :: !Real !Real -> Real
 (^) a b
	= code inline {
			powR
	}

instance abs Real
where
 abs::!Real -> Real
 abs x
	= code inline {
			absR
	}

instance sign Real
where
 sign::!Real -> Int
 sign x | x == 0.0	= 0
		| x < 0.0	= -1
 					= 1

instance ~ Real
where
 ~ :: !Real -> Real
 ~ x
	= code inline {
			negR
	}

instance == Real
where
 (==) :: !Real !Real -> Bool
 (==) a b
	= code inline {
			eqR
	}

instance < Real
where
 (<) :: !Real !Real -> Bool
 (<) a b
	= code inline {
			 ltR
	}

instance ln Real
where
	ln a
		= code inline {
				lnR
		}

instance log10 Real
where
	log10 a
		= code inline {
				log10R
		}

instance exp  Real
where exp a
		= code inline {
				expR
		}

instance sqrt Real
where sqrt a
		= code inline {
				sqrtR
		}

instance sin Real
where
	sin a
		= code inline {
				sinR
		}

instance cos Real
where
	cos a
		= code inline {
				cosR
		}

instance tan Real
where 
	tan a
		= code inline {
				tanR
		}
		
instance asin Real
where
	asin a
		= code inline {
				asinR
		}

instance acos Real
where
	acos a
		= code inline {
				acosR
		}

instance atan Real
where
	atan a
		= code inline {
				atanR
		}

instance sinh Real
where
	sinh x = (exp x - exp (~ x)) * 0.5

instance cosh Real
where
	cosh x =  (exp x + exp (~ x)) * 0.5

instance tanh Real
where
	tanh x = (expx - expmx) / (expx + expmx)
	where
		expx = exp x
		expmx = exp (~ x)

instance asinh Real
where
	asinh x = ln (x + sqrt (x*x + 1.0))

instance acosh Real
where
	acosh x = ln (x + sqrt (x*x - 1.0))  // only the positive value is taken

instance atanh Real
where
	atanh x = ln ((1.0 + x)/(1.0 - x)) * 0.5

instance toReal	Int
where
 toReal :: !Int -> Real
 toReal a
	= code inline {
			ItoR
	}

instance toReal Real
where
 toReal :: !Real -> Real
 toReal a
	= code inline {
			 no_op
	}

instance fromReal Int
where
 fromReal :: !Real -> Int
 fromReal a
	= code inline {
			RtoI
	}

instance fromReal Real
where
 fromReal :: !Real -> Real
 fromReal a
	= code inline {
			 no_op
	}

instance fromReal {#Char}
where
 fromReal :: !Real -> {#Char}
 fromReal a
	= code inline {
		.d 0 1 r
			jsr RtoAC
		.o 1 0
	}

instance toReal {#Char}
where
 toReal::!{#Char} -> Real
 toReal s
	|	len == 0
			=	0.0
	|	first  == '-'
			= 	~ signedval
	|	first  == '+'
			=	signedval
	//	otherwise
			= 	val
 where
	len
		=	size s
	signedval
		= 	toReal2 s 1 0.0 False 1.0 False 0 0
	val
		=	toReal2 s 0 0.0 False 1.0 False 0 0
	first
		=	s.[0]

	toReal2 s posn val dec_new dval exp eneg eval
		| posn == len
			= 	val*dval*10.0 ^  toReal (eneg*eval)
		| digit && not dec_new && not exp
			= 	toReal2 s (posn+1) (toReal n + 10.0*val) dec_new dval exp eneg eval
		| digit && dec_new && not exp
			= 	toReal2 s (posn+1) (toReal n + 10.0*val) dec_new (dval/10.0) exp eneg eval
		| digit && exp
			= 	toReal2 s (posn+1) val dec_new dval exp eneg (n + 10*eval )
		| not dec_new && not exp && c == '.'
			= 	toReal2 s (posn+1) val True 1.0 exp eneg eval
		| not exp && (c== 'e' || c== 'E')
			| posn<len-2 && s.[posn+1] == '-'
				= 	toReal2 s (posn+2) val dec_new dval True (-1) 0
			| posn<len-2 && s.[posn+1] == '+'
				= 	toReal2 s (posn+2) val dec_new dval True (+1) 0
			| posn<len-1
				= 	toReal2 s (posn+1) val dec_new dval True 1 0 
			// otherwise
				= 	0.0
		// otherwise
			= 	0.0
		where
			c		=	s.[posn]
			n		=	toInt c  -  toInt '0' 
			digit	=	0<=n  &&  n<=9 

entier :: !Real -> Int
entier a
	= code inline {
			entierR
	}
