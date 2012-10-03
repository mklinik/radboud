implementation module
	StdFraction

// ****************************************************************************************

import
	StdEnv

// ----------------------------------------------------------------------------------------
:: Fraction =
// ----------------------------------------------------------------------------------------
	  DivideByZero
	| Fraction		!Int !Int

test :: !Int !Int -> Int
test x y
	| z == 12
		| y == 12
			= 12
			= 14
		= 14
	where
		z = x + 12

toReal2 s posn val dec_new dval exp eneg eval
//	| posn == len
//		= 	val*dval*10.0 ^  toReal (eneg*eval)
//	| digit && not dec_new && not exp
//		= 	toReal2 s (posn+1) (toReal n + 10.0*val) dec_new dval exp eneg eval
//	| digit && dec_new && not exp
//		= 	toReal2 s (posn+1) (toReal n + 10.0*val) dec_new (dval/10.0) exp eneg eval
	| digit && exp
		= 	toReal2 s (posn+1) val dec_new dval exp eneg (n + 10*eval )
//	| not dec_new && not exp && c == '.'
//		= 	toReal2 s (posn+1) val True 1.0 exp eneg eval
//	| not exp && (c== 'e' || c== 'E')
//		| posn<len-2 && s.[posn+1] == '-'
//			= 	toReal2 s (posn+2) val dec_new dval True (-1) 0
//		| posn<len-2 && s.[posn+1] == '+'
//			= 	toReal2 s (posn+2) val dec_new dval True (+1) 0
//		| posn<len-1
//			= 	toReal2 s (posn+1) val dec_new dval True 1 0 
//		// otherwise
//			= 	0.0
	// otherwise
		= 	0.0
	where
		c		=	s.[posn]
		n		=	toInt c  -  toInt '0' 
		digit	=	0<=n  &&  n<=9 
		len		=	12
