implementation module StdCleanTypes

import StdCleanTypes


// import StdEnv
/* =
	{	tc_desc :: !Int
	}
*/

:: CTCons =
	E.a:
	{	cons :: !a
	}
cast :: !.a -> .b
cast a
	= code
		{
			pop_a 0
		}

:: List a
	=	Nil
	|	Cons a (List a)
/*
tdList =:
	{	td_name	= "List"
	,	td_arity = 1
	,	td_unq = False
	,	td_rhs = CTAlgType [nil, cons]
	}
	where
		nil =
			{	cd_cons = toCons []
			,	cd_args = []
			,	cd_exist = 0
			,	cd_fixity = CTFixNone
			}

		cons =
			{	cd_cons = toCons []
			,	cd_args = []
			,	cd_exist = 0
			,	cd_fixity = CTFixNone
			}

toTypeCons :: !a -> CTTypeCons
toTypeCons _ =
	{	tc_desc = 0
	}

*/

CTToCons :: !a -> CTCons
CTToCons a
	=	{	cons = cast a
		}
	
getDescriptor :: !a -> Int
getDescriptor _
	=	code
	{
			pushD_a	0
			pop_a	1
	}

from StdOverloaded import class toString
instance toString CTCons where
	toString {cons}
		=	descriptorIDtoString (getDescriptor cons)
//	where
descriptorIDtoString :: !Int -> {#Char}
descriptorIDtoString id
	=	code
	{
	.d 0 1 i
		jsr DtoAC
	.o 1 0
	}
