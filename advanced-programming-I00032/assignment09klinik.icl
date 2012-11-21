module assignment09klinik

import StdEnv, gast
/*
	Pieter Koopman, pieter@cs.ru.nl
	2010, 2012
	Skeleton for exercise 9 of the course advanced programming:
	The operational semantics of a strict first order functional programming language
*/

/********************** data types **********************/

:: Var = VI Ident
:: Fun = FI Ident

:: Ident :== String

:: Expr
	= Int   Int
	| Bool  Bool
	| Fun   Fun
	| Var   Var
	| Ap    Expr [Expr]
	| Infix Expr Prim Expr
	| Prim  Prim

:: Prim = IF | +. | *. | -. | <. | NOT

:: Def = Def Ident [Var] Expr

/********************** environments **********************/

:: State :== Env Expr
:: Funs  :== Env Def
:: Env e :== Ident -> e

(|->) infix 9 :: Ident v -> (Env v) -> (Env v)
(|->) v e = \env x.if (x==v) e (env x)

newEnv :: Env e
newEnv = \v.abort ("No binding for " + v)

/********************** semantic functions **********************/

E :: Expr State Funs -> Expr
E e vf fs = e								// to be improved

Ds :: [Def] -> Expr
Ds defs = abort "Ds not proprly defined"	// to be improved

/********************** instances of generic functions for gast **********************/

derive gEq		Expr, Prim, Def, Var, Fun
derive genShow	Expr, Prim, Def, Var, Fun
derive ggen		Expr, Prim, Def, Var, Fun	// likely to be improved for real tests

derive bimap []
/********************** some unit tests using gast **********************/

Start
 = test
	[ ("a" |-> "a") (("b" |-> "b") newEnv) "b" == "b"
	, ("a" |-> "a") (("b" |-> "b") newEnv) "a" == "a"
	, ("a" |-> "a") (("a" |-> "b") newEnv) "a" == "a"
	, E (Ap (Fun (FI "id")) [Int 7]) newEnv (("id" |-> ID) newEnv) === Int 7
	, E (Ap (Prim +.) [Int 3, Int 5]) newEnv newEnv === Int 8
	, E (Infix (Int 3) +. (Int 5)) newEnv newEnv === Int 8
	, E (Ap (Fun (FI "max")) [Int 3, Int 5]) newEnv (("max" |-> MAX) newEnv) === Int 5
	, E (Ap (Fun (FI "max")) [Int 5, Int 3]) newEnv (("max" |-> MAX) newEnv) === Int 5
	, E (Ap (Fun (FI "max")) [Int 5, Int 3]) newEnv (("fac" |-> FAC) (("max" |-> MAX) newEnv)) === Int 5
	, E (Infix (Infix (Int 5) -. (Int 1)) <. (Int 2)) newEnv newEnv === Bool False
	, E (Ap (Fun (FI "dec")) [Int 3]) newEnv (("dec" |-> DEC) newEnv) === Int 2
	, E (Ap (Prim +.) [Ap (Prim +.) [Int 3, Int 5], Int 5]) newEnv newEnv === Int 13
	, E (Ap (Fun (FI "dec")) [Ap (Prim +.) [Int 3, Int 5]]) newEnv (("dec" |-> DEC) newEnv) === Int 7
	, E (Ap (Fun (FI "count")) [Int 0]) newEnv (("count" |-> COUNT) newEnv) === Int 1
	, E (Ap (Fun (FI "count")) [Int 1]) newEnv (("count" |-> COUNT) newEnv) === Int 1
	, E (Ap (Fun (FI "fac")) [Int 1]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 1
	, E (Ap (Fun (FI "fac")) [Int 2]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 2
	, E (Ap (Fun (FI "fac")) [Int 3]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 6
//	, E (Ap (Fun (FI "twice")) [Fun (FI "inc"),Int 0]) newEnv (("inc" |-> INC) (("twice" |-> TWICE) newEnv)) === Int 3 // higher order
	, Ds [start0  :defs] === Int 42
	, Ds [start1 1:defs] === Int 1
	, Ds [start1 2:defs] === Int 2
	, Ds [start1 3:defs] === Int 6
	, Ds [start1 4:defs] === Int 24
	]

start0   = Def "Start" [] (Int 42)
start1 i = Def "Start" [] (Ap (Fun (FI "fac")) [Int i])

defs = [ID, DEC, INC, FAC, MAX, COUNT]


ID  = Def "id" [(VI "x")] (Var (VI "x"))
MAX = Def "max" [(VI "x"),(VI "y")] (Ap (Prim IF) [Ap (Prim <.) [Var (VI "x"),Var (VI "y")],Var (VI "y"),Var (VI "x")])
DEC = Def "dec" [(VI "x")] (Ap (Prim -.) [Var (VI "x"),Int 1])
INC = Def "inc" [(VI "x")] (Infix (Var (VI "x")) +. (Int 1))
COUNT = Def "count" [(VI "x")] (Ap (Prim IF)
									[Infix (Var (VI "x")) <. (Int 1)
									,Int 1
									,Ap (Fun (FI "count")) [Infix (Var (VI "x")) -. (Int 1)]
									]
						  )
FAC = Def "fac" [(VI "x")] (Ap (Prim IF) [Infix (Var (VI "x")) <. (Int 2)
									,Int 1
									,Ap (Prim *.) [Var (VI "x")
													 ,Ap (Fun (FI "fac")) [Ap (Fun (FI "dec")) [Var (VI "x")]]
													 ]
									]
						  )
TWICE = Def "twice" [(VI "f"),(VI "x")] (Ap (Var (VI "f")) [Ap (Var (VI "f")) [Var (VI "x")]]) // this is higher order !!
