implementation module stdProperty

/*
	GAST: A Generic Automatic Software Test-system
	
	stdProperty: opertors on logical properties

	Pieter Koopman, 2004..2008
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import testable, StdEnv
from MersenneTwister import genRandInt

class (==>) infixr 1 b :: b p -> Property | Testable p

instance ==> Bool
where
	(==>) c p
		| c	= Prop (evaluate p)
			= Prop (\rs r = [{r & res = Rej}])

instance ==> Property
where
	(==>) c p = Prop imp
	where
		imp rs r
		# r1 = testAnalysis r (evaluate c rs r)
		= case  r1.res of
			OK	= evaluate p rs r1
				= [{r & res = Rej}]

class (\/) infixr 2 a b	:: !a b -> Property	//	Conditional or  of arg1 and arg2
class (/\) infixr 3	a b :: !a b -> Property	//	Conditional and of arg1 and arg2

instance /\ Bool     Bool      where (/\) x y = prop (x && y)
instance /\ Property Bool      where (/\) x y = x /\ prop y
instance /\ Bool     Property  where (/\) x y = prop x /\ y
instance /\ Property Property
where (/\) x y = Prop (and x y)
	  where
		and x y rs r 
			# (rs2,rs) = split rs
			  r1 = testAnalysis r (evaluate x rs r)
			  r2 = testAnalysis r (evaluate y rs2 r)
			= case (r1.res,r2.res) of // collect labels  !! 
				(CE   ,_    )	= [r1] // to fix the evaluation order
				(_    ,CE   )	= [r2]
				(Undef,_    )	= [r2]
				(Rej  ,OK   )	= [r2]
								= [r1]
/*
				(OK   ,OK   )	= [r1]
				(OK   ,Rej  )	= [r1]
				(OK   ,Undef)	= [r1]
				(OK   ,CE   )	= [r2]
				(Rej  ,OK   )	= [r2]
				(Rej  ,Rej  )	= [r1]
				(Rej  ,Undef)	= [r1]
				(Pass ,CE   )	= [r2]
				(Pass ,OK   )	= [r1]
				(Pass ,Rej  )	= [r1]
				(Pass ,Undef)	= [r1]
				(Pass ,CE   )	= [r2]
				(Undef,OK   )	= [r2]
				(Undef,Rej  )	= [r2]
				(Undef,Undef)	= [r2]
				(Undef,CE   )	= [r2]
				(CE   ,OK   )	= [r1]
				(CE   ,Rej  )	= [r1]
				(CE   ,Undef)	= [r1]
				(CE   ,CE   )	= [r1]
*/
instance \/ Bool     Bool      where (\/) x y = prop (x || y)
instance \/ Property Bool      where (\/) x y = x \/ prop y
instance \/ Bool     Property  where (\/) x y = prop x \/ y
instance \/ Property Property
where (\/) x y = Prop (or x y)
	  where
		or x y rs r 
			# (rs2,rs) = split rs
			= case testAnalysis r (evaluate x rs r) of
				r=:{res=OK}		= [r]
				r=:{res=Pass}	= case testAnalysis r (evaluate y rs2 r) of
									r2=:{res=OK} = [r2]
												 = [r]
								= evaluate y rs2 r

(<==>) infix 4 :: !a !b -> Property	| Testable a & Testable b		//	True if properties are equivalent
(<==>) p q 
		# rs = genRandInt 42
		  r  = {res=Undef, labels=[], args=[], name=[]}
		  b  = testAnalysis r (evaluate p rs r)
		  c  = testAnalysis r (evaluate q rs r)
		= prop (b.res == c.res) // can this be improved?

(===>) infix 1 :: Bool Bool -> Bool
(===>) p q = (not p) || q 

ExistsIn :: (x->p) [x] -> Property | Testable p & TestArg x
ExistsIn f l = Prop p
where p rs r = [exists r [testAnalysis r (evaluate (f a) rs r)\\a <- l] MaxExists]

Exists :: (x->p) -> Property | Testable p & TestArg x
Exists f = Prop p
where p rs r
		# (rs,rs2) = split rs
		= [exists r [testAnalysis r (evaluate (f a) rs2 r)\\a <- generateAll rs] MaxExists]
exists r []				n = {r & res = CE}
exists r _				0 = {r & res = Undef}
exists _ [r=:{res}:x]	n = case res of
								OK	= r
								Pass	= r
										= exists r x (n-1)

noCE r []              n = {r & res = OK}
noCE r _               0 = {r & res = Pass}
noCE _ [r=:{res=CE}:x] n = r
noCE _ [r=:{res=OK}:x] n = noCE {r&res=Pass} x (n-1)
noCE r [_:x]           n = noCE r x (n-1)

testAnalysis :: Admin [Admin] -> Admin // maakt van een lijst resultaten een enkel resultaat
testAnalysis r l = analysis l MaxExists Undef OK
where
	analysis []    n min max = {r & res = max}
	analysis _     0 min max = {r & res = min}
	analysis [s:x] n min max
	 = case s.res of
		CE		= s
		OK		= analysis x (n-1) Pass max
		Pass	= analysis x (n-1) Pass Pass
		Undef	= analysis x (n-1) min  max
		Rej		= case min of
					OK  	= analysis x (n-1) OK   max
					Pass	= analysis x (n-1) Pass max
							= analysis x (n-1) Rej  max
				= abort "Unknow result in testAnalysis"

ForAll :: !(x->p) -> Property | Testable p & TestArg x
ForAll f = Prop (evaluate f)

ForEach :: ![x] !(x->p) -> Property | Testable p & TestArg x
ForEach list f = Prop (forAll f list)

(For) infixl 0 :: !(x->p) ![x] -> Property | Testable p & TestArg x
(For) p list = ForEach list p

// XXXXXXXXXXXXXXXXXXXXXXXXXX

class (VOOR) infixl 0 t :: (t a b) [a] -> [b]
instance VOOR (->)
where VOOR f l = map f l

:: PL a b = PL [a->b]
instance VOOR PL
where VOOR (PL fl) l = diagonal [map f l\\f<-fl] //[f x \\ f<-fl, x<-l]

//| Testable p & TestArg x

// XXXXXXXXXXXXXXXXXXXXXXXXXX

(ForAndGen) infixl 0 :: !(x->p) ![x] -> Property | Testable p & TestArg x
(ForAndGen) p list = Prop (evaluate p)
where evaluate f rs result
		# (rs,rs2) = split rs
		= forAll f (list++generateAll rs) rs2 result

classify :: !Bool l !p -> Property | Testable p & genShow{|*|} l
classify c l p
	| c	= Prop (\rs r = evaluate p rs {r & labels = [show1 l:r.labels]})
		= Prop (evaluate p)

label ::  !l !p -> Property | Testable p & genShow{|*|} l
label l p = Prop (\rs r = evaluate p rs {r & labels = [show1 l:r.labels]})

name :: !n !p -> Property | Testable p & genShow{|*|} n
name n p = Prop (\rs r = evaluate p rs {r & name = [show1 n:r.name]})

instance ~ Bool where ~ b = not b

instance ~ Result
where
	~ CE = OK
	~ OK = CE
	~ Pass = CE
	~ Rej = Rej
	~ Undef = Undef

instance ~ Property
where ~ (Prop p) = Prop (\rs r = let r` = testAnalysis r (p rs r) in [{r` & res = ~r`.res}])

