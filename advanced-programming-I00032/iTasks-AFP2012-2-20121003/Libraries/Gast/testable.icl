implementation module testable

/*
	GAST: A Generic Automatic Software Test-system
	
	testable: the test algorithm for logical properties

	Pieter Koopman, 2002-2010
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdEnv, MersenneTwister, genLibTest /*, StdTime*/ , gen

derive gLess Result
instance == Result where (==) x y = x===y

newAdmin :: Admin
newAdmin = {res=Undef, labels=[], args=[], name=[]}

class TestArg a | genShow{|*|}, ggen{|*|} a

//class Testable a where evaluate :: a RandomStream Admin -> [Admin]

//instance Testable Bool where evaluate b rs result = [{result & res = if b OK CE, args = reverse result.args}]
instance Testable Bool where evaluate b rs result=:{args} = [{result & args = reverse args, res = if b OK CE}]

instance Testable Result where evaluate r rs result=:{args} = [{result & args = reverse args, res = r}]

instance Testable Property
where evaluate (Prop p) rs result = p rs result

instance Testable (a->b) | Testable b & TestArg a  
where evaluate f rs admin
		# (rs,rs2) = split rs
		= forAll f (generateAll rs) rs2 admin

instance Testable [a] | Testable a  
where
//	evaluate []   rs admin=:{args} = [{admin & args = reverse args, res = OK}] 
	evaluate list rs admin = diagonal [ evaluate x (genRandInt seed) admin \\ x<-list & seed<-rs ]
//	evaluate list rs admin = flatten [ evaluate x (genRandInt seed) admin \\ x<-list & seed<-rs ]

:: Property = Prop (RandomStream Admin -> [Admin])

prop :: a -> Property | Testable a
prop p = Prop (evaluate p)

forAll :: !(a->b) ![a] RandomStream !Admin -> [Admin] | Testable b & TestArg a
forAll f []   rs r=:{args} = [{r & args = reverse args, res = OK}]				// to handle empty sets of values
forAll f list rs r = diagonal [apply f a (genRandInt seed) r \\ a<-list & seed<-rs ]

apply :: !(a->b) a RandomStream !Admin -> [Admin] | Testable b & TestArg a
apply f a rs r = evaluate (f a) rs {r & args = [show1 a:r.args]}

diagonal :: [[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]

generateAll :: !RandomStream -> [a] | ggen{|*|} a
generateAll rnd = ggen{|*|} 2 rnd

derive gEq Result
derive bimap [], (,), (,,), (,,,), (,,,,), (,,,,,)

//--- Random ---//

split :: !RandomStream -> (RandomStream,RandomStream)
split [r,s:rnd]
	# seed = r*s
	| seed==0
		= split rnd
		= (rnd, genRandInt seed)

(>>=) infix 0 :: (a -> (b,a)) (b a -> d) -> a -> d
(>>=) f g = \st = let (r,st1) = f st in g r st1

result :: b -> a -> (b,a)
result b = \a = (b,a)

//--- testing ---//

:: Config
 =	{ maxTests	:: Int
	, maxArgs	:: Int
	, every		:: Int Admin [String] -> [String]
	, fails		:: Int
	, randoms	:: [Int]
	}

verboseConfig
 =	{ maxTests	= NrOfTest
	, maxArgs	= 2*NrOfTest
	, every		= verboseEvery
	, fails		= 1
	, randoms	= aStream
	}
verboseEvery n r c = [blank,toString n,":":showArgs r.args c]

traceConfig
 =	{ maxTests	= 100
	, maxArgs	= 1000
	, every		= traceEvery
	, fails		= 1
	, randoms	= aStream
	}
traceEvery n r c = ["\n",toString n,":":showArgs r.args c]

blank :: String
blank =: { createArray len ' ' & [0] = '\r', [len-1] = '\r' } where len = 81

countConfig
 =	{ maxTests	= 100
	, maxArgs	= 10000
	, every		= countEvery
	, fails		= 1
	, randoms	= aStream
	}
countEvery n r c = [toString n,"\r": c]

quietConfig
 =	{ maxTests	= 1000
	, maxArgs	= 10000
	, every		= animate2 // \n r c = if (n rem 10000 == 0) [".":c] c
	, fails		= 1
	, randoms	= aStream
	}

animate n r c
	| n rem Steps == 0
		= case (n/Steps) rem 4 of
			0 = ["\r|":c]
			1 = ["\r/":c]
			2 = ["\r-":c]
			3 = ["\r\\":c]
			  = ["??":c]
		= c

Steps =: 1000

animate2 n r c
	| n rem Steps == 0
		= ["\r       \r",toString n," ":c]
		= c

Test :: [Testoption] !p -> [String] | Testable p
Test options p = (\config.testConfig config.randoms {config & randoms = []} p) (foldl handleOption verboseConfig options)
where
	handleOption c (Tests i)		= {c & maxTests = i, maxArgs = 2*i}
	handleOption c (Fails i)		= {c & fails = i}
	handleOption c (Args i)			= {c & maxArgs = i}
	handleOption c (RandomSeed i)	= {c & randoms = genRandInt i}
	handleOption c (RandomList r)	= {c & randoms = r}
	handleOption c Verbose			= {c & every = verboseEvery}
	handleOption c Concise			= {c & every = countEvery}
	handleOption c Quiet			= {c & every = animate2}

TestList :: [Testoption] ![p] -> [String] | Testable p
TestList options ps = flatten (map (Test options) ps)

test :: !p -> [String] | Testable p
test p = testn NrOfTest p

testn :: !Int !p -> [String] | Testable p
testn n p = verbosen n aStream p

testnm :: !Int !Int !p -> [String] | Testable p
testnm n m p = testConfig aStream { verboseConfig & maxTests = n, maxArgs = 100*n, fails = m } p

ttestn :: !Int !p -> [String] | Testable p
ttestn n p = testConfig aStream { traceConfig & maxTests = n, maxArgs = 100*n } p

ttestnm :: !Int !Int !p -> [String] | Testable p
ttestnm n m p = testConfig aStream { traceConfig & maxTests = n, maxArgs = 100*n, fails = m } p

verbose  :: !RandomStream !p -> [String] | Testable p
verbose rs p = testConfig rs verboseConfig p

verbosen :: !Int !RandomStream !p -> [String] | Testable p
verbosen n rs p = testConfig rs { verboseConfig & maxTests = n, maxArgs = 100*n } p

concise :: !RandomStream !p -> [String] | Testable p
concise rs p = testConfig rs countConfig p

concisen   :: !Int !RandomStream !p -> [String] | Testable p
concisen n rs p = testConfig rs { countConfig & maxTests = n, maxArgs = 100*n } p

quiet :: !RandomStream !p -> [String] | Testable p
quiet rs p = testConfig rs quietConfig p

quietn   :: !Int !RandomStream !p -> [String] | Testable p
quietn n rs p = testConfig rs { quietConfig & maxTests = n, maxArgs = 100*n } p

quietnm   :: !Int !Int !RandomStream !p -> [String] | Testable p
quietnm n m rs p = testConfig rs { quietConfig & maxTests = n, maxArgs = 100*n, fails = m } p

aStream :: RandomStream
aStream = genRandInt 1957 //1964

gather :: [Admin] -> [[String]]
gather list = [r.args \\ r<- list]

testConfig :: RandomStream Config p -> [String] | Testable p
testConfig rs {maxTests,maxArgs,every,fails} p = analyse (evaluate p rs newAdmin) maxTests maxArgs 0 0 0 [] []
where
	analyse :: ![.Admin] !Int !Int !Int !Int !Int ![(String,Int)] ![String] -> [String]
	analyse [] ntests nargs 0    0    0  labels name = [blank:showName name [" Proof: success for all arguments": conclude ntests nargs 0 0 labels]]
	analyse [] ntests nargs nrej 0    0  labels name = [blank:showName name [" Proof: Success for all not rejected arguments,": conclude ntests nargs nrej 0 labels]]
	analyse [] ntests nargs nrej nund 0  labels name 
		| ntests==maxTests	= [blank:showName name [" Undefined: no success nor counter example found, all tests rejected or undefined ": conclude ntests nargs nrej nund labels]]
							= [blank:showName name [" Success for arguments, ": conclude ntests nargs nrej nund labels]]
	analyse [] ntests nargs nrej nund ne labels name = [blank:showName name [toString ne," counterexamples found,": conclude ntests nargs nrej nund labels]]
	analyse _  0      nargs nrej nund 0  labels name = [blank:showName name [" Passed": conclude 0 nargs nrej nund labels]]
	analyse _  0      nargs nrej nund ne labels name = [blank:showName name [toString ne," counterexamples found,": conclude 0 nargs nrej nund labels]]
	analyse _  ntests 0     nrej nund 0  labels name 
		| ntests==maxTests	= [blank:showName name [" No tests performed, maximum number of arguments (",toString maxArgs,") generated": conclude ntests 0 nrej nund labels]]
							= [blank:showName name [" Passed: maximum number of arguments (",toString maxArgs,") generated": conclude ntests 0 nrej nund labels]]
	analyse _  ntests 0     nrej nund ne labels name = [blank:showName name [toString ne," counterexamples found,": conclude 0 0 nrej nund labels]]
	analyse [res:rest] ntests nargs nrej nund ne labels name 
		= every (maxTests-ntests+1) res
		  (	case res.res of
			 OK 	= analyse rest (ntests-1) (nargs-1) nrej nund ne (admin res.labels labels) res.name
			 Pass	= analyse rest (ntests-1) (nargs-1) nrej nund ne (admin res.labels labels) res.name // NOT YET CORRECT ?
			 CE		= ["\r":showName res.name ["Counterexample ",toString (ne+1)," found after ",toString (maxTests-ntests+1)," tests:":showArgs res.args ["\n":more]]]
			 		  where
			 		  	more | ne+1<fails
			 		  		= analyse rest (ntests-1) (nargs-1) nrej nund (ne+1) labels res.name
			 		  		= showName res.name [toString (ne+1)," counterexamples found,": conclude (ntests-1) nargs nrej nund labels]
			 Rej	= analyse rest ntests (nargs-1) (nrej+1) nund     ne labels res.name
			 Undef	= analyse rest ntests (nargs-1) nrej     (nund+1) ne labels res.name
			 		= abort "Error in Gast: analyse; missing case for result\n"
		  )

	conclude ntests nargs nrej nund labels
		# n    = maxTests-ntests
		  rest = showLabels n (sort labels)
		  rest = case nrej of
		  			0 = rest
		  			1 = [" one case rejected":rest]
		  			  = [" ",toString nrej," cases rejected":rest]
		  rest = case nund of
		  			0 = rest
		  			1 = [" one case undefined":rest]
		  			  = [" ",toString nund," cases undefined":rest]
		| n==0
			= rest
			= [" after ",toString n," tests":rest]

	admin :: ![String] ![(String,Int)] -> [(String,Int)]
	admin [] accu = accu
	admin [label:rest] accu = admin rest (insert label accu)

	insert :: !String ![(String,Int)] -> [(String,Int)]
	insert label [] = [(label,1)]
	insert label [this=:(old,n):rest]
	 | label==old
		= [(old,n+1):rest]
		= [this:insert label rest]

	showLabels :: !Int ![(String,Int)] -> [String]
	showLabels ntests [] = ["\n"]
	showLabels 0      [(lab,n):rest] = ["\n",lab,": ",toString n:showLabels 0 rest]
	showLabels ntests [(lab,n):rest] = ["\n",lab,": ",toString n," (",toString (toReal (n*100)/toReal ntests),"%)":showLabels ntests rest]

	showName l c = s (reverse l) c
	where
		s [] c = c
		s [l] c = [l," ":c]
		s [a:x] c = [a,".":s x c]

cr :== "\r"

showArgs :: ![String] [String] -> [String]
showArgs []       c = c // ["\n":c] // c
showArgs [a:rest] c = [" ",a: showArgs rest c]
