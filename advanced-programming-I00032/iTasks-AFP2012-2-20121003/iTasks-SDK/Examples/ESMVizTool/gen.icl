implementation module gen

/*
	GAST: A Generic Automatic Software Test-system
	
	gen: generic generation of values of a type

	Pieter Koopman, 2004
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdGeneric, Random
import StdBool, StdList, StdArray, StdEnum, StdMisc
from StdFunc import id, seqList, :: St
// -------
:: RandomStream :== [Int]

aStream :: RandomStream
aStream = genRandInt 42

split :: RandomStream -> (RandomStream,RandomStream)
split [r,s:rnd]
	# seed = r*s
	| seed==0
		= split rnd
		= (genRandInt seed, rnd)
split _ = abort "gen.icl: the increadable has been done, you have used all random values!"
// -------

randomize :: [a] [Int] Int ([Int] -> [a]) -> [a]
randomize list rnd n c = rand list rnd n []
where
	rand [] rnd m [] = c rnd
	rand [] rnd m [x] = [x:c rnd]
	rand [] rnd m l = rand l rnd n []
	rand [a:x] [i:rnd] m l
		| m==0 || (i rem m) == 0
			= [a:rand x rnd (m-1) l]
			= rand x rnd m [a:l]
// -------

generic ggen a :: Int [Int] -> [a]

maxint :: Int
maxint = 2147483647

minint :: Int
minint = -2147483648

ggen{|Int|}  n rnd = randomize [0,1,-1,maxint,minint] rnd 5 id
ggen{|Bool|} n rnd = randomize [False,True] rnd 2 \_.[]
ggen{|Char|} n rnd = randomize (map toChar [32..126] ++ ['\t\n\r']) rnd 98 (\_.[])
ggen{|Real|}  n rnd = randomize [0.0,1.0,-1.0] rnd 3 (\[s:x] -> f x (genRandReal s))
where f [i,j:x] [r:s]
		| r==0.0
			= [r:f x s]
			# r = if (isOdd i) r (~r)
			  r = if (isOdd j) r (1.0/r)
			= [r:f x s]

bias :== 1024

ggen{|UNIT|} n rnd = [UNIT]
ggen{|PAIR|} f g n rnd
	# (rn2,rnd) = split rnd
	= map (\(a,b)=PAIR a b) (diag2 (f n rnd) (g n rn2)) // inlinen !?
ggen{|EITHER|} f g n rnd
		# (r1,rnd) = split rnd
		  (r2,rnd) = split rnd
		= Merge n rnd (f n r1) (g (n+1) r2)
where
	Merge :: Int RandomStream [a] [b] -> [EITHER a b] // Correct, strict in none of the lists!
	Merge n [i:r] as bs
	 | (i rem n) <> 0
//	 | (i rem bias) > n+(bias/2)
		= case as of
			[] = map RIGHT bs
			[a:as] = [LEFT a: Merge n r as bs]
		= case bs of
			[] = map LEFT as
			[b:bs] = [RIGHT b: Merge n r as bs]
/*	Merge :: RandomStream [a] [b] -> [EITHER a b] // Wrong, strict in both lists
	Merge r [] bs = map RIGHT bs
	Merge r as [] = map LEFT as
	Merge [i:r] [a:as] [b:bs]
		| isOdd i
			= [LEFT a, RIGHT b:Merge r as bs]
			= [RIGHT b, LEFT a:Merge r as bs]
*//*		= Merge (isOdd (hd rnd)) (f r1) (g r2)
where
	Merge :: Bool [a] [b] -> [EITHER a b]
	Merge r as bs
	 | r
		= case as of
			[] = map RIGHT bs
			[a:as] = [LEFT a: Merge (not r) as bs]
		= case bs of
			[] = map LEFT as
			[b:bs] = [RIGHT b: Merge (not r) as bs]
*/
ggen{|CONS|}   f n rnd = map CONS (f n rnd)
ggen{|OBJECT|} f n rnd = map OBJECT (f n rnd)
ggen{|FIELD|}  f n rnd = map FIELD (f n rnd)
ggen{|String|} n rnd = ["hello world!","Tested with GAST!": rndStrings 0 rnd]
where
	rndStrings 0 rnd = ["": rndStrings 1 rnd]
	rndStrings len [r,s:rnd] 
		# (chars,rnd)	= seqList (repeatn ((abs r) rem len) genElem) rnd
		  string		= {c \\ c<-chars}
		= [string:rndStrings ((len rem StrLen)+1) rnd]

class genElem a where genElem :: RandomStream -> .(a,RandomStream)

instance genElem Int where genElem [r:rnd] = (r,rnd)
instance genElem Char where genElem [r:rnd] = (toChar (32+((abs r) rem 94)),rnd)
instance genElem Bool where genElem [r:rnd] = (isOdd r,rnd)
instance genElem Real where genElem [r,s,t:rnd] = ((toReal r/toReal s)*toReal t,rnd)

derive ggen (,), (,,), (,,,), []
derive bimap []
