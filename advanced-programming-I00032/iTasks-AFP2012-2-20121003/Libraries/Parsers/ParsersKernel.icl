implementation module ParsersKernel

// ****************************************************************************************
//	Clean Library Module
//	Erik Zuurbier eri.zuurbier@tiscali.nl
//	Copyright 2005 HILT bv & Radboud University Nijmegen, The Netherlands
// ****************************************************************************************

import StdEnv
from StdMaybe import :: Maybe(..)

:: Parsable s		:== ([Int],Int,[s],SymbolTypes)
:: ParseResult s r	:== (Suggestions,[(Parsable s,r)])
:: BareParser s r	:== (Parsable s) -> ParseResult s r
:: Alt s t  		:== (Xor s t) Suggestions -> ParseResult s t
:: Succ s r t		:== r (Alt s t) (Xor s t) Suggestions -> BareParser s t
:: Parser s r t		:== Hypothesis (Succ s r t) (Alt s t) (Xor s t) Suggestions -> BareParser s t
:: Xor s t			:== Suggestions -> ParseResult s t

empty_xc :: Xor s t
empty_xc = \sg -> (sg,[])

:: Suggestions 		= (@) infix 9 (SymbolTypes,[Hypothesis]) HypPos
:: SymbolTypes		:== [SymbolType]		// in increasingly coarser units
:: SymbolType		= Whole String			// e.g. "letter", "word", "sentence","paragraph"
					| Slice String Int		// when atMost has cut off some

instance + Suggestions
where	(+) sg1=:((symTypes,hyp1)@i1) sg2=:((_,hyp2)@i2)
			| i1 > i2	= sg1
			| i1 < i2	= sg2
			= (symTypes,hyp1++hyp2) @ i1
				// if the positions are equal, the symTypes should also be equal, right?

:: Hypothesis		:== [(String,HypPos)]
		// String = description of where we are in the PARSER (not in the input!)

:: HypPos			:== [Int]
	/*	Int above indicates HALF a position:
			EndAt	0th symbol -> 1
			At		1st symbol -> 2
			EndAt	1st symbol -> 3
			At		2nd symbol -> 4
		This odd construction (quite literally) is for reasonable error-reporting in checkIf */

posLength		=: 2
halfPosLength	=: 1
startPos		=: posLength
startErrorPos	=: [startPos]

//toPos :: Int -> Int
toPos i :== i<<1

//fromPos :: Int -> Int
fromPos i :== i>>1

toSugPos :: Int -> SugPosition
toSugPos i = let sp=fromPos i in if (isEven i) (At sp) (EndAt sp) 

:: SugPosition	= At Int | EndAt Int

toSugPos` :: (String,HypPos) -> (String,[SugPosition])
toSugPos` (str,hyPos) = (str,map toSugPos hyPos)

instance == SugPosition
where	(==) (At i)		sp	= case sp of
				(At j)		-> i==j
				else		-> False
		(==) (EndAt i)	sp	= case sp of
				(EndAt j)	-> i==j
				else		-> False

instance < SugPosition
where	(<) (At i)		sp	= case sp of
				(At j)		-> i<j
				(EndAt j)	-> i<j
		(<) (EndAt i)	sp	= case sp of
				(EndAt j)	-> i<j
				(At j)		-> i<=j

fail :: Parser s r t
fail = \hy sc ac xc sg ss -> ac xc sg

yield :: r -> Parser s r t
yield x = \hy sc ac xc -> sc x ac empty_xc

anySymbol :: Parser s s t
anySymbol = p
where	p hy sc ac xc sg (is,i,[s:ss],symTypes)	= sc s ac empty_xc sg (is,i+posLength,ss,symTypes)
		p hy sc ac xc sg (is,i,[]	 ,symTypes)	= ac xc (sg + (symTypes,[hy])@(is++[i]))

symbol :: s -> Parser s s t | == s
symbol x = p
where	p hy sc ac xc sg (is,i,[s:ss],symTypes) | x==s	= sc s ac empty_xc sg (is,i+posLength,ss,symTypes)
		p hy sc ac xc sg (is,i,_	 ,symTypes)			= ac xc (sg + (symTypes,[hy])@(is++[i]))


satisfy :: (s -> Bool) -> Parser s s t
satisfy ok = p
where	p hy sc ac xc sg (is,i,[s:ss],symTypes) | ok s	= sc s ac empty_xc sg (is,i+posLength,ss,symTypes)
		p hy sc ac xc sg (is,i,_	 ,symTypes)			= ac xc (sg + (symTypes,[hy])@(is++[i]))

getPosition	:: Parser s [Int] t
getPosition = \hy sc ac xc sg ss=:(is,i,_,_) -> sc (map fromPos (is++[i])) ac empty_xc sg ss

advancePosition :: !Int -> Parser s r t
advancePosition n = p
where	p hy sc ac xc sg (is,i,ss,symTypes)
			# error					= ac xc (sg + (symTypes,[hy])@(is++[i]))
			| n<0					= error
			# (firstn,rest)			= splitAt n ss
	 		| length firstn == n	= sc u ac empty_xc sg (is,i+toPos n,rest,symTypes)
									= error
		u	= abort "undefined result of parser-constructor 'advancePosition' accessed"

getParsable :: Parser s (Parsable s) t
getParsable = \hy sc ac xc sg ss -> sc ss ac empty_xc sg ss

setParsable :: (Parsable s) -> Parser s r t
setParsable ss = \hy sc ac xc sg ss` -> sc u ac empty_xc sg ss
where	u	= abort "undefined result of parser-constructor 'setParsable' accessed"

token :: [s] -> Parser s [s] t | == s
token ss = p
where	p hy sc ac xc sg (is,i,ssi,symTypes)
			# n						= length ss
			# (firstn,rest)			= splitAt n ssi
			| firstn == ss			= sc ss ac empty_xc sg (is,i+toPos n,rest,symTypes)
			= ac xc (sg + (symTypes,[hy])@(is++[i]))

empty :: Parser s r t
empty = p
where	p hy sc ac xc sg (is,i,ss,symTypes)	= case ss of
			[]	-> sc u1 ac empty_xc sg u2
			_	-> ac xc (sg + (symTypes,[hy])@(is++[i]))
		u1	= abort "undefined result of parser-constructor 'empty' accessed" 
		u2	= abort "undefined rest-input of parser-constructor 'empty' accessed" 

// PARSER COMBINATORS:

(<|>) infixr 4 :: (Parser s r t) (Parser s r t) -> Parser s r t
(<|>) p1 p2 = \hy sc ac xc sg ss -> p1 hy sc (\xc3 sg` -> p2 hy sc ac xc3 sg` ss) xc sg ss

(<&>) infixr 6 :: (Parser s u t) (u -> Parser s v t) -> Parser s v t
(<&>) p wp = \hy sc ac xc -> p hy (\t ac1 xc1 -> wp t hy sc ac1 xc) ac xc

(<++>) infixl 6 :: (Parser s (r->u) t) (Parser s r t) -> Parser s u t
(<++>) p1 p2 = \hy sc ac xc -> p1 hy (\f ac1 xc1 -> p2 hy (\rp -> sc (f rp)) ac1 xc) ac xc

:: MonadicSeq s r u t = (<&->)  infixr 6 (Parser s u      t) (u -> Parser s r t)
:: ArrowSeq   s u r t = (<++->) infixl 6 (Parser s (r->u) t) (     Parser s r t)

class Orr c
where	(<-!>) infixr 4 :: !(c s r u t) (Parser s r t) -> Parser s r t

(<!>) infixr 4 :: (Parser s r t) (Parser s r t) -> Parser s r t
(<!>) p1 p2   =                   \hy sc ac xc sg ss -> p1 hy sc	(choose ac) (\sg` -> p2 hy sc ac xc sg` ss) sg ss

instance Orr MonadicSeq
where	(<-!>) :: !(MonadicSeq s r u t) (Parser s r t) -> Parser s r t
		(<-!>) (p1 <&->  wp) p2 = \hy sc ac xc sg ss -> p1 hy (\r ac1 xc1 -> wp r hy sc ac1 xc)
																	(choose ac) (\sg` -> p2 hy sc ac xc sg` ss) sg ss

instance Orr ArrowSeq
where	(<-!>) :: !(ArrowSeq s r u t) (Parser s r t) -> Parser s r t
		(<-!>) (p1 <++-> p2) p3 = \hy sc ac xc sg ss -> p1 hy (\f ac1 xc1 -> p2 hy (\r2 -> sc (f r2)) ac1 xc)
																	(choose ac) (\sg` -> p3 hy sc ac xc sg` ss) sg ss

choose :: (Alt s r) (Xor s r)  Suggestions -> ParseResult s r
choose ac xc sg
	# (res=:(sg,sol))	= xc sg
	| isEmpty sol		= ac empty_xc sg
						= res

// PARSER TRANSFORMERS:

first :: (Parser s r t) -> Parser s r t
first p = \hy sc ac xc sg -> p hy (\r ac` xc -> sc r ac empty_xc) ac xc sg

/*	checkIf: Paying attention to the proper error message will make things work after all this
	way. Say you have a parser that subsequently takes two digits and finally checks whether
	they form a triple. Then the checkIf will get the level description "triple". If you then
	let it parse 3F, you get: "Error [2]: triple [1], and within that: digit"
	If you let it parse 32, you get "Error [..2]: triple [1]"
	So see triple the same way you would regard any other multi-level construct, such as
	an ifstatement, encompassing a condition and two other statements. Now triple is the more
	global level and two digits are the more detailed level. triple covers no ground of its own,
	so what?*/


	
(checkIf) infix 7 :: (Parser s r t) (r -> Bool) -> Parser s r t
(checkIf) p test = p <&> checkIf` test

checkIf` :: (r -> Bool) -> (r -> Parser s r t)
checkIf` test = wp
where	wp r hy sc ac xc sg ss=:(is,i,_,symTypes)
			| test r	= sc r ac empty_xc sg ss
			= ac xc (sg + (symTypes,[hy])@(is++[i-halfPosLength])) 

/*
The following is slightly compromising the idea in error reporting that the hypotheses should
just cumulate the actual alternatives on a certain position. The compromise is that
the test may now only name an alternative if (a part of) a test is violated.
*/
(checkExplain) infix 7 :: (Parser s r t) (r -> Maybe String) -> Parser s r t
(checkExplain) p test = p <&> checkExplain` test

checkExplain` :: (r -> Maybe String) -> (r -> Parser s r t)
checkExplain` test = wp
where	wp r hy sc ac xc sg ss=:(is,i,_,symTypes)
			= case test r of
				Nothing		= sc r ac empty_xc sg ss
//The following sneaks in the extra error message produced by the test
				(Just hyl)	= ac xc (sg + (symTypes,[[(hyl,is++[i-halfPosLength])]:[hy]])@(is++[i-halfPosLength]))

from ParsersDerived import <&
rewind :: (Parser s r t) -> Parser s r t
rewind p = getParsable <&> \pp -> p <& setParsable pp

dropCheck :: (s -> Bool) (Parser s r t) -> Parser s r t
dropCheck pred p = p2
where	p2 hy sc ac xc sg (is,i,ss,symTypes)
			# (ss,j)	= drop pred ss 0
			= p hy sc ac xc sg (is,i+j,ss,symTypes)

		drop :: (a->Bool) [a] Int -> ([a],!Int)
		drop pred all=:[a:as] i
			| pred a	= drop pred as (i+posLength)
			= (all,i)
		drop pred as i	= (as,i)

atMost :: !Int (Parser s r t) -> Parser s r t
atMost n p	= p1
where	p1 hy sc ac xc sg (is,i,ss,symTypes=:[word,sentence:rSymTypes])
			| n < 0			= ac xc (sg + (symTypes,[hy])@(is++[i]))
			# (firstn,rest)	= splitAt n ss
			# sc1			= \t ac xc` sg (js,j,ss,_) -> sc t ac empty_xc sg (js,j,ss++rest,symTypes)
			# symTypes		= if (length firstn == n) [word,slice sentence n:rSymTypes] symTypes
			= p hy sc1 ac xc sg (is,i,firstn,symTypes)
		p1 _ _ _ _ _ _		= abort "parser-transformer 'atMost' called with fewer than two symboltype-levels"
		
		slice :: SymbolType Int -> SymbolType
		slice (Whole str) n		= Slice str n
		slice (Slice str m) n	= Slice str (min n m)

// PARSER TRANSFORMERS THAT LEAVE AND RE-ENTER THE REALM OF CONTINUATIONS:

from ParsersDerived import <&
from ParsersAccessories import class toString (..), instance toString SymbolType
from ParserLanguage import endOf

drill :: (Parser s r r) String -> Parser [s] r t
drill p subSymbol = p1
where	p1 hy sc ac xc sg (is,i,ss`,symTypes=:[st:_])
			= yieldAll sc ac xc sugs (map f list)
			where	(s,ss)	= case ss` of
						[]		-> ([],[])
						[s:ss]	-> (s,ss)
					ss1	= (is++[i],startPos,s,[Whole subSymbol:symTypes])
					end = endOf +++ " " +++ toString st
					(sugs,list) = (p <& end :> empty) hy init_sc init_ac init_xc sg ss1
					init_sc		= \r ac xc sg ss -> let (sg`,rs) = ac xc sg in (sg`,[(ss,r):rs])
					init_ac		= \xc sg -> xc sg
					init_xc		= empty_xc
					f (parsable,res) = ((is,i+posLength,ss,symTypes),res)
		p1 hy sc ac xc sg _ = abort "parser-transformer 'drill' called with empty symbol types"

yieldAll :: (Succ s r t) (Alt s t) (Xor s t) Suggestions [(Parsable s,r)] -> ParseResult s t
yieldAll sc ac xc sg [(ss`,r):rs]	= sc r (\xc` sg` -> yieldAll sc ac xc` sg` rs) empty_xc sg ss`
yieldAll sc ac xc sg []				= ac xc sg

sortResultBy :: (r r -> Bool)  (Parser s r r) -> Parser s r t
sortResultBy less  p = p1
where	p1 hy sc ac xc sg ss
			# (sg`,rs)	= parse` p hy sg ss
			= yieldAll sc ac xc sg` (sortBy (\(_,r1) (_,r2) -> less r1 r2) rs)

minListByAll :: (a a -> Bool) !.[a] -> [a]
minListByAll less [a:as] = min1 a [] as
where	min1 a cum [b:bs]
			| less a b	= min1 a cum	 bs	// b too great: skip
			| less b a	= min1 b []		 bs	// b smaller: start a new cumulation 
						= min1 a [b:cum] bs	// equal: add to list of results
		min1 a cum []	= [a:cum]
minListByAll less [] = []

minResultBy :: (r r -> Bool) (Parser s r r) -> Parser s r t
minResultBy less p	= p1
where	p1 hy sc ac xc sg ss
			# (sg`,rs)	= parse` p hy sg ss
			= yieldAll sc ac xc sg` (minListByAll (\(_,r1) (_,r2) -> less r1 r2) rs)

fstLonger :: (Parsable s,r) (Parsable s,r) -> Bool
fstLonger ((is1,i1,_,_),_) ((is2,i2,_,_),_) = is1 == is2 && i1 > i2 || is1 > is2

longest :: (Parser s r r) -> Parser s r t
longest p	= p1
where	p1 hy sc ac xc sg ss
			# (sg`,rs)	= parse` p hy sg ss
			= yieldAll sc ac xc sg` (minListByAll fstLonger rs)

// FOR ERROR REPORTING:

// To add a hypothesis level to a parser
/*	It adds a describing string and the starting-position to the hypothesis,
	so that you can get a suggestion like:
	
	in the 'function definition' that starts at position 34, and within that
	in the 'assignment statement' that starts at position 45, and within that
	in the 'expression' that starts at position 56, and within that
	at position 61 the parser expects either a '(' or a number.    */

(:>) infixl 8 :: String (Parser s r t) -> Parser s r t
(:>) hyl p = \hy sc ac xc sg ss=:(is,i,_,_) -> p [(hyl,is++[i]):hy] sc ac xc sg ss

/*	To add a hypothesis level to a wanting parser, with the possibility to use the wanted
	result in the hypothesis level description */
(:=>) infixl 8 :: (r -> String) (r -> Parser s r t) -> (r -> Parser s r t)
(:=>) whyl wp = \r -> whyl r :> wp r	// whyl = wanting hypothesis level

// FOR APPLYING A PARSER TO AN INPUT

:: Result r	= Err SymbolTypes (Rose (String,[SugPosition])) [SugPosition] | Succ [r]

:: RoseNode a = RoseLeaf | RoseTwig a (Rose a)
	// RoseLeaf indicates where a twig ends, so the following are indeed different
	// [RoseTwig 1 [RoseTwig 3 [RoseLeaf,RoseTwig 4 [RoseLeaf]]]]
	// this contains a path [1,3] and a path [1,3,4] (both are closed by a RoseLeaf)
	
:: Rose a :== [RoseNode a]

toRose :: [[a]] -> Rose a | == a
toRose as = foldl addPath [] as
where	addPath :: (Rose a) [a] -> Rose a | == a
		addPath []					 [a:as]			 = [RoseTwig a (addPath [] as)] 
		addPath rose				 []	   			 = [RoseLeaf:rose]
		addPath [RoseTwig b bs:rest] [a:as] | a == b = [RoseTwig b (addPath bs as):rest]
		addPath [e	 		  :rest] as				 = [e:addPath rest as]

//parse` for internal use only

parse` :: !(Parser s r r) Hypothesis Suggestions (Parsable s) -> ParseResult s r
parse` p hy sg ss = p hy sc ac xc sg ss
where	sc :: Succ s r r			// final success-continuation
		sc	= \r ac xc sg ss -> let (sg`,rs) = ac xc sg in (sg`,[(ss,r):rs])
		ac :: Alt s r				// final alternative continuation
		ac	= \xc sg -> xc sg
		xc :: Xor s r				// final xor continuation
		xc	= empty_xc

parse :: !(Parser s r r) [s] String String -> Result r
parse p syms sentence word = prettify (parse` p hy sg ([],startPos,syms,symTypes))
where	hy :: Hypothesis
		hy	= []
		
		sg :: Suggestions
		sg	= (symTypes,[])@startErrorPos
		
		symTypes :: [SymbolType]
		symTypes = [Whole word,Whole sentence]

		prettify :: (ParseResult s r) -> Result r
		prettify (sug,[])
			# ((symTypes,hyps)@i) = sug	// do not pattern-match in sug - that would be too strict
			# hyps	= map reverse hyps
					/*	hyps :: [[(String,[Int])]]
						one Hypothesis :: [(String,[Int]] is a path; the complete hyps is a number
						of hypotheses, so hyps is a list of lists. Because individual paths grow
						from coarse to fine by prepending ever more detailed parse-items, paths
						must be reversed for error reporting. */
			# hyps	= map (map toSugPos`) hyps
					/*	Go inside each hypothesis and convert each Int to a
						SugPosition: 1 -> EndAt 0, 2 -> At 1, 3 -> EndAt 1 etc. */
			= Err symTypes (toRose hyps) (map toSugPos i)
					/*	Factor out prefixes (making a Rose) and convert error position to
						End / At notation. The error position list is already in the right order */
		prettify (_,suc)	= Succ (map snd suc)