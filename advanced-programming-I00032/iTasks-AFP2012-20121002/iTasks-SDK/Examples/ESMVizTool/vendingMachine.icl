module vendingMachine

import ESMVizTool
import StdMisc

Start :: *World -> *World
Start world		= esmVizTool vendingESM world

//Start = testnm 10000 100 (pFair vSpec) // CE
//Start = testnm 10000 100 (pProduct vSpec) // CE
//Start = testnm 1000 100 (pAllProducts vSpec) // CE
//Start = testnm 10000 100 (pFair vSpec2) // Proof, even for Max = 4000, 1.2 sec
//Start = quietnm 10000 100 aStream (pFair vSpec2) // Proof, even for Max = 4000, 0.12 sec
//Start = testnm 10000 100 (pProduct vSpec2) // Proof, even for Max = 4000, 0.8 sec
//Start = testnm 1000 100 (pAllProducts vSpec2) // Proof, even for Max = 4000, 0.01 sec

:: State		= Off | On Int
:: Input		= SwitchOn | SwitchOff | Coin Int | But Product
:: Product		= Coffee | Tea | Chocolate
:: Output		= Cup Product | Return Int

vendingESM :: ESM State Input Output
vendingESM = { s_0 = Off, d_F = vSpec, out = undef, pred = healthy }

vSpec :: !State !Input -> [Trans Output State]
vSpec Off SwitchOn  = [Pt [] (On 0)]
vSpec s   SwitchOff = [Pt [] Off]
vSpec (On s) (Coin c)
 | s<Max	= [Pt [] (On (s+c))]				// s+c<=Max
			= [Pt [] (On s)]					// output should be Return c // pFair
vSpec (On s) (But Coffee)
 | s>=20	= [Pt [Cup Coffee] (On (s-20))]
vSpec (On s) (But Tea)
 | s>=10	= [Pt [Cup Coffee] (On (s-10))]		// We get Coffee instead of Tea // pProduct, pAllProducts
vSpec (On s) (But p) = [Pt [] (On s)]
vSpec state input = [] // [Pt [] state]

vSpec2 :: !State !Input -> [Trans Output State]
vSpec2 Off SwitchOn  = [Pt [] (On 0)]
vSpec2 s   SwitchOff = [Pt [Return (value s)] Off]
vSpec2 (On s) (Coin c)
 | s+c<=Max		= [Pt [] (On (s+c))]
				= [Pt [Return c] (On s)]
vSpec2 (On s) (But p)
 | s>=value p	= [Pt [Cup p] (On (s-value p))]
 				= [Pt [] (On s)]
vSpec2 state input = []

Max = 4000

healthy :: (SeenTrans State Input Output) -> [[String]]
healthy (s,i,o,t)
	= [if (vs+vi == vo+vt)
		[]
		["value is not preserved in this transition, value s+value i=",toString (vs+vi),", and value o+value t=",toString (vo+vt)]
	  ,if (vt>Max)
	   	["Value of target state ",toString vt," larger than Max (",toString Max,")."]
	   	[]
	  ,case (i,o) of
	  	(But p,[Cup q]) | p =!= q = ["The required product is unequal to the delivered product!"]
	  	_ = []
	  ]
where vs = value s; vi = value i; vo = value o; vt = value t

class value a :: a -> Int

instance value State
where
	value (On s) = s
	value Off    = 0
instance value Product
where
	value Tea		 = 10
	value Coffee	 = 20
	value Chocolate	 = 25
instance value Input
where
	value (Coin c)	 = c
	value input 	 = 0
instance value Output
where
	value (Cup p)	 = value p
	value (Return m) = m
instance value [a] | value a where value list = sum (map value list)
instance value (Trans o s) | value o & value s
where
	value (Pt o s) = value o+value s
	value (Ft f  ) = abort "value (Ft f) is undefined"

pAllProducts :: (Spec State Input Output) Product -> Property
pAllProducts spec p = Exists \s. ~ (isEmpty [p \\ (Pt [Cup q] t) <- spec s (But p)|p===q])
/*
Counterexample 1 found after 2 tests: Tea
Counterexample 2 found after 3 tests: Chocolate
2 errors found, after 3 tests
*/
pProduct :: (Spec State Input Output) State Product -> Property
pProduct spec s p = checkProd For spec s (But p)
where
	checkProd (Pt [Cup q] t) = p === q
	checkProd _              = True
/*
Counterexample 1 found after 5 tests: (On 10) Tea (Pt ([Cup Coffee] (On 0)))
Counterexample 2 found after 9 tests: (On 15) Tea (Pt ([Cup Coffee] (On 5)))
Counterexample 3 found after 15 tests: (On 25) Chocolate (Pt ([Cup Coffee] (On 5)))
Counterexample 4 found after 16 tests: (On 30) Tea (Pt ([Cup Coffee] (On 10)))
Counterexample 5 found after 18 tests: (On 20) Tea (Pt ([Cup Coffee] (On 0)))
Counterexample 6 found after 26 tests: (On 35) Chocolate (Pt ([Cup Coffee] (On 15)))
Counterexample 7 found after 28 tests: (On 20) Chocolate (Pt ([Cup Coffee] (On 0)))
Counterexample 8 found after 32 tests: (On 40) Tea (Pt ([Cup Coffee] (On 20)))
Counterexample 9 found after 34 tests: (On 25) Tea (Pt ([Cup Coffee] (On 5)))
Counterexample 10 found after 35 tests: (On 30) Chocolate (Pt ([Cup Coffee] (On 10)))
Counterexample 11 found after 36 tests: (On 35) Tea (Pt ([Cup Coffee] (On 15)))
Counterexample 12 found after 41 tests: (On 40) Chocolate (Pt ([Cup Coffee] (On 20)))
12 errors found, after 42 tests
*/
pFair :: (Spec State Input Output) State Input -> Property
pFair spec s i = p For spec s i
where p t = value s + value i == value t

pNoDebit :: State Input -> Property
pNoDebit Off i = prop True
pNoDebit (On n) i
 = n>=0 ==> (isPos For vSpec (On n) i)
where
	isPos (Pt _ (On t)) = t >= 0
	isPos _ = True
/*
Counterexample 1 found after 5 tests: (On 10) Tea (Pt ([Cup Coffee] (On 0)))
Counterexample 2 found after 9 tests: (On 15) Tea (Pt ([Cup Coffee] (On 5)))
Counterexample 3 found after 15 tests: (On 25) Chocolate (Pt ([Cup Coffee] (On 5)))
Counterexample 4 found after 16 tests: (On 30) Tea (Pt ([Cup Coffee] (On 10)))
Counterexample 5 found after 18 tests: (On 20) Tea (Pt ([Cup Coffee] (On 0)))
Counterexample 6 found after 26 tests: (On 35) Chocolate (Pt ([Cup Coffee] (On 15)))
Counterexample 7 found after 28 tests: (On 20) Chocolate (Pt ([Cup Coffee] (On 0)))
Counterexample 8 found after 32 tests: (On 40) Tea (Pt ([Cup Coffee] (On 20)))
Counterexample 9 found after 34 tests: (On 25) Tea (Pt ([Cup Coffee] (On 5)))
Counterexample 10 found after 35 tests: (On 30) Chocolate (Pt ([Cup Coffee] (On 10)))
Counterexample 11 found after 36 tests: (On 35) Tea (Pt ([Cup Coffee] (On 15)))
Counterexample 12 found after 41 tests: (On 40) Chocolate (Pt ([Cup Coffee] (On 20)))
12 errors found, after 42 tests
*/
derive class iTask  State, Input, Output, Product
/*
gEq{|State|} (On n) (On m) = n==m || n>=max && m>=max where max = 50
gEq{|State|} Off Off = True
gEq{|State|} s t = False
derive gEq Input, Output, Product
*/
instance == State where (==) s t = s === t

derive bimap []
ggen{|Input|} _ _ = [SwitchOn,SwitchOff:[Coin c\\c<-[10,20]]++[But p\\p<-[Coffee,Tea]]]
ggen{|State|} _ _ = [Off:[On v\\v<-[0,5..Max]]]
derive genShow State, Input, Output, Product
derive ggen Output, Product
ggen{|Trans|} f g _ _ = undef

instance render State //where render s = toHtmlString s
where
//	render (On n)	= toString n
	render state	= toHtmlString state
instance render Input   where render i			= toHtmlString i
instance render Output  where render o			= toHtmlString o
