module CoffeeTeaBoundedCoinsMachine

import iTasks
import ESMVizTool
import StdMisc

Start :: *World -> *World
Start world		= esmVizTool coffeeNcoins world

/**************************************************************************************
*
*	The concrete example: a coffeemachine that accepts a bounded nr of coins.
*
**************************************************************************************/

::	State	= Coins Int
::	Input	= Coin 	| But Product | Surprise
::	Output	= Cup Product | Change Int
::	Product	= Tea	| Coffee

coffeeNcoins :: ESM State Input Output
coffeeNcoins = { s_0 = Coins 0, d_F = coffeeNcoins`, out = undef, pred = healthy}
where
	coffeeNcoins` :: State Input			-> [Trans Output State]
	coffeeNcoins` (Coins n) Coin | n<=Max	= [Pt [] (Coins (n+1))]
	coffeeNcoins` (Coins n) (But Tea)		= if (n>=2) [Pt [Cup Tea]    (Coins (n-2))] []
	coffeeNcoins` (Coins n) (But Coffee)	= if (n>=3) [Pt [Cup Coffee] (Coins (n-3))] []
	coffeeNcoins` (Coins n) Surprise		= if (n>=3) [Pt [Cup Tea]    (Coins (n-2))
											            ,Pt [Cup Coffee] (Coins (n-2))]
											  (if (n>=2) [Pt [Cup Tea]   (Coins (n-2))] [])
	coffeeNcoins` state			_ 			= [Pt [] state]

Max = 4
	
healthy :: (SeenTrans State Input Output) -> [[String]]
healthy (s,i,o,t)
 =	[ if (vs+vi == vo+vt)
			[]
			[ "value is not preserved in this transition"
			, " value s+value i= ", toString (vs+vi)
			, " and value o+value t= ", toString (vo+vt)]
	, if (vt>Max)
	   	["Value of target state ",toString vt," larger than Max."]
	   	[]
	, case (i,o) of
	  	(But p,[Cup q]) | p =!= q
				= ["The required product is not the delivered product!"]
	  	_ 	= []
	]
where vs = value s; vi = value i; vo = value o; vt = value t

class value x :: x -> Int
instance value State  where value (Coins n) = n
instance value Input  where value Coin = 1; value _ = 0
instance value Output where value (Cup p) = value p; value (Change c) = c
instance value Product where value Tea = 2; value Coffee = 3
instance value [x] | value x where value list = sum (map value list)

derive bimap (,), Maybe, []
derive class iTask State, Input, Output, Product
derive genShow State

//derive gEq    State, Input, Output, Product
derive ggen   Input, Product

instance render	State	where render s	= visualizeAsText AsDisplay s
instance render	Input	where render i	= visualizeAsText AsDisplay i
instance render	Output	where render e	= visualizeAsText AsDisplay e
instance ==	State	where (==) s1 s2 = n1 < minbound && n2 < minbound || n1 > maxbound && n2 > maxbound || n1==n2
                                	  where   n1 = toInt s1; n2 = toInt s2
instance == Input	where (==) i1 i2 = gEq{|*|} i1 i2
instance == Output	where (==) e1 e2 = gEq{|*|} e1 e2

instance toInt	State	where toInt (Coins n) = n

minbound = 0
maxbound = 2
