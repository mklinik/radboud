module FNWIvending

import ESMVizTool
import StdMisc

Start :: *World -> *World
Start world	= esmVizTool FNWIvendingESM world

:: State	= { products :: [Product], choice :: Int, money :: Int}
:: Input	= Coin Coin | But Digit | ButOK | ButRst | GoToErrorState
:: Product	= { id :: Int, descr :: String, price :: Int }
:: Output	= Yield String | Return [Coin] | Message String
:: Digit = D Int // numbers 0..9
:: Coin  = C Int // 5, 10, 20, 50, 100, 200 cents


derive class iTask	State, Input, Product, Output, Digit, Coin
//derive gEq			State, Input, Product, Output // from derive class iTask
derive ggen			State, Input, Product, Output
ggen{|Coin|}  _ _ = map C [5,10,20,50,100,200]
ggen{|Digit|} _ _ = map D [0..9]
derive genShow		State, Input, Product, Output
genShow{|Digit|} s p (D i) c = [toString i:c]
genShow{|Coin|}  s p (C i) c = [toString i:c]
instance render State	where render {choice,money}	= toHtmlString (toString choice + " " + toString (toReal money/100.0))
instance render Input   where render i		= show1 i
instance render Output  
where
	render (Yield s)	= s
	render (Return l)	= show1 l
	render (Message m)	= m
derive bimap []

instance == State where (==) s t = s === t

state0 :: State
state0 = {products = products, choice = 0, money = 0}

products =
	[{id = 53, descr = "Harlekijntjes", price = 155}
	,{id = 54, descr = "Autodrop", price = 155}
	,{id = 72, descr = "Mars", price = 140}
	,{id = 31, descr = "Paprika chips", price = 120}
	]

FNWIvendingESM :: ESM State Input Output
FNWIvendingESM = { s_0 =state0, d_F = fnwiSpec, out = \s i.[[],[Message "??"]], pred = \_.[] }

fnwiSpec :: State Input -> [Trans Output State]
fnwiSpec st i | st.choice < 0 = [] // to test the tool
fnwiSpec st (Coin (C c))
	# money = st.money + c
	= [Pt [] {st & money = money}]
fnwiSpec st (But (D i))
	| st.choice == 0 && not (isEmpty [p \\ p <- st.products | (p.id / 10) == i])
		= [Pt [Message (toString i)] {st & choice = i}]
	# choice = 10 * st.choice + i
	= case [p \\ p <- st.products | p.id == choice] of
		[p] = [Pt [Message p.descr] {st & choice = choice}]
			= []
fnwiSpec st GoToErrorState = [Pt [] {st & choice = -1, money = 0}] // to test the tool
fnwiSpec st ButRst = [Pt [Return [C st.money]] {st & choice = 0, money = 0}]
fnwiSpec st ButOK
	= case [p \\ p <- st.products | p.id == st.choice && p.price <= st.money] of
		[p]	# money = st.money - p.price
			= [Pt [Yield p.descr: if (money == 0) [] [Return [C money]]] {st & choice = 0, money = 0}
			  ,Ft \out. case out of [Message m] = [{st & choice = 0}]; = []]
		[]	= [Pt [] st]
		_	= []
fnwiSpec st input = []


