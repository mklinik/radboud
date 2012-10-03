implementation module Coffeemachine

import iTasks

coffeemachineExample :: [Workflow]
coffeemachineExample = [workflow "Examples/Miscellaneous/Coffeemachine" "A coffee machine demo" coffeemachine]

coffeemachine :: Task (String,EUR)
coffeemachine  =				enterChoice ("Product","Choose your product") []
									[("Coffee", EUR 100)
									,("Cappucino", EUR 150)
									,("Tea", EUR 50)
									,("Chocolate", EUR 100)
									] 
	>>= \(product,toPay) ->		getCoins product (toPay,EUR 0)

getCoins :: String (EUR,EUR) -> Task (String,EUR)
getCoins product (cost,paid) = getCoins`
where
	getCoins`		
		=			
					viewInformation "Status" [] 
						(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("To pay: " <+++ cost)]) 
			||-		enterChoice  ("Insert coins","Please insert a coin...") [] coins
			>>*		[AnyTime ActionCancel (\_ -> show "Cancelled" paid)
					,WithResult ActionOk (const True) handleMoney
					]
	coins	= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

	handleMoney coin 
	| cost > coin	= getCoins product (cost-coin, paid+coin)
	| otherwise		= show product (coin-cost)
	
	show product money = viewInformation "Coffeemaker+" [ViewWith (\(product,money) -> ("product = " <+++ product <+++ ", money returned = " <+++ money))] (product,money)
