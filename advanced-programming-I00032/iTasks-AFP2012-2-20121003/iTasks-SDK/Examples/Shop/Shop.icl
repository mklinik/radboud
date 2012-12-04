module Shop

import StdClass, StdList
from   StdFunc import o, flip
import iTasks
import StdListExt, ShopDSL

Start :: *World -> *World
Start world = startEngine shopFlows world

shopFlows :: [Workflow]
shopFlows 
= [ { name		= "Catalog/Manage catalog"
    , label		= "Manage catalog"
    , roles		= []
    , mainTask	= defaultProduct >>= \defProd -> manageCatalog defProd >>| stop
    }
  , { name		= "Catalog/Browse shop"
	, label		= "Browse shop"
	, roles		= []
	, mainTask	= defaultCart >>= \defCart -> browseShop defCart
    }
  , { name		= "Suppliers/Manage suppliers"
    , label		= "Manage suppliers"
    , roles		= []
    , mainTask	= return Void
  	}
  ,	{ name		= "Suppliers/Special order"
  	, label		= "Special order"
  	, roles		= []
  	, mainTask	= specialProductSearch
  	}
  ]

// The shop management workflow:

ACTION_ADD	:== ActionLabel "Add item"
ACTION_EDIT	:== ActionLabel "Edit item"
ACTION_DELETE :== ActionLabel "Delete item"

manageCatalog :: a -> Task (Maybe a) | iTask, DB a
manageCatalog _ 
	= (dbReadAll >>= browseCatalog) <! isNothing
where
	browseCatalog :: [a] -> Task (Maybe a) | iTask, DB a
	browseCatalog [] =
		showMessageA "The catalog is empty." [ButtonAction (ACTION_ADD,Always),ButtonAction (ActionQuit,Always)]
		>>= \action -> case action of
			ACTION_ADD	= continue createItem
			ActionQuit	= return Nothing
	browseCatalog items =
		enterChoiceA "Manage the catalog" [ButtonAction (ACTION_EDIT,IfValid)
										  ,ButtonAction (ACTION_DELETE,IfValid)
										  ,ButtonAction (ACTION_ADD,Always)
										  ,ButtonAction (ActionQuit,Always)
										  ] items
		>>= \(action,item) -> case action of
			ACTION_EDIT		= continue (updateItem item)
			ACTION_DELETE	= continue (deleteItem item)
			ACTION_ADD		= continue createItem
			ActionQuit		= return Nothing
			
	createItem :: Task a | iTask, DB a
	createItem = enterInformation "Enter details of new item:" >>= dbCreateItem

	updateItem :: a -> Task a | iTask, DB a
	updateItem item = updateInformation "Please update the following item:" item >>= dbUpdateItem
	
	deleteItem :: a -> Task a | iTask, DB a
	deleteItem item = dbDeleteItem (getItemId item) >>| return item
	
	continue :: (Task a) -> Task (Maybe a) | iTask a
	continue t = t >>= \i -> return (Just i)
	
// The customer workflow:		
browseShop :: (Cart a) -> Task Void | iTask, DB, Product a
browseShop initCart 
	= dbReadAll      >>= \items ->
	  doShopping initCart items >>= 
	  doAction   initCart items
where
	doShopping :: (Cart a) [a] -> Task (ShopAction,Cart a) | iTask, Product a
	doShopping cart []
		= showMessage [normalText "Currently no items in catalogue, sorry."]
		  >>| return (LeaveShop,cart)
	doShopping cart items
		= anyTask
			[ navigateShop cart
			: map (itemActions cart) items					
			] 
	where
		itemActions :: (Cart a) a -> Task (ShopAction,Cart a) | iTask, Product a
		itemActions cart item
			=	showMessageAbout "Add to cart" item
			>>|	return (ToCart, add (toCartItem item) cart)
		where
			add :: (CartItem a) [CartItem a] -> [CartItem a]
			add new []				= [new]
			add new [item:items]	= if (eqItemNr new item) 
									     [amountOrderedUpd item (amountOrderedOf item+1):items]
									     [item:add new items]

	navigateShop :: (Cart a) -> Task (ShopAction, Cart a) | iTask a
	navigateShop cart
		= (enterChoice "" 
			[ return (ToShop,   cart) <<@ "Do Shopping"
			, return (ToPay,    cart) <<@ "Check Out And Pay"
			, return (ToCart,   cart) <<@ "Show Cart"
			, return (LeaveShop,cart) <<@ "Leave Shop"
			] >>= \task -> task)
			  -||-
			  (showStickyMessageAbout [BrTag [], boldText "Total cost of ordered items = "] (totalCost cart) >>| getDefaultValue)
			     
	doAction :: (Cart a) [a] (ShopAction, Cart a) -> Task Void | iTask, DB, Product a
	doAction initCart items (action,cart)
		= case action of
			LeaveShop	= return Void
			ToCart		= showCart   cart       >>= doAction initCart items
			ToShop		= doShopping cart items >>= doAction initCart items
			ToPay		= checkOutAndPay cart   >>| browseShop initCart
	
	showCart :: (Cart a) -> Task (ShopAction,Cart a) | iTask a
	showCart cart=:[]
		= navigateShop cart -||- (showStickyMessage [normalText "No items in cart yet!"] >>| getDefaultValue)
	showCart cart
		= anyTask
			[ navigateShop cart : map (itemActions cart) cart ]
	where
		itemActions :: (Cart a) (CartItem a) -> Task (ShopAction,Cart a) | iTask a
		itemActions cart item
			= 	showStickyMessageAbout "Item: " item
				||-
			 	updateInformation "Change amount: " {orderAmount = amountOrderedOf item}
				>>= \{orderAmount=n} -> 
					return (ToCart,if (n <= 0) (filter (not o eqItemNr item) cart)
				                               (lreplace eqItemNr (amountOrderedUpd item n) cart)
				           )
	
	checkOutAndPay :: (Cart a) -> Task Void | iTask a
	checkOutAndPay cart 
		= getDefaultValue >>= \order -> 
		  fillInClientInfo {order & itemsOrdered = cart} >>= \order ->
		  showMessageA [normalText "Confirm Order"] [ButtonAction (ActionYes,Always),ButtonAction (ActionNo,Always)]
		  >>= \action -> case action of
		  	ActionYes =
			    (dbCreateItem order >>|
			     getCurrentUser >>= \me -> 
				 spawnProcess me.User.userName True (showMessageAbout "Order:" (order,costOrder order) <<@ "Order Confirmed")
				 >>|	
				 spawnProcess shopOwner True (showMessageAbout "Order" (order,costOrder order) <<@ "New Order from " <+++ me.User.displayName) 
				 >>| return Void
				)
			ActionNo = 
				(return Void)
	where
		costOrder order	= totalCost order.itemsOrdered
	
		fillInClientInfo order
			= fillInData name_prompt nameOf              nameUpd           order >>= \order ->
			  fillInData billing_prompt billingAddressOf billingAddressUpd order >>= \order ->
			 
			  showMessageA ([ normalText "Billing address:" :visualizeAsHtmlDisplay (billingAddressOf order)
			          ] ++ [ normalText "Is the shipping addres same as the billing address above?"
			          ]) [ButtonAction (ActionYes,Always),ButtonAction (ActionNo,Always)] >>= \action ->
			          (case action of
			          	ActionYes	= (return {order & shippingAddress = billingAddressOf order})
			            ActionNo	= (fillInData shipping_prompt shippingAddressOf shippingAddressUpd order)
			           ) >>= \order ->
			  showMessageA (showOrder order) [ButtonAction (ActionYes,Always),ButtonAction (ActionNo,Always)] >>= \action ->
			  	case action of
			  		ActionYes	= return order
			  		ActionNo	= fillInClientInfo order
		where
			name_prompt     = "Please fill in your name:"
			billing_prompt  = "Please fill in the billing address:"
			shipping_prompt = "Please fill in the shipping address:"
			
			fillInData prompt valueOf updateOf record
				= updateInformation [normalText prompt] (valueOf record) >>= \value -> 
				  return (updateOf record value)
			
			showOrder order
				= section "name:"             (visualizeAsHtmlDisplay (nameOf            order)) ++
				  section "billing address:"  (visualizeAsHtmlDisplay (billingAddressOf  order)) ++
				  section "shipping address:" (visualizeAsHtmlDisplay (shippingAddressOf order)) ++
				  section "ordered items:"    (flatten (map (visualizeAsHtmlDisplay o toInCart) order.itemsOrdered) ++ [DivTag [] (visualizeAsHtmlDisplay (costOrder order))]) ++
				  section "Confirm:"          [normalText "Is the data above correct?"]

// Special order from suppliers
specialProductSearch :: Task Void 
specialProductSearch =
	defineSearch					>>=	\search		->
	selectSuppliers 				>>=	\suppliers	->
	collectBids	search suppliers	>>= \bids		->
	selectBid bids	 				>>= \bid		->
	confirmBid search bid
	
defineSearch :: Task ProductSearch
defineSearch = enterInformation "Please describe the item you are looking for"
	
selectSuppliers :: Task [User]
selectSuppliers
	=	getUsersWithRole "supplier"
	>>= enterMultipleChoice "Select the suppliers you want to ask"
		
collectBids :: ProductSearch [User] -> Task [(User,Currency)]
collectBids search suppliers
	= allTasks
		["Bid for " +++ search.ProductSearch.name +++ " from " +++ supplier.User.displayName
		 @>> (supplier @: ("Bid request regarding " +++ search.ProductSearch.name, collectBid search supplier))
		 \\ supplier <- suppliers]
where
	collectBid :: ProductSearch User -> Task (User,Currency)
	collectBid search bid
		=	enterInformationAbout
				"Please make a bid to supply the following product"
				search
				>>= \price -> return (bid,price)  	
	
selectBid :: [(User,Currency)] -> Task (User,Currency)
selectBid bids
	=	determineCheapest bids	>>= \cheapestBid=:(supplier,price) ->	
		requestConfirmation
			[ Text "The cheapest bid is ", Text (toString price), Text " by ", Text supplier.User.displayName, BrTag [],
	  		  Text "Do you want to accept this bid?"
	  		] >>= \acceptCheapest ->
		if acceptCheapest
			( return cheapestBid )
			( enterChoice "Please select one of the following bids" bids )
where
	determineCheapest bids = return (hd (sortBy (\(_,x) (_,y) -> x < y) bids))
	
confirmBid :: ProductSearch (User,Currency) -> Task Void
confirmBid search bid =: (user, price)
	= user @: ("Bid confirmation", showMessage [Text "Your bid of ", Text (toString price),Text " for the product ",ITag [] [Text search.ProductSearch.name], Text " has been accepted."])


// little markup functions:

boldText   text				= BTag    [] [Text text, BrTag [], BrTag []]
normalText text				= DivTag [] [Text text, BrTag [], BrTag []]
ruleText   text				= section text [HrTag []]
section    label content	= [HrTag  [], boldText label : content]