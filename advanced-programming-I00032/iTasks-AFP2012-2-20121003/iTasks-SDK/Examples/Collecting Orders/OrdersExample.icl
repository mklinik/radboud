module OrdersExample

import iTasks

derive class iTask Item, Order, OrderState, SupOrder, SupOrderState
derive bimap (,), Maybe

// Order Types

:: Order a			= 	{ item  	  :: [a]		
						, requester	  :: Requester
						, buyer		  :: Buyer	
						, orderNumber :: OrderNumber a
						, orderStatus :: [TimeStamp OrderState]					
						, supOrders	  :: [SupOrder a]	
	  					}
:: Requester 		:== User
:: Buyer	 		:== User
:: OrderNumber a 	:== Int
:: OrderState		=	OrderCreated
					|	SentToBuyer
					|	SuppliersChosen 		
					|	ShippedToRequester
					|	OrderCancelled	
:: TimeStamp a		:== (a,DateTime)
:: SupOrder a		=	{ item			:: [a]
						, supplier		:: Supplier
						, supOrderNumber:: (OrderNumber a, SupOrderNumber a)
						, supOrderStatus:: [TimeStamp SupOrderState]
						}
:: Supplier			:== User
:: SupOrderNumber a	:== Int
:: SupOrderState	=	ToBeSendToSupplier 	
					|	SentToSupplier 		
					|	ShippedToBuyer		
					|	RejectedBySupplier 			
:: OrderId a		:== (OrderNumber a, OrderStore a)
:: SupOrderId a		:== (SupOrderNumber a, OrderNumber a, OrderStore a)

:: OrderStore a 	:== Shared [Order a] [Order a]

// Delayed ordering process administration

:: OrderingProcess	:== [(Requester,Buyer)]
:: OrderingProcessStore
					:== Shared OrderingProcess OrderingProcess

orderingProcessStore :: OrderingProcessStore
orderingProcessStore = sharedStoreDefault "orderingStore"
	  	
// start 

Start world = startEngine [ workflow "Order Items" "Order Example" (Title "Request items..." @>> request itemStore)
						  ] world	  	

// specific instantiations

:: Item
	=	{ description 	:: String
//		, productCode	:: String
//		, color			:: String
		}

:: ItemStore 		:== OrderStore Item

itemStore :: ItemStore
itemStore = sharedStoreDefault "itemOrderStore"


// order storage access utility functions

addOrder ::  Requester Buyer [a] OrderState (OrderStore a) -> Task (OrderNumber a) | iTask a
addOrder requester buyer items orderstate store 
	= 						getCurrentDateTime
		>>= \timestamp ->	readShared store
		>>= \as ->			let orderNumber = length as
							    new			= { item 		= items
							 				  , orderNumber = orderNumber
							  				  , requester 	= requester
							  				  , buyer 		= buyer
							  				  , orderStatus = [(orderstate,timestamp)]
							  				  , supOrders 	= [] 
							  				  }
							in
							    writeShared store (append as new)
							>>| return orderNumber

fetchOrder :: (OrderId a) -> Task (Order a) | iTask a 
fetchOrder (nr, store)
	=						readShared store
		>>= \orders -> 		return (orders!!nr)

updateOrder :: (Order a) (OrderId a) -> Task (Order a) | iTask a 
updateOrder order (nr, store)
	=						updateShared (updateAt nr order) store
		>>|					return order

setOrderState :: OrderState (OrderId a) -> Task (Order a) | iTask a
setOrderState orderState orderId
	=						getCurrentDateTime
		>>= \timestamp ->	fetchOrder orderId
		>>= \order -> 		updateOrder {order & orderStatus = [(orderState,timestamp):order.orderStatus]} orderId

addSupplier :: [a] Supplier SupOrderState (OrderId a) -> Task (SupOrderNumber a) | iTask a
addSupplier items supplier supstate orderId=:(orderNumber, store)
	=						getCurrentDateTime
		>>= \timestamp ->	fetchOrder orderId
		>>= \order ->		let subOrderNumber 	= length order.supOrders
								nsup			= { item 			= items
												  , supplier 		= supplier
												  , supOrderNumber 	= (orderNumber, subOrderNumber)
												  , supOrderStatus 	= [(supstate,timestamp)]
												  }
							in
							    updateOrder {order & supOrders = append order.supOrders nsup} orderId
							>>| return subOrderNumber

changeSupOrderState :: Buyer Supplier SupOrderState SupOrderState (OrderStore a) -> Task [Order a] | iTask a
changeSupOrderState buyer supplier oldstate newstate store
	=						getCurrentDateTime
		>>= \timestamp ->	readShared store
		>>= \orders ->		writeShared store (foldl (update (newstate, timestamp)) orders (fetchOrderNumbers buyer supplier oldstate orders))
where
	update :: (TimeStamp SupOrderState) [Order a] (OrderNumber a, SupOrderNumber a) -> [Order a]
	update nsupstate orders (i,j)
							= updateAt i norder orders
	where
		norder				= {order & supOrders = updateAt j nsuporder order.supOrders}
		nsuporder			= {suporder & supOrderStatus = [nsupstate:suporder.supOrderStatus]}
		suporder			= order.supOrders!!j 
		order				= orders!!i

// utility functions

instance == Item
where
	(==) i1 i2 = i1 === i2

append xs x = xs ++ [x]

selectUserWithRole :: String -> Task User
selectUserWithRole role 
	= 						getUsersWithRole role 
		>>= \users ->		enterChoice ("Choose a " +++ role) (map userName users)
		>>= \name ->		return (NamedUser name)

getProperty :: [TimeStamp a] -> a
getProperty [(p,_):_] = p

myOrder orderNr orders = hd [o \\ o <- orders | o.orderNumber == orderNr]

fetchOutstandingSubOrders :: Buyer Supplier SupOrderState [Order a] -> [SupOrder a]
fetchOutstandingSubOrders buyer supplier supStatus orders 
	= 	[sup 
		\\ o <- orders, sup <- o.supOrders 
		| o.buyer == buyer && sup.supplier == supplier && getProperty sup.supOrderStatus === supStatus] 

fetchOrderNumbers :: Buyer Supplier SupOrderState [Order a] -> [(OrderNumber a, SupOrderNumber a)]
fetchOrderNumbers buyer supplier supStatus orders 
	= 	map (\s -> s.supOrderNumber) (fetchOutstandingSubOrders buyer supplier supStatus orders)


// the workflows:

// requester

request :: (OrderStore a) -> Task Void | iTask a & Eq a
request store
	=						getCurrentUser
		>>= \requester ->	enterInformation "What do you want to order ?"
		>>= \items ->		selectUserWithRole "buyer"
		>>= \buyer ->		addOrder requester buyer items OrderCreated store 
		>>= \orderNr ->		startChooseSuppliers buyer items (orderNr, store)
		>>|					monitor "Status of your order:" (showStatus orderNr) (finished orderNr) False store
		>>|					return Void
where
	showStatus orderNr orders = Display (myOrder orderNr orders)
	finished orderNr   orders = let prop = getProperty (myOrder orderNr orders).orderStatus in
									prop === ShippedToRequester  || prop === OrderCancelled

// buyer

startChooseSuppliers :: Buyer [a] (OrderId a) -> Task Void | iTask a & Eq a
startChooseSuppliers buyer items orderId
	=	spawnProcess True True (buyer @: (Title "Choose suppliers..." @>> chooseWorkflow)) >>| return Void	
where
	chooseWorkflow 
		= 					setOrderState SentToBuyer orderId
			>>|				chooseSuppliers buyer items orderId
				  
chooseSuppliers :: Buyer [a]  (OrderId a) -> Task Void | iTask a & Eq a
chooseSuppliers buyer [] orderId  
	=						setOrderState SuppliersChosen orderId
		>>|					return Void
chooseSuppliers buyer items orderId
	=						( enterMultipleChoice "Select items from list intended for the same supplier " items
							     -&&-
							  selectUserWithRole "supplier"
							)
		>>= \(chosen,supplier) ->
							addSupplier chosen supplier ToBeSendToSupplier orderId
		>>|					startOrderingProcess buyer supplier (snd orderId) 
		>>|					chooseSuppliers buyer (removeMembers items chosen) orderId
	

startOrderingProcess :: Buyer Supplier (OrderStore a) -> Task Void | iTask a
startOrderingProcess buyer supplier store
	=						readShared orderingProcessStore
		>>= \table ->		if (isMember (buyer,supplier) table)
							   (		return Void			// process already active
							   )
							   (		spawnProcess True True (Title "Collecting orders..." @>> buyer @: shipOrder buyer supplier store)
								>>| 	writeShared orderingProcessStore [(buyer,supplier):table]
								>>|		return Void
							   )

shipOrder :: Buyer Supplier (OrderStore a) -> Task Void | iTask a
shipOrder buyer supplier store
	=						monitor ("Orders collected for " <+++ supplier) showSupOrders (const True) False store
		>>|					changeSupOrderState buyer supplier ToBeSendToSupplier SentToSupplier store
		>>= \orders ->		supplier @: deliver (fetchOutstandingSubOrders buyer supplier SentToSupplier orders)
		>>= \ok -> 			changeSupOrderState buyer supplier SentToSupplier (if ok ShippedToBuyer RejectedBySupplier) store
		>>|					return Void
where
	showSupOrders orders = Display (fetchOutstandingSubOrders buyer supplier ToBeSendToSupplier orders)
	
// suppliers

deliver :: [SupOrder a] -> Task Bool | iTask a
deliver subOrders
	=						requestConfirmationAbout "Can you deliver: " subOrders


/* old stuff

:: LineItemStatus
	=	ItemCreated
	|	ItemSentToBuyer
	|	ItemSentToSupplier
	|	ItemManufactured
	|	ItemShippedToBuyer
	|	ItemRejected
	|	ItemCanceled

:: RequisitionOrderStatus
	=	RequisitionCreated
	|	RequisitionSentToBuyer
	|	RequisitionSentToSupplier
	|	RequisitionShippedToBuyer
	|	RequisitionSippedToRequester
	|	RequisitionCanceled
		}
:: ProcurementStatus
	=	ProcurementCreated
	|	ProcurementSentToSupplier
	|	ProcurementOrderAssembled
	|	ProcurementShippedToBuyer
	|	ProcurementRejected
	|	ProcurementCanceled

*/
