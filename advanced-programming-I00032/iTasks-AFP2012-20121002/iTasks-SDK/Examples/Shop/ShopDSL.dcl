definition module ShopDSL

import iTasks, ShopDSLboilerplate
import CommonDomain

//	The Domain Specific Language for the shop workflow case.
class Product a | nameOf, priceOf, id_Of, inStockOf a
class InCart  a | nameOf, priceOf, amountOrderedOf  a

:: Book				=	{ id				:: !Hidden (DBRef Book)
						, title				:: !String
						, author            :: !String
						, price				:: !Currency
						, inStock			:: !Int
						}
:: Cart     item	:== [CartItem item]
:: CartItem	item	=	{ itemNr			:: !DBRef item
						, name				:: !String
						, inStock			:: !Int
						, amountOrdered		:: !Int
						, price				:: !Currency
						}
:: CartAmount		=	{ orderAmount		:: !Int
						}
:: Order item		=	{ id				:: !DBRef (Order item)
						, name				:: !String
						, itemsOrdered		:: !Cart item
						, billingAddress	:: !Address
						, shippingAddress	:: !Address
						}
:: Address			=	{ street			:: !String
						, number			:: !String
						, postalCode		:: !String
						, city				:: !String
						}
:: InCart			=	{ name				:: !String
						, amountOrdered		:: !Int
						, price				:: !Currency
						}
:: ShopAction		=	LeaveShop | ToCart | ToPay | ToShop

:: ProductSearch	=	{ name		:: !String
						, amount	:: !Int
						, note		:: !Note
						}


defaultProduct		:: Task Book
defaultCart			:: Task (Cart Book)

//	Conversions between DSL data types:
toCartItem			:: a -> CartItem a | Product a
toInCart			:: a -> InCart     | InCart  a

//	Database operations on DSL data types:
instance DB Book
instance DB (Order a)

eqItemNr			:: !(CartItem item) !(CartItem item) -> Bool
totalCost			:: [a] -> Currency | priceOf, amountOrderedOf a
shopOwner			:: UserName
