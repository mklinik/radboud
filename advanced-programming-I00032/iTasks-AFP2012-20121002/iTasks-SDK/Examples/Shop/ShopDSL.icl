implementation module ShopDSL

import StdClass, StdInt, StdList
import GenBimap
import iTasks
import ShopDSLboilerplate

toCartItem :: a -> CartItem a | Product a
toCartItem p			= { itemNr        = id_Of     p
						  , name          = nameOf    p
						  , inStock       = inStockOf p
						  , amountOrdered = if (inStockOf p >= 0) 1 0
						  , price         = priceOf p
						  }
toInCart :: a -> InCart | InCart a
toInCart p				= { InCart
						  | name          = nameOf          p
						  , amountOrdered = amountOrderedOf p
						  , price         = priceOf         p
						  }
instance DB Book where
	databaseId			= mkDBid "books"
	getItemId item		= id_Of  item
	setItemId id item	= id_Upd item id
instance DB (Order item) where
	databaseId			= mkDBid "orders"
	getItemId item		= id_Of  item
	setItemId id item	= id_Upd item id


defaultProduct :: Task Book
defaultProduct			= getDefaultValue

defaultCart :: Task (Cart Book)
defaultCart				= getDefaultValue

eqItemNr :: !(CartItem a) !(CartItem a) -> Bool
eqItemNr x y			= x.itemNr == y.itemNr

totalCost :: [a] -> Currency | priceOf, amountOrderedOf a
totalCost set			= EUR (sum [amountOrderedOf item * toInt (priceOf item) \\ item <- set])

shopOwner :: UserName
shopOwner				= "root"
