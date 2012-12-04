definition module ShopDSLboilerplate

import ShopDSL
import GenVisualize, GenUpdate

//	Generic boilerplate code:
derive gPrint		Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch
derive gParse		Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch
derive gVisualize	Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch
derive gUpdate		Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch

//	Manual boilerplate code:
class billingAddressOf   a :: a -> Address
class shippingAddressOf  a :: a -> Address
class amountOrderedOf    a :: a -> Int
class nameOf             a :: a -> String
class id_Of              a :: a -> DBRef a
class priceOf            a :: a -> Currency
class inStockOf          a :: a -> Int

class billingAddressUpd  a :: a Address      -> a
class shippingAddressUpd a :: a Address      -> a
class amountOrderedUpd   a :: a Int          -> a
class nameUpd            a :: a String       -> a
class id_Upd             a :: a (DBRef a)    -> a
class priceUpd           a :: a Currency	 -> a
class inStockUpd         a :: a Int          -> a

instance id_Of              Book;			instance id_Upd             Book
instance nameOf             Book;			instance nameUpd            Book
instance priceOf            Book;			instance priceUpd           Book
instance inStockOf          Book;			instance inStockUpd         Book

instance nameOf             (CartItem a);	instance nameUpd            (CartItem a)
instance inStockOf          (CartItem a);	instance inStockUpd         (CartItem a)
instance amountOrderedOf    (CartItem a);	instance amountOrderedUpd   (CartItem a)
instance priceOf            (CartItem a);	instance priceUpd           (CartItem a)

instance id_Of              (Order a);		instance id_Upd             (Order a)
instance nameOf             (Order a);		instance nameUpd            (Order a)
instance billingAddressOf   (Order a);		instance billingAddressUpd  (Order a)
instance shippingAddressOf  (Order a);		instance shippingAddressUpd (Order a)

instance nameOf             InCart;			instance nameUpd            InCart
instance amountOrderedOf    InCart;			instance amountOrderedUpd   InCart
instance priceOf            InCart;			instance priceUpd           InCart
