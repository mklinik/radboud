implementation module ShopDSLboilerplate

import GenBimap, GenVisualize, GenUpdate
import ShopDSL

//	Generic boilerplate code:
derive gPrint		Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch
derive gParse		Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch
derive gVisualize	Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch
derive gUpdate		Book, Order, Address, CartItem, CartAmount, ShopAction, InCart, ProductSearch

//	Manual boilerplate code:
instance billingAddressOf   (Order    a) where billingAddressOf   r		= r.Order.billingAddress
instance shippingAddressOf  (Order    a) where shippingAddressOf  r		= r.Order.shippingAddress
instance amountOrderedOf    (CartItem a) where amountOrderedOf    r		= r.CartItem.amountOrdered
instance nameOf             (Order    a) where nameOf             r		= r.Order.name
instance nameOf             Book         where nameOf             r		= r.Book.title
instance id_Of              Book         where id_Of              r		= fromHidden r.Book.id
instance priceOf            Book         where priceOf            r		= r.Book.price
instance id_Of              (Order    a) where id_Of              r		= r.Order.id
instance priceOf            (CartItem a) where priceOf            r		= r.CartItem.price
instance priceOf            InCart       where priceOf            r		= r.InCart.price
instance inStockOf          Book         where inStockOf          r		= r.Book.inStock
instance inStockOf          (CartItem a) where inStockOf          r		= r.CartItem.inStock
instance nameOf             (CartItem a) where nameOf             r		= r.CartItem.name
instance nameOf             InCart       where nameOf             r		= r.InCart.name
instance amountOrderedOf    InCart       where amountOrderedOf    r		= r.InCart.amountOrdered

instance billingAddressUpd  (Order    a) where billingAddressUpd  r new	= {r & Order.billingAddress   = new}
instance shippingAddressUpd (Order    a) where shippingAddressUpd r new	= {r & Order.shippingAddress  = new}
instance amountOrderedUpd   (CartItem a) where amountOrderedUpd   r new	= {r & CartItem.amountOrdered = new}
instance nameUpd            Book         where nameUpd            r new	= {r & Book.title             = new}
instance nameUpd            (Order    a) where nameUpd            r new	= {r & Order.name             = new}
instance id_Upd             Book         where id_Upd             r new	= {r & Book.id                = toHidden new}
instance priceUpd           Book         where priceUpd           r new = {r & Book.price             = new}
instance id_Upd             (Order    a) where id_Upd             r new	= {r & Order.id               = new}
instance priceUpd           (CartItem a) where priceUpd           r new = {r & CartItem.price         = new}
instance priceUpd           InCart       where priceUpd           r new = {r & InCart.price           = new}
instance inStockUpd         Book         where inStockUpd         r new = {r & Book.inStock           = new}
instance inStockUpd         (CartItem a) where inStockUpd         r new = {r & CartItem.inStock       = new}
instance nameUpd            (CartItem a) where nameUpd            r new = {r & CartItem.name          = new}
instance nameUpd            InCart       where nameUpd            r new = {r & InCart.name            = new}
instance amountOrderedUpd   InCart       where amountOrderedUpd   r new = {r & InCart.amountOrdered   = new}
