definition module GenMap

import StdGeneric

generic gMap a b :: .a -> .b
derive gMap c, PAIR, EITHER, CONS, FIELD, OBJECT, {}, {!} 

derive gMap [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
