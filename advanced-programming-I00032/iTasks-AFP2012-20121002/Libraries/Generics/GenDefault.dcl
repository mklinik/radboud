definition module GenDefault

import StdGeneric

generic gDefault a ::  a 

derive gDefault Int, Real, String, PAIR, EITHER, CONS, FIELD, OBJECT 

derive gDefault [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

