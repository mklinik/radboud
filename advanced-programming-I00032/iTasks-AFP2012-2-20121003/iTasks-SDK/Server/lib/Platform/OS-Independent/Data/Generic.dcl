definition module Generic

import StdGeneric

fromOBJECT	:: !(OBJECT x)	-> x
fromCONS	:: !(CONS x)	-> x
fromFIELD	:: !(FIELD x)	-> x
fromPAIRX	:: !(PAIR x y)	-> x
fromPAIRY	:: !(PAIR x y)	-> y

isRecordType	:: !GenericTypeDefDescriptor	-> Bool
isRecordCons	:: !GenericConsDescriptor		-> Bool