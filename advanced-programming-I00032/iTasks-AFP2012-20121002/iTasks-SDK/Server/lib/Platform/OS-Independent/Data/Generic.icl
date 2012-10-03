implementation module Generic

import StdGeneric, StdList

fromOBJECT	:: !(OBJECT x)	-> x
fromOBJECT	(OBJECT x)	= x

fromCONS	:: !(CONS x)	-> x
fromCONS	(CONS x)	= x

fromFIELD	:: !(FIELD x)	-> x
fromFIELD	(FIELD x)	= x

fromPAIRX	:: !(PAIR x y)	-> x
fromPAIRX	(PAIR x _)	= x

fromPAIRY	:: !(PAIR x y)	-> y
fromPAIRY	(PAIR _ y)	= y

isRecordType :: !GenericTypeDefDescriptor -> Bool
isRecordType {gtd_conses} = case gtd_conses of
	[{gcd_fields}]	= not (isEmpty gcd_fields)
	_				= False
	
isRecordCons :: !GenericConsDescriptor -> Bool
isRecordCons {gcd_fields} = not (isEmpty gcd_fields)