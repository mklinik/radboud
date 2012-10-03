definition module DefaultElem

from StdArray import class Array

class DefaultElem a
where
	default_elem :: a
	
instance DefaultElem Int
instance DefaultElem Char
instance DefaultElem Bool

instance DefaultElem {#a} | Array {#} a & DefaultElem a

instance DefaultElem (a e) | Array a e

instance DefaultElem (a,b)	| DefaultElem a & DefaultElem b 

class DefaultElemU a
where 
	default_elemU :: *a

