implementation module DefaultElem

import StdEnv

class DefaultElem a
where
	default_elem :: a
	
instance DefaultElem Int
where
	default_elem = 0
	
instance DefaultElem Char
where
	default_elem = ' '

instance DefaultElem Bool
where
	default_elem = False

instance DefaultElem {#a}	| Array {#} a & DefaultElem a
where
	default_elem
		= {default_elem}

instance DefaultElem (a,b)	| DefaultElem a & DefaultElem b 
where
	default_elem
		= (default_elem,default_elem);		

instance DefaultElem (a e) | Array a e
where
	default_elem
		= {}
		
instance DefaultElem [e]
where
	default_elem
		= []
		
class DefaultElemU a
where 
	default_elemU :: *a


