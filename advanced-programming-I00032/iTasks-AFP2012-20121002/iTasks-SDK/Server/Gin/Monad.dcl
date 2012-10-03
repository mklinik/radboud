definition module Monad

/** 
* Standard Monad class. 
* Although GenMonad.dcl defines a similar type,
* this definition uses >>> as bind operator, instead of >>=,
* to avoid conflicts with the >>= task sequence operator in iTasks.
*/

class Monad m 
where
	ret :: a -> m a
	(>>>) infixr 5 :: (m a) (a -> m b) -> m b

(>>>|) infixr 5 :: (m a) (m b) -> m b | Monad m
