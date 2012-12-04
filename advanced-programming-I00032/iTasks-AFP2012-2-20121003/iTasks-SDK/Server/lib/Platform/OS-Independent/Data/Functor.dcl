definition module Functor

class Functor f
where
	fmap :: !(.a -> .b) !u:(f .a) -> u:(f .b)