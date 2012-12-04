implementation module Tuple

tuple :: !a !b -> (!a,!b)
tuple a b = (a,b)

tuple3 :: !a !b !c -> (!a,!b,!c)
tuple3 a b c = (a,b,c)

appFst	:: (.a -> .c) !(.a,.b) -> (.c,.b)
appFst f (a,b) = (f a,b)

appSnd	:: (.b -> .c) !(.a,.b) -> (.a,.c)
appSnd f (a,b) = (a,f b)

appFst3 :: (.a -> .d) !(.a,.b,.c) -> (.d,.b,.c)
appFst3 f (a,b,c) = (f a,b,c)

appSnd3 :: (.b -> .d) !(.a,.b,.c) -> (.a,.d,.c)
appSnd3 f (a,b,c) = (a,f b,c)

appThd3 :: (.c -> .d) !(.a,.b,.c) -> (.a,.b,.d)
appThd3 f (a,b,c) = (a,b,f c)