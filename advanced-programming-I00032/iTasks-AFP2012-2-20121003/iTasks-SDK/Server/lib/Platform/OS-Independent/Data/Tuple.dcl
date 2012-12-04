definition module Tuple

tuple	:: !a !b	-> (!a,!b)
tuple3	:: !a !b !c	-> (!a,!b,!c)

appFst	:: (.a -> .c) !(.a,.b) -> (.c,.b)
appSnd	:: (.b -> .c) !(.a,.b) -> (.a,.c)

appFst3 :: (.a -> .d) !(.a,.b,.c) -> (.d,.b,.c)
appSnd3 :: (.b -> .d) !(.a,.b,.c) -> (.a,.d,.c)
appThd3 :: (.c -> .d) !(.a,.b,.c) -> (.a,.b,.d)
