definition module Either
/**
* This module defines the "Either" type to represent binary choice.
* Clean's generics define a similar type EITHER, but this should only be
* used inside generic functions, since most generic functions treat this
* type in a special way which may lead to strange behavior.
*/

:: Either a b = Left a | Right b