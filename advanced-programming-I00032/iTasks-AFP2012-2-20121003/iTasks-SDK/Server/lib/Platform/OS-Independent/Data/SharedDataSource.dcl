definition module SharedDataSource

import FilePath, Void, Maybe, Error, Time

from _SharedDataSourceTypes import :: RWShared, :: BasicShareId, :: WriteShare(..)

:: Shared a env		:== RWShared a a env
:: ROShared a env	:== RWShared a Void env
:: WOShared a env	:== RWShared Void a env
:: Hash				:== String

class registerSDSMsg		msg	env	:: !BasicShareId msg			!*env -> *env
class reportSDSChange			env :: !BasicShareId				!*env -> *env
class reportSDSChangeFilter	msg	env :: !BasicShareId !(msg -> Bool)	!*env -> *env

createChangeOnWriteSDS ::
	!String
	!String
	!(*env -> *(!MaybeErrorString r, !*env))
	!(w *env -> *(!MaybeErrorString Void, !*env))
	->
	RWShared r w *env
	
createReadOnlySDS ::
	!(*env -> *(!r, !*env))
	->
	ROShared r *env
	
createReadOnlySDSError ::
	!(*env -> *(!MaybeErrorString r, !*env))
	->
	ROShared r *env
	

read			::						!(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env)
readRegister	:: !msg					!(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env)		| registerSDSMsg msg env
write			:: !w					!(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env)	| reportSDSChange env
writeFilterMsg	:: !w !(msg -> Bool)	!(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env)	| reportSDSChangeFilter msg env
//getHash		::		!(RWShared r w *env) !*env -> (!MaybeErrorString Hash, !*env)

/** core combinators **/
// read combinator
(>?>) infixl 6 :: !(RWShared rx wx *env) !(rx -> MaybeErrorString (RWShared ry wy *env)) -> RWShared ry wx *env
// write combinator
(>!>) infixl 6 :: !(RWShared r w` *env) !(!w -> MaybeErrorString (RWShared r` w`` *env), !w r` -> MaybeErrorString [WriteShare *env]) -> RWShared r w *env

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead			:: !(r -> r`)					!(RWShared r w *env) -> RWShared r` w *env
mapWrite		:: !(w` r -> Maybe w)			!(RWShared r w *env) -> RWShared r w` *env
mapReadWrite	:: !(!r -> r`,!w` r -> Maybe w)	!(RWShared r w *env) -> RWShared r` w` *env

mapReadError		:: !(r -> MaybeErrorString r`)										!(RWShared r w *env) -> RWShared r` w *env
mapWriteError		:: !(w` r -> MaybeErrorString (Maybe w))							!(RWShared r w *env) -> RWShared r w` *env
mapReadWriteError	:: !(!r -> MaybeErrorString r`,!w` r -> MaybeErrorString (Maybe w))	!(RWShared r w *env) -> RWShared r` w` *env

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) (wx,wy) *env
(>+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wx *env
(|+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wy *env
(|+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) Void *env

toReadOnly :: !(RWShared r w *env) -> ROShared r *env

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param RWShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *env) !(Shared b *env) -> (!Shared a *env, !Shared b *env)

// null share
null		:: WOShared a *env
// constant share, value does never change
constShare	:: !a -> ROShared a *env