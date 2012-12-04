definition module _SharedDataSourceTypes

import Maybe, Error, Void, Time

:: RWShared r w *env
	= E.b:			BasicSource		!(BasicSource b r w env)
	| E.rx wy:		ComposedRead	!(RWShared rx w env) !(rx -> MaybeErrorString (RWShared r wy env))
	| E.r` w` w``:	ComposedWrite	!(RWShared r w` env) !(w -> MaybeErrorString (RWShared r` w`` env)) !(w r` -> MaybeErrorString [WriteShare env])

:: BasicSource b r w *env =
	{ notification	:: !ChangeNotification
	, read			:: !env -> *(!MaybeErrorString r, !env)
	, write			:: !w env -> *(!MaybeErrorString Void, !env)
	}
	
:: ChangeNotification = None | RegisterId !BasicShareId | Timer !Timestamp
:: BasicShareId :== String	
:: WriteShare *env = E.r w: Write !w !(RWShared r w env)
	