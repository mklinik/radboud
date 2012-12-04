definition module fmap

import StdOverloaded

:: FMap a b :== [(a,b)]

lookupFM    ::  !a               !(FMap a b) ->        b | == a
updateFM    :: !(a,b)            !(FMap a b) -> FMap a b | == a
updateFMBy  :: !(a,c) (c b -> b) !(FMap a b) -> FMap a b | == a
filterFMBy	:: ((a,b) -> Bool)   !(FMap a b) -> FMap a b
appendFM    ::  (a,b)            !(FMap a b) -> FMap a b
domainFM    ::                   !(FMap a b) -> [a]
rangeFM     ::                   !(FMap a b) -> [b]
updateFst   :: a !(a,b) -> (a,b)
updateSnd   :: b !(a,b) -> (a,b)
