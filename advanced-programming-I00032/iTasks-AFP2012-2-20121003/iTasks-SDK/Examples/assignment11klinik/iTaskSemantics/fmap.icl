implementation module fmap

import StdFunc, StdList, StdTuple

:: FMap a b :== [(a,b)]

lookupFM :: !a !(FMap a b) -> b | == a
lookupFM a table = snd (hd (filterFMBy (((==) a) o fst) table))

updateFM :: !(a,b) !(FMap a b) -> FMap a b | == a
updateFM new table = updateFMBy new const table

updateFMBy :: !(a,c) (c b -> b) !(FMap a b) -> FMap a b | == a
updateFMBy (a,c) f table = map (\(a`,b`) -> (a`, if (a == a`) (f c b`) b`)) table

filterFMBy :: ((a,b) -> Bool) !(FMap a b) -> FMap a b
filterFMBy f table = filter f table

appendFM :: (a,b) !(FMap a b) -> FMap a b
appendFM ab table = table ++ [ab]

domainFM :: !(FMap a b) -> [a]
domainFM table = map fst table

rangeFM :: !(FMap a b) -> [b]
rangeFM table = map snd table

updateFst :: a !(a,b) -> (a,b)
updateFst a (_,b) = (a,b)

updateSnd :: b !(a,b) -> (a,b)
updateSnd b (a,_) = (a,b)
