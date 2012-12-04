implementation module FlightSupport

import StdEnv, Maybe

instance == Seat
where
	(==) (Seat r1 s1) (Seat r2 s2) = r1 == r2 && s1 == s2

instance < Seat
where
	(<) (Seat r1 s1) (Seat r2 s2) = r1 < r2 || s1 < s2

instance fromString Seat
where
 fromString str = Seat (toInt (str % (0,size str - 2))) (fromChar (str.[size str - 1] - 'A') + 1)

instance toString Seat
where
 toString (Seat r s) = toString r +++ toString ('A' + (toChar (s - 1)))

find :: (a -> Bool) [a] -> Maybe a
find f as = case filter f as of
	[] = Nothing					
	fs = Just (hd fs)


intercalate :: [a] [[a]] -> [a]
intercalate ss xss = flatten (intercalate` ss xss)
where
	intercalate` ss []
		= []
		
	intercalate` ss [x]
		= [x]

	intercalate` ss [x,y:xs]
		= [x,ss] ++ intercalate` ss [y:xs]

replicate :: Int a -> [a]
replicate n a = repeatn n a

max a b = if (a<b) b a

maximum :: [a] -> a | Ord a
maximum [a] = a
maximum [a:as] = max a (maximum as)

zipWith :: (a b -> c) [a] [b] -> [c]
zipWith f as [] = []
zipWith f [] bs = []
zipWith f [a:as] [b:bs] = [f a b : zipWith f as bs]

concat :: [[a]]-> [a]
concat as = flatten as

concatMap :: (a -> [b]) [a] -> [b]
concatMap f as = flatten (map f as)


