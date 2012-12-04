definition module FlightSupport

import StdString, StdClass, Maybe

:: Seat = Seat Int Int

instance == Seat
instance < Seat

instance fromString Seat
instance toString Seat

find :: (a -> Bool) [a] -> Maybe a
intercalate :: [a] [[a]] -> [a]
replicate :: Int a -> [a]
maximum :: [a] -> a | Ord a
zipWith :: (a b -> c) [a] [b] -> [c]
concat :: [[a]] -> [a]
concatMap :: (a -> [b]) [a] -> [b]