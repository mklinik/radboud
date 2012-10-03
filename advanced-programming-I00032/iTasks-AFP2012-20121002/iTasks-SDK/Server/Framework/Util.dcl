definition module Util

import StdGeneric, SystemTypes, Error, GenEq_NG, StdClass

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

voidNothing 		:: Maybe Void

camelCaseToWords 	:: !String -> String

instance toString (Maybe a) | toString a

pad					:: !Int !Int -> String
decFormat			:: !Int -> String

// Functions for accessing dates and times
currentTime 			:: !*IWorld -> (!Time,!*IWorld)
currentDate 			:: !*IWorld -> (!Date,!*IWorld)
currentDateTime 		:: !*IWorld -> (!DateTime,!*IWorld)
currentTimestamp		:: !*IWorld -> (!Timestamp,!*IWorld)
currentTimestampError	:: !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)
currentDateTimeWorld	:: !*World	-> (!DateTime,!*World)
timestampToGmDateTime	:: !Timestamp -> DateTime
dateToTimestamp			:: !Date -> Timestamp

//Path conversion
toCanonicalPath			:: !FilePath !*World -> (!FilePath,!*World)

//Simple key value functions when fullblown maps are overkill
kvGet		:: k	![(k,v)]	-> Maybe v	| Eq k
kvSet		:: k v	![(k,v)]	-> [(k,v)]	| Eq k 
kvSetOnce	:: k v	![(k,v)]	-> [(k,v)]	| Eq k 
