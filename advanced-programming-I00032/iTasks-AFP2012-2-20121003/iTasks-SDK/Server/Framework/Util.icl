implementation module Util

import StdBool, StdList, StdFile, StdMisc, StdArray, StdString, StdTuple, StdFunc, StdGeneric, StdOrdList, Maybe, Time, Text, FilePath, Directory, JSON_NG, Void, Error, GenEq_NG
from SystemTypes	import :: Date{..}, :: Time{..}, :: DateTime(..)
from IWorld 		import :: IWorld{currentDateTime,timestamp}

mb2list	:: !(Maybe [a]) -> [a]
mb2list	Nothing = []
mb2list (Just a) = a

list2mb	:: ![a] -> (Maybe [a])
list2mb [] = Nothing
list2mb a = (Just a)

voidNothing :: Maybe Void
voidNothing = Nothing

pad :: !Int !Int -> String
pad len num
	# nums = toString num
	| size nums >= len
		= nums
		= createArray (len - size nums) '0' +++ nums
	
decFormat :: !Int -> String
decFormat x = toString (x / 100) +++ "." +++ pad 2 (x rem 100)

camelCaseToWords :: !String -> String
camelCaseToWords label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= fromString label
	addspace []			= []
	addspace [c:cs]
		| c == '_'			= [' ':addspace cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

currentTime :: !*IWorld -> (!Time,!*IWorld)
currentTime iworld=:{currentDateTime=l=:(DateTime _ time)} = (time,iworld)

currentDate :: !*IWorld -> (!Date,!*IWorld)
currentDate iworld=:{currentDateTime=l=:(DateTime date _)} = (date,iworld)
	
currentDateTime :: !*IWorld -> (!DateTime,!*IWorld)
currentDateTime iworld=:{currentDateTime} = (currentDateTime,iworld)

currentTimestamp :: !*IWorld -> (!Timestamp,!*IWorld)
currentTimestamp iworld=:{timestamp} = (timestamp,iworld)

currentTimestampError :: !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)
currentTimestampError iworld=:{timestamp} = (Ok timestamp,iworld)

currentDateTimeWorld :: !*World -> (!DateTime,!*World)
currentDateTimeWorld world
	# (tm,world) = localTime world
	= (tmToDateTime tm,world)
	
timestampToGmDateTime :: !Timestamp -> DateTime
timestampToGmDateTime timestamp = tmToDateTime (toGmTime timestamp)

tmToDateTime :: !Tm -> DateTime
tmToDateTime tm
	# date	= {Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year}
	# time	= {Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}
	= DateTime date time

dateToTimestamp :: !Date -> Timestamp
dateToTimestamp {Date|day,mon,year}
	= mkTime {Tm|sec = 0, min = 0, hour = 0, mday = day, mon = mon - 1, year = year - 1900, wday = 0, yday = 0, isdst = False}

instance toString (Maybe a) | toString a
where
	toString Nothing	= ""
	toString (Just x)	= toString x

toCanonicalPath	:: !FilePath !*World -> (!FilePath,!*World)
toCanonicalPath path world
	= case split {pathSeparator} path of
		["":ds]		= (join {pathSeparator} ["":canonicalize ds],world)
		ds			= case getCurrentDirectory world of
			(Ok curDir,world)	= (join {pathSeparator} [curDir:canonicalize ds], world)
			(_,world)			= (join {pathSeparator} (canonicalize ds), world)
where
	canonicalize path = undot [] path

	undot acc []				= reverse acc
	undot []  ["..":ds]			= undot [] ds
	undot [_:acc] ["..":ds]		= undot acc ds
	undot acc [".":ds]			= undot acc ds
	undot acc ["":ds]			= undot acc ds
	undot acc [d:ds] 			= undot [d:acc] ds

kvGet :: k ![(k,v)]		-> Maybe v	| Eq k // Linear search
kvGet m []				= Nothing
kvGet m [(k,v):kvs]		= if (k == m) (Just v) (kvGet m kvs)

kvSet :: k v ![(k,v)]	-> [(k,v)]	| Eq k //Linear search
kvSet m nv []			= [(m,nv)]
kvSet m nv [(k,v):kvs]	= if (k == m) [(k,nv): kvs] [(k,v):kvSet m nv kvs]

kvSetOnce :: k v ![(k,v)]	-> [(k,v)]	| Eq k //Linear search
kvSetOnce m nv []			= [(m,nv)]
kvSetOnce m nv [(k,v):kvs]	= if (k == m) [(k,v):kvs] [(k,v):kvSet m nv kvs]


