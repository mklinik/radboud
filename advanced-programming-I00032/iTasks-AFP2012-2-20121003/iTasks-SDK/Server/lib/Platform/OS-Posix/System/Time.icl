implementation module Time

import StdString, StdArray, StdClass, StdOverloaded, StdInt
import _Pointer

//String buffer size
MAXBUF :== 256

instance == Timestamp
where
	(==) (Timestamp t1) (Timestamp t2) = t1 == t2

instance < Timestamp
where
	(<) (Timestamp t1) (Timestamp t2) = t1 < t2

instance toString Tm
where
	toString tm = derefString (toStringTmC (packTm tm))
	where
		toStringTmC :: !{#Int} -> Pointer
		toStringTmC a0 = code {
			ccall asctime "A:p"
		}
instance toString Timestamp
where
	toString (Timestamp t) = derefString (toStringTimeC (packInt t))
	where	
		toStringTimeC :: !{#Int} -> Pointer
		toStringTimeC a0 = code {
			ccall ctime "A:p"
		}
instance toString Clock
where
	toString (Clock c) = toString c
instance toInt Timestamp
where
	toInt (Timestamp i) = i

clock :: !*World -> (!Clock, !*World)
clock world
	# (c, world) = clockC world
	= (Clock c, world)
	where
	clockC :: !*World -> (!Int, !*World)
	clockC world = code {
		ccall clock ":I:p"
	}

time :: !*World -> (!Timestamp, !*World)
time world
	# (t, world)	= timeC 0 world
	= (Timestamp t, world)
	where
	timeC :: !Int !*World -> (!Int,!*World)
	timeC a0 world = code {
		ccall time "I:I:p"
	}

gmTime :: !*World -> (!Tm, !*World)
gmTime world
	# ((Timestamp t),world)	= time world
	# (tm, world)			= gmTimeC (packInt t) world
	= (derefTm tm, world)
	where
	gmTimeC :: !{#Int} !*World -> (!Int, !*World)
	gmTimeC tm world = code {
    	ccall gmtime "A:p:p"
	}

localTime :: !*World -> (!Tm, !*World)
localTime world
	# ((Timestamp t),world)	= time world
	# (tm,world)			= localTimeC (packInt t) world
	= (derefTm tm, world)
	where
	localTimeC :: !{#Int} !*World -> (!Int, !*World)
	localTimeC tm world = code {
    	ccall localtime "A:p:p"
	}

mkTime :: !Tm -> Timestamp
mkTime tm 
	# t = mkTimeC (packTm tm)
	= Timestamp t
	where
	mkTimeC :: !{#Int} -> Int
	mkTimeC tm = code {
		ccall mktime "A:I"
	}

diffTime :: !Timestamp !Timestamp -> Int
diffTime (Timestamp t1) (Timestamp t2) = t1 - t2

strfTime :: !String !Tm -> String
strfTime format tm 
	# buf		= createArray MAXBUF 'X'
	# (len,buf)	= strfTimeC buf MAXBUF (packString format) (packTm tm) buf
	= buf % (0, len - 1)
	where
		strfTimeC :: !{#Char} !Int !{#Char} !{#Int} !{#Char} -> (!Int,!{#Char})
		strfTimeC a0 a1 a2 a3 a4 = code {
			ccall strftime "sIsA:I:A"
		}

toLocalTime :: !Timestamp !*World -> (!Tm,!*World)
toLocalTime (Timestamp t) world
    # (tm,world) = localTimeC (packInt t) world
    = (derefTm tm, world)

toGmTime :: !Timestamp -> Tm
toGmTime (Timestamp t) = derefTm (gmTimeC (packInt t))

gmTimeC :: !{#Int} -> Pointer
gmTimeC tm = code {
    ccall gmtime "A:p"
}

localTimeC :: !{#Int} !*World -> (!Pointer, !*World)
localTimeC tm world = code {
    ccall localtime "A:p:p"
}

derefTm :: !Pointer-> Tm
derefTm ptr = unpackTm (derefCharArray ptr sizeOfTm) 0

packTm :: !Tm -> {#Int}
packTm tm = (IF_INT_64_OR_32 packTm64 packTm32) tm

packTm64 :: !Tm -> {#Int}
packTm64 tm =   { tm.sec  + tm.min  << 32
                , tm.hour + tm.mday << 32
                , tm.mon  + tm.year << 32
                , tm.wday + tm.yday << 32
                , if tm.isdst 1 0
                }

packTm32 :: !Tm -> {#Int}
packTm32 tm =   { tm.sec
                , tm.min
                , tm.hour
                , tm.mday
                , tm.mon
                , tm.year
                , tm.wday
                , tm.yday
                , if tm.isdst 1 0
                }

unpackTm :: !{#Char} !Int -> Tm
unpackTm buf off =
	{ sec   = unpackInt4S buf (off + 0)
    , min   = unpackInt4S buf (off + 4)
	, hour  = unpackInt4S buf (off + 8)
	, mday  = unpackInt4S buf (off + 12)
	, mon   = unpackInt4S buf (off + 16)
	, year  = unpackInt4S buf (off + 20)
	, wday  = unpackInt4S buf (off + 24)
	, yday  = unpackInt4S buf (off + 28)
	, isdst = unpackBool buf (off + 32)
	}

sizeOfTm :: Int
sizeOfTm = 36 
