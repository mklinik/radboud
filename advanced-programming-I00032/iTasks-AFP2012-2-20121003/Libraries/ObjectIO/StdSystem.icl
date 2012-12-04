implementation module StdSystem


import	StdString, StdReal, StdInt, StdTuple
import	ossystem
from	StdIOBasic	import :: Size{..}


//	System dependencies concerning the file system

dirseparator :: Char
dirseparator = OSdirseparator			// Separator between folder- and filenames in a pathname

homepath :: !String -> String
homepath fname = osHomepath fname

applicationpath :: !String -> String
applicationpath fname = osApplicationpath fname

newlineChars :: String
newlineChars = OSnewlineChars

//	System dependencies concerning the time resolution

ticksPerSecond :: Int
ticksPerSecond = OStickspersecond


//	System dependencies concerning the screen resolution
mmperinch :== 25.4

hmm :: !Real -> Int
hmm mm = osMMtoHPixels mm

vmm :: !Real -> Int
vmm mm = osMMtoVPixels mm

hinch :: !Real -> Int
hinch inch = osMMtoHPixels (inch*mmperinch)

vinch :: !Real -> Int
vinch inch = osMMtoVPixels (inch*mmperinch)

maxScrollWindowSize :: Size
maxScrollWindowSize
	= {w=w,h=h}
where
	(w,h)	= osMaxScrollWindowSize

maxFixedWindowSize :: Size
maxFixedWindowSize
	= {w=w,h=h}
where
	(w,h)	= osMaxScrollWindowSize

// MW11++
printSetupTypical :: Bool
printSetupTypical = osPrintSetupTypical
