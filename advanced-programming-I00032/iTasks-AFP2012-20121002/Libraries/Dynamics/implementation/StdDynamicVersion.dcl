definition module StdDynamicVersion;

from StdInt import bitand;
from StdOverloaded import class ==;

:: Version = {
		major	:: !Int
	,	minor	:: !Int
	};
	
DefaultVersion :: Version;
	
toVersion :: !Int -> Version;

fromVersion :: !Version -> Int;

getVersionNumber version :== version bitand 0x00ffffff;

instance == Version;
