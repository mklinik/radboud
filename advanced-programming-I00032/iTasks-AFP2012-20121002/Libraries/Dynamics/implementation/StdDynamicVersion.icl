implementation module StdDynamicVersion;

import StdEnv;
from StdInt import bitand;

:: Version = {
		major	:: !Int
	,	minor	:: !Int
	};
	
DefaultVersion :: Version;
DefaultVersion 
	= { 
		major	= 0
	,	minor	= 0
	};
	
toVersion :: !Int -> Version;
toVersion version
	#! version
		= { Version |
			major	= (version >> 8) bitand 0x0000ffff
		,	minor	= version bitand 0x000000ff
		};
	= version;
	
fromVersion :: !Version -> Int;
fromVersion {major,minor}
	= (major << 8) bitor minor;
	
getVersionNumber version :== version bitand 0x00ffffff;
// most significant byte is reserved

instance == Version
where {
	(==) {major=major1,minor=minor1} {major=major2,minor=minor2}
		= major1 == major2 && minor1 == minor2;
};