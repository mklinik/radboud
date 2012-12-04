implementation module menuCCall_12

import ostoolbox
import code from "cCrossCallMenus_121."

winInitialiseMenus :: !*OSToolbox -> *OSToolbox
winInitialiseMenus _
	= code
	{
		.inline InstallCrossCallMenus
			ccall InstallCrossCallMenus "I-I"
		.end
	}
