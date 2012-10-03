definition module tmfile

import	StdString
from	StdFile		import class FileSystem
from	tm			import :: Turing

WriteTuringToFile	:: Turing	!String !*env	-> (!Bool,!*env)           | FileSystem env
ReadTuring			::			!String !*env	-> (!(!Int,!Turing),!*env) | FileSystem env
RemovePath			::			!String			-> String
