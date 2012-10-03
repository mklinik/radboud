definition module launch

::	PathAndApplication	:== String
::	CommandlineArgs		:== String

launch :: !PathAndApplication !CommandlineArgs !*World -> (!Bool,!Int,!*World)
