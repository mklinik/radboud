definition module iTaskClass

import GenVisualize

//iTask context restriction
class iTask a
	| gVisualizeEditor{|*|}
	, gVisualizeText{|*|}
	, gHeaders{|*|}
	, gGridRows{|*|}
	, gUpdate{|*|}
	, gVerify{|*|}
	, JSONEncode{|*|}
	, JSONDecode{|*|}
	, gEq{|*|}
	, TC a
	
:: Container a c = Container a & iTask c // container for context restrictions
