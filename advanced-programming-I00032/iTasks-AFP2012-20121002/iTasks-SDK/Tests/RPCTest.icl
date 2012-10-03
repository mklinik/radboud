module RPCTest

import iTasks
import TaskTree
import Base64
import JSON
import GeoDomain

import RPCStubs

from TSt import mkRpcTask

rpcStub2 :: Task String
rpcStub2 = mkRpcTask "Ls Command"
	{ RPCExecute
	| taskId 		= ""
	, interface		= { protocol = System
					  , type	 = Plain
					  }
	, operation		= { name = "Ls command"
					  , parameters = []
					  , location = "ls"
					  , callType = RequestResponse
					  , returnType = "String"
					  }
	, paramValues	= [{name = "a", serializedValue = ""},{name = "l", serializedValue = ""}]
	, status 		= ""
	}
	base64Decode
	
rpcStub :: Map -> Task String
rpcStub map 
# (lat,lng) = extractCoords map
= ocean_names lat lng base64Decode

extractCoords :: Map -> (Real,Real)
extractCoords map =: {markers}
# head = hd(markers)
= head.MapMarker.position
	
rpcTestTask :: Task Void
rpcTestTask = 
	enterInformation "Click an ocean" >>= rpcStub >>= showMessage
	
rpcCountryCode :: Task Void
rpcCountryCode =
	enterInformation "Click on a country" >>= rpcCountryCode` >>= showMessage
where
	rpcCountryCode` :: Map -> Task String
	rpcCountryCode` map
	# (lat,lng) = extractCoords map
	= country_code lat lng "JSON" base64Decode
	
rpcTestTask2 :: Task Void
rpcTestTask2 = rpcStub2 >>= showMessage
	
Start :: *World -> *World
Start world = startEngine [workflow "Fetch Ocean Name" rpcTestTask, 
						   workflow "Fetch Country Code" rpcCountryCode,
						   workflow "Do 'ls'-command" rpcTestTask2 ] 
						   world