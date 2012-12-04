definition  module streamUtil

import iTasks


// spawn an asyncronous process

spawnP 	:: String (Task a) -> Task (Task a) | iTask a					

waitP 	:: (a -> Task  b) (Task a) -> Task b  | iTask a & iTask b





