definition module SerializationGraphCopy

from JSON_NG import generic JSONEncode, generic JSONDecode, ::JSONNode
from Error import ::MaybeError, ::MaybeErrorString
from Maybe import ::Maybe
from Store import :: StoreFormat

serialize :: !a -> *String
deserialize	:: !*String -> MaybeErrorString a
serializeDynamic :: !Dynamic -> *String
deserializeDynamic :: !*String -> Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)

//Check if a JSON serialization contains encoded functions or dynamics
functionFree		:: !JSONNode -> Bool

dynamicJSONEncode	:: !a -> JSONNode
dynamicJSONDecode	:: !JSONNode -> Maybe a

serializationModule	:: String
defaultStoreFormat	:: StoreFormat