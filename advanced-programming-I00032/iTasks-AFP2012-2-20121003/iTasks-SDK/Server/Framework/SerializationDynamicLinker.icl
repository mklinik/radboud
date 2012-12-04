implementation module SerializationDynamicLinker

import StdEnv

import Base64
import JSON
import StdMisc
import Error
from Serialization import qualified serialize, deserialize, serializeDynamic, deserializeDynamic
from Store import ::StoreFormat(..)

serialize :: !a -> String | TC a
serialize value = 'Serialization'.serialize value

deserialize	:: !String -> MaybeErrorString a | TC a
deserialize str = 'Serialization'.deserialize str

serializeDynamic :: !Dynamic -> String
serializeDynamic dyn = 'Serialization'.serializeDynamic dyn

deserializeDynamic :: !String -> MaybeErrorString Dynamic
deserializeDynamic str = 'Serialization'.deserializeDynamic str

JSONEncode{|Dynamic|} dyn = [JSONString ""]
JSONEncode{|(->)|} _ _ f = [JSONString ""]

JSONDecode{|Dynamic|} _ = abort "SerializationDynamicLinker, JSONDecode(|Dynamic|} not supported"

JSONDecode{|(->)|} _ _ c = abort "SerializationDynamicLinker, JSONDecode(|(->)|} not supported"

dynamicJSONEncode :: !a -> [JSONNode]
dynamicJSONEncode _ = [JSONString ""]

dynamicJSONDecode :: !JSONNode -> Maybe a
dynamicJSONDecode _ = abort "SerializationDynamicLinker, dynamicJSONDecode not supported"

defaultStoreFormat :: StoreFormat
defaultStoreFormat = SFDynamic

serializationModule :: String
serializationModule = "SerializationDynamicLinker"