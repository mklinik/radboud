implementation module SerializationGraphCopy

import StdEnv
import dynamic_string
from Store import :: StoreFormat(..)

import Base64
import Error
import JSON_NG
import Maybe

serialize :: !a -> *String
serialize value = copy_to_string value

deserialize	:: !*String -> MaybeErrorString a
deserialize str = let (a,_) = (copy_from_string str) in (Ok a)
				
serializeDynamic :: !Dynamic -> *String
serializeDynamic dyn = dynamic_to_string dyn

deserializeDynamic :: !*String -> Dynamic
deserializeDynamic str = string_to_dynamic str

JSONEncode{|Dynamic|} dyn = [JSONArray [JSONString "_DYNAMIC_", JSONString (base64URLEncode (serializeDynamic dyn))]]

JSONEncode{|(->)|} _ _ f = [JSONArray [JSONString "_FUNCTION_", JSONString (base64URLEncode (serialize f))]]

JSONDecode{|Dynamic|} [JSONArray [JSONString "_DYNAMIC_",JSONString string]:c]	= (Just (deserializeDynamic (base64URLDecode string)), c)
JSONDecode{|Dynamic|} c												= (Nothing, c)

JSONDecode{|(->)|} _ _ [JSONArray [JSONString "_FUNCTION_",JSONString string]:c] = (Just (fst(copy_from_string {s` \\ s` <-: base64URLDecode string})) ,c)
JSONDecode{|(->)|} _ _ c											= (Nothing,c)

functionFree :: !JSONNode -> Bool
functionFree (JSONString "_FUNCTION_") = False
functionFree (JSONString "_DYNAMIC_") = False
functionFree (JSONString "_DYNAMICENCODE_") = False
functionFree (JSONArray items) = and (map functionFree items)
functionFree (JSONObject fields) = and (map (functionFree o snd) fields)
functionFree _ = True

dynamicJSONEncode :: !a -> JSONNode
dynamicJSONEncode f = JSONArray [JSONString "_DYNAMICENCODE_",JSONString (base64URLEncode (copy_to_string f))]

dynamicJSONDecode :: !JSONNode -> Maybe a
dynamicJSONDecode (JSONArray [JSONString "_DYNAMICENCODE_",JSONString str]) = Just (fst (copy_from_string (base64URLDecode str)))
dynamicJSONDecode _					= Nothing

serializationModule :: String
serializationModule = "SerializationGraphCopy"

defaultStoreFormat :: StoreFormat
defaultStoreFormat = SFPlain
