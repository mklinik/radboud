definition module JSON
/*
* This module provides functions to encode and decode any Clean data type
* to JSON format. It provides two generic functions JSONEncode and JSONDecode
* which must be derived for concrete types. Then toJSON and fromJSON may be
* used to convert any value to and from JSON.
*
* For more info about JSON see: http://www.json.org/
*/

import StdGeneric, Maybe, StdString

:: JSONNode	= JSONNull
			| JSONBool !Bool
			| JSONInt !Int
			| JSONReal !Real
			| JSONString !String
			| JSONArray ![JSONNode]
			| JSONObject ![(!String,!JSONNode)]
			| JSONRaw !String
			| JSONError
/**
* Serializing JSON structures is done with a toString instance
*/
instance toString JSONNode
/**
* Deserializing JSON structures is done with a fromString instance
*/
instance fromString JSONNode

/**
* Encodes any value to JSON format.
* @param The value to encode
* @return The JSON encoded value
*/
toJSON		:: !a		-> JSONNode	| JSONEncode{|*|} a
/**
* Tries to parse a JSON encoded string.
* When parsing fails, the result is Nothing.
*
* @param The JSON encoded input
* @return Just the result, when parsing succeeds
*/
fromJSON	:: !JSONNode	-> Maybe a	| JSONDecode{|*|} a

/**
* Escapes a string for manual JSON construction
*
* @param The unescaped string
* @return A properly escaped string
*/
jsonEscape	:: !String	-> String

/**
* Unescapes a string that is escaped for use in a serialized JSON string
*
* @param The escaped string
* @return An unescaped string
*/
jsonUnescape :: !String -> String

/**
* Simple query-by-path function that enables searching of JSON structures
*
* @param The query path separated by '/'. Objects are indexed by fieldname
*        and arrays by their array index.
*        Example paths: 'node1/node3' 'node1/node2/23'
*
* @return The value if a value of the right type is at that path.
*/
jsonQuery :: !String !JSONNode -> Maybe a | JSONDecode{|*|} a

/**
* Generic encoding function. This function should not be used
* directly but always through the toJSON function. It must be derived
* for each type you want to encode in JSON format.
*/
generic JSONEncode t :: !t -> [JSONNode]
derive  JSONEncode Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], (,), (,,), (,,,), (,,,,), {}, {!}, Maybe, JSONNode
/**
* Generic decoding function. This function should not be used
* directly, but always through the fromJSON function. It must be derived
* for each type you want to parse from JSON format.
*/
generic JSONDecode t :: ![JSONNode] -> (!Maybe t,![JSONNode])
derive  JSONDecode Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], (,), (,,), (,,,), (,,,,), {}, {!}, Maybe, JSONNode

/**
* Equality of JSON nodes.
* JSON Reals are considered equal if their string representation is equal.
* JSON Objects are considered equal if they contain the same non-null fields.
*/
instance == JSONNode
