definition module Store
/**
* This module provides a simple generic store.
* It is used to store the internal databases with users, sessions and workflow processes
* and for storage of intermediate task results.
*
* Values can be stored either by generic serialization to plain text, or by writing a dynamic
* to disk.
* Dynamics are generally more expensive, so only when really necessary (for example to store tasks or
* functions) should they be used.
*/
import JSON_NG
from Time import :: Timestamp
from IWorld import :: IWorld
from FilePath import :: FilePath

// Storage formats
:: StoreFormat = SFPlain | SFDynamic

:: StoreKey			:== String
:: StoreNamespace	:== String
:: StorePrefix		:== String

// Predefined namespaces
NS_TASK_INSTANCES		:== "task-instances"
NS_DOCUMENT_CONTENT		:== "document-data"
NS_APPLICATION_SHARES	:== "application-data"

/**
* Determine the location of the store from data directory and build
*/
storePath :: !FilePath !String -> FilePath

/**
* Store a value in the default format
*/
storeValue				:: !StoreNamespace !StoreKey !a				!*IWorld -> *IWorld							| JSONEncode{|*|}, TC a

/**
* Load a value from the store
*/
loadValue				:: !StoreNamespace !StoreKey				!*IWorld -> (!Maybe a,!*IWorld)				| JSONDecode{|*|}, TC a

/**
* Deletes the value with given key from the store
*/
deleteValue				:: !StoreNamespace !StoreKey				!*IWorld -> *IWorld

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues			:: !StoreNamespace !StorePrefix				!*IWorld -> *IWorld

/**
* Store a binary blob
*/
storeBlob				:: !StoreNamespace !StoreKey !{#Char}		!*IWorld -> *IWorld

/**
* Load a binary blob
*/
loadBlob				:: !StoreNamespace !StoreKey 				!*IWorld -> (!Maybe {#Char}, !*IWorld)
