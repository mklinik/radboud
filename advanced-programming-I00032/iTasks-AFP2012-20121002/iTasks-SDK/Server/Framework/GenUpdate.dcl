definition module GenUpdate

import StdGeneric, Maybe, Void, Either, Store, SystemTypes
from Map	import :: Map

//Datapath is used to point to substructures of data structures
:: DataPath

:: UpdateMask = Untouched
			  | PartiallyTouched ![UpdateMask]
			  | Touched
			  | TouchedWithState !JSONNode		//Some components need to keep local state that can't be encoded in the value
			  | Blanked

:: *USt =
	{ searchPath		:: !DataPath
	, currentPath		:: !DataPath
	, update			:: !JSONNode
	, consPath			:: ![ConsPos]
	, oldMask			:: ![UpdateMask]
	, newMask			:: ![UpdateMask]
	, iworld			:: !*Maybe IWorld
	}

:: UpdateMode a = UDSearch !a | UDCreate

generic gUpdate a :: !(UpdateMode a) !*USt -> (!a,!*USt)

derive gUpdate UNIT, PAIR, EITHER, CONS of {gcd_arity}, OBJECT of {gtd_num_conses,gtd_conses}, RECORD of {grd_arity}, FIELD
derive gUpdate Int, Real, Char, Bool, String
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), JSONNode, Void, HtmlTag, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gUpdate URL, Note, DateTime, Document, FormButton, Username, Password, EUR, USD, BoundedInt, Date, Time, User, UserConstraint, RadioChoice, ComboChoice, GridChoice, CheckMultiChoice, Map, Tree, TreeChoice, TreeNode, Table, Progress
derive gUpdate EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority
derive gUpdate GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gUpdate DynamicChoice,DynamicChoiceNoView

//Wrapper functions for updating
defaultValue			:: a																		| gUpdate{|*|} a
updateValueAndMask  	:: !DataPath !JSONNode !a !UpdateMask !*IWorld -> (!a,!UpdateMask,!*IWorld)	| gUpdate{|*|} a

//Utility functions for working accessing the iWorld in a USt
appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
accIWorldUSt :: !.(*IWorld -> *(!.a,!*IWorld))!*USt -> (!.a,!*USt)

//Utility functions for dealing with DataPath values
startDataPath			:: DataPath			//Path initialized at position "0"
emptyDataPath			:: DataPath			//Path initialized empty
stepDataPath			:: !DataPath		-> DataPath
shiftDataPath			:: !DataPath		-> DataPath
childDataPath			:: !DataPath !Int	-> DataPath
parentDataPath			:: !DataPath		-> (!DataPath,!Int)
dataPathLevel			:: !DataPath		-> Int
dataPathList 			:: !DataPath 		-> [Int]
dataPathFromList		:: ![Int]			-> DataPath

dp2s			:: !DataPath			-> String
s2dp			:: !String				-> DataPath
isdps			:: !String				-> Bool

// detect whether two paths are equal or if path A is a sub-path of B, assuming reverse-notation. 
// e.g. [1,0] <== [0] 
(<==) infixr 1 :: !DataPath !DataPath -> Bool

class GenMask m
where
	popMask 			:: ![m] -> (!m, ![m])
	appendToMask 		:: ![m] !m -> [m]
	childMasks			:: !m -> [m]
	childMasksN			:: !m !Int -> [m]
	isTouched			:: !m -> Bool

instance == DataPath
instance GenMask UpdateMask

// utility functions for custom gUpdate definitions
/**
* Updates a value.
*
* @param The current update mode
* @param A function defining how to update the value given an update
* @param A default value for creation
* @param USt
*
* @return modified USt
*/
basicUpdate :: !(UpdateMode a) (upd a -> a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
/**
* Updates a value which's new value can be calculated from the update-json
* without knowledge of the previous value.
*
* @param The current update mode
* @param A default value for creation
* @param USt
*
* @return modified USt
*/
basicUpdateSimple :: !(UpdateMode a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a
/**
* Creates a default value.
* (Same as basicUpdate with UDCreate as first parameter.)
*
* @param The created default value
* @param USt
*
* @return modified USt
*/
basicCreate :: !a !*USt -> *(!a,!*USt)
/**
* Search in a value and possibly apply update.
* (Same as basicUpdate with (UDSearch a) as first parameter.)
*
* @param The value to search in
* @param A function defining how calculate a new value from an update
* @param USt
*
* @return modified USt
*/
basicSearch :: !a !(upd a -> a) !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
