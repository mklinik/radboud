definition module iTask_semantics

import Maybe, fmap
from   SystemTypes import :: Action(..), :: ActionName, instance == Action
import iTasks

derive class iTask State, Event`, Reduct, Response, Value`, EditorResponse,  EditMode, TaskResult`, Stability`

// reduction
:: Task` a 			:==	Event` State -> (Reduct a, Responses, State)	

:: Event`			= 	RefreshEvent`																// Recalculate iTask expression							
					|	EditEvent`   TaskNo` LocalVal  												// EditEvent` TaskIdent (Val l)
					|	ActionEvent` TaskNo` Action													// Used in step
:: TaskNo`			:==	Int
:: EditValue		:== (LocalVal, GlobalVal)
:: LocalVal 		:== SerializedValue										
:: GlobalVal		:== SerializedValue

:: Reduct a 		=	Reduct (TaskResult` a) (Task` a)   											// result of a task is a value and the task which has produced this value, which can be further evaluated
:: TaskResult` a	=	ValRes` TimeStamp` (Value` a)
					|	ExcRes` Ex
:: Value` a 		=   NoVal`
					|   Val` a Stability`															// task has no valid result (yet)
:: Stability`		=   Unstable`  																	// task has a result which may change
					|	Stable` 		  															// task has reached a final result
:: TimeStamp`		:== Int
:: Responses		:== [(TaskNo`, Response)]
:: Response			=	EditorResponse  EditorResponse												// used to visualize an editor 
					|	ActionResponse` ActionResponse												// used to visualize an action
:: EditorResponse	=	{ description  :: String
						, editValue    :: EditValue
						, editing      :: EditMode
						}
:: EditMode			=	Editing
					|	Displaying
:: ActionResponse   :== [(Action,Enabled)]
:: Enabled			:== Bool

:: State 			= 	{ mem          :: [SerializedValue]											// memory for the shared data				
						, taskNo       :: TaskNo`
						, timeStamp    :: TimeStamp`
						}
						
:: SerializedValue	:== JSONNode																	// used for serialization
:: Ex				:== Int																			// exception represented as simple error code


class iTask` a | JSONEncode{|*|} a & JSONDecode{|*|} a & gEq{|*|} a

initState 			:: State

// shared variables
:: ROShared` r		:== RWShared` r Void
:: Shared`   a		:== RWShared` a a    
:: RWShared` r w

// access functions
isStable			:: !(Value` a) -> Bool
isValue				:: !(Value` a) -> Bool
getValue			:: !(Value` a) -> a

serialize`			:: !a -> SerializedValue | JSONEncode{|*|} a
deserialize`		:: !SerializedValue -> a | JSONDecode{|*|} a

// basic tasks
withShared`			:: a ((Shared` a) -> Task` b) -> Task` b | iTask` a

edit`				:: String l (RWShared` r w) (l r -> Maybe a) -> Task` a | iTask` l & iTask` r 	// Deliveres an Unstable value, it may or maynot be a value depending on the edit actions end user
show`				:: String a (RWShared` r w) -> Task` a | iTask` r & iTask` a					// Delivers a Stable value, yet the shared value may change over time

simplified_edit		:: String a -> Task` a | iTask` a
simplified_show		:: String a -> Task` a | iTask` a

throw`				:: Ex -> Task` a 																// Raises an exception
return`				:: a  -> Task` a 																// Delivers a Stable version, no shared stuf, nothing will be shown either...

// core combinators
:: TaskStep` a b    = OnAction` Action ((Value` a) -> Bool) ((Value` a) -> Task` b)
				    | OnValue`         ((Value` a) -> Bool) ((Value` a) -> Task` b)
				    | OnException`                          (      Int  -> Task` b) //& iTask` e

(>>>*) infixl 1		:: (Task` a) [TaskStep` a b] -> Task` b											// take a 'step'

// modifiers
(@@?) infixl 1		:: (Task` a)  ((Value` a) -> Value` b) -> Task` b								// pure transform (not a 'step')
(@@>) infixl 1		:: (Task` a) !((Value` a) r -> Maybe w, RWShared` r w) -> Task` a | iTask` a

// parallel
:: ParTask    a 	:== (STaskTable a) -> Task` a
:: STaskTable a		:== Shared` (TaskTable` a)
:: TaskTable` a 	:== FMap (Pid a) (Reduct a)
:: TaskEntry  a     :== (Pid a, (Reduct a)) 

:: Entry a			:== (Pid a, Shared` (TaskTable` a))
:: Pid a			:== Int

parallel`			:: [ParTask a] -> Task` [(TimeStamp`,Value` a)] | iTask` a
