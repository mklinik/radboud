implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, SystemTypes, GenRecord, HTTP, Map, Util
import GenVisualize, iTaskClass, IWorld
from TaskState			import :: TaskTree(..), :: DeferredJSON(..), :: TIMeta(..)
from LayoutCombinators	import :: Layout(..), autoLayout
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
mkInstantTask iworldfun = Task (evalOnce iworldfun)
where
	evalOnce f _ repOpts (TCInit taskId ts) iworld = case f taskId iworld of	
		(Ok a,iworld)							= (ValueResult (Value a Stable) {lastEvent=ts,expiresIn=Nothing} (finalizeRep repOpts rep) (TCStable taskId ts (DeferredJSON a)), iworld)
		(Error (e,s), iworld)					= (ExceptionResult e s, iworld)

	evalOnce f _ repOpts state=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a Stable) {lastEvent=ts,expiresIn=Nothing} (finalizeRep repOpts rep) state, iworld)
		Nothing	= (exception "Corrupt task result", iworld)

	evalOnce f _ _ (TCDestroy _) iworld	= (DestroyedResult,iworld)

	rep = TaskRep (UIControlGroup (put TYPE_ATTRIBUTE "single" newMap,[],Vertical,[])) []

fromJSONOfDeferredJSON :: !DeferredJSON -> Maybe a | TC a & JSONDecode{|*|} a
fromJSONOfDeferredJSON (DeferredJSON v)
	= case make_dynamic v of
		(v :: a^)
			-> Just v
fromJSONOfDeferredJSON (DeferredJSONNode json)
	= fromJSON json

make_dynamic v = dynamic v

derive gGetRecordFields	TaskValue, Stability
derive gPutRecordFields	TaskValue, Stability

JSONEncode{|Task|} _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Task|} _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a	= Task (\_ -> abort funerror)
	funerror		= "Creating default task functions is impossible"
	
gUpdate{|Task|} _ (UDSearch t) ust = basicSearch t (\Void t -> t) ust

gVerify{|Task|} _ _ vst = alwaysValid vst

gVisualizeText{|Task|} _ _ _ = ["<Task>"]
gVisualizeEditor{|Task|} _ _ _ _ _ vst = (NormalEditor [(stringDisplay "<Task>",newMap)],vst)

gHeaders{|Task|} _ _ = ["Task"]
gGridRows{|Task|} _ _ _ _	= Nothing	
gEq{|Task|} _ _ _			= True // tasks are always equal??

gGetRecordFields{|Task|} _ _ _ fields = fields
gPutRecordFields{|Task|} _ t _ fields = (t,fields)

exception :: !e -> TaskResult a | TC, toString e
exception e = ExceptionResult (dynamic e) (toString e)

repLayout :: TaskRepOpts -> Layout
repLayout {TaskRepOpts|useLayout,modLayout}	= (fromMaybe id modLayout) (fromMaybe autoLayout useLayout)

afterLayout :: TaskRepOpts -> (UIDef -> UIDef)
afterLayout {TaskRepOpts|afterLayout} = fromMaybe id afterLayout

finalizeRep :: TaskRepOpts TaskRep -> TaskRep
finalizeRep repOpts=:{TaskRepOpts|appFinalLayout=True} rep=:(TaskRep def parts) = TaskRep (UIFinal ((repLayout repOpts).Layout.final def)) parts
finalizeRep repOpts rep = rep

instance Functor TaskValue
where
	fmap f (NoValue)		= NoValue
	fmap f (Value v s)		= Value (f v) s
