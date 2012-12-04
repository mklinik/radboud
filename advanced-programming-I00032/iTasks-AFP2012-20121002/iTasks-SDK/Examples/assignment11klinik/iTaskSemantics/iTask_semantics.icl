implementation module iTask_semantics

/*
	Operational semantics of iTask3.
	
	This version uses a stable/unstable value state of tasks.
	Rewriting aims at obtaining a stable value of a tasks.

	Editors only process edit events
	Step processes commit events and triggers
		
	Peter Achten,     p.achten@cs.ru.nl
	Pieter Koopman,   pieter@cs.ru.nl
	Bas Lijnse,       b.lijnse@cs.ru.nl
	Steffen Michels,  s.michels@cs.ru.nl
	Rinus Plasmeijer, rinus@cs.ru.nl
	
	Run test with Show basic values only as project option
*/

import StdList
from   StdFunc import flip
import fmap
import Func
from   SystemTypes import :: Action(..), :: ActionName, instance == Action

import iTasks
derive class iTask State, Event`, Response, EditorResponse, Value`, EditMode, Reduct, TaskResult`, Stability`


//=== state and utility functions on the state

initState							:: State
initState							= {mem = [], taskNo = 0, timeStamp = 0}

newTask								:: (TaskNo` TimeStamp` ->  Task` a) -> Task` a 
newTask f							= \ev st -> f st.taskNo st.timeStamp ev {st & taskNo = st.taskNo+1}

// Shared Store
// utility functions for handling shares in the state 

serialize`							:: !a -> SerializedValue | JSONEncode{|*|} a
serialize`    v						= toJSON v

deserialize`						:: !SerializedValue -> a | JSONDecode{|*|} a
deserialize` v						= fromJust (fromJSON v)

:: RWShared` r w					= { get :: State -> (r,State)				
									  , set :: w State -> State	
									  }

createShared						:: a State -> (Shared` a,State) | iTask` a
createShared a st=:{mem} 
	= ({ get = get, set = set }, {st & mem = mem ++ [serialize` a]})
where
	idx 		   					= length mem
	get st=:{mem}   				= (deserialize` (mem!!idx),st)
	set a st=:{mem}					= {st & mem = updateAt idx (serialize` a) mem}

withShared`							:: a ((Shared` a) -> Task` b) -> Task` b | iTask` a 
withShared` va tfun					= withShared2
where
	withShared2 ev st				= let (sh_a,st1) = createShared va st in tfun sh_a ev st1

updateShared						:: (r -> w) (RWShared` r w) State -> (w,State)
updateShared f share st
# (readv,st)						= share.get st
# writev							= f readv
= (writev,share.set writev st)

updateMaybeShared					:: (r -> Maybe w) (RWShared` r w) State -> (Maybe w,State)
updateMaybeShared f share st
# (readv,st)						= share.get st
= case f readv of
	Nothing							= (Nothing,st)
	Just wv							= (Just wv,share.set wv st)

stable`								:: TimeStamp` a -> Task` a
stable` t a							= \ev st -> (Reduct (ValRes` t (Val` a Stable`)) (stable` t a), [], st)

return`								:: a -> Task` a
return` a							= \ev st -> stable` st.timeStamp a ev st

throw`								:: Ex -> Task` a
throw` ex							= \ev st -> (Reduct (ExcRes` ex) (throw` ex), [], st)

constShare							:: a -> ROShared` a
constShare a						= {get = \st -> (a,st), set = \_ -> id}

readOnly							:: (RWShared` r w)	-> ROShared` r
readOnly sh							= {sh & set = \_ -> id}


// utility functions

isStable							:: !(Value` a) -> Bool
isStable (Val` _ Stable`)			= True
isStable _							= False

isValue								:: !(Value` a) -> Bool
isValue (Val` _ _)					= True
isValue _							= False

getValue							:: !(Value` a) -> a
getValue (Val` a _)					= a


// basic tasks

edit`								:: String l (RWShared` r w) (l r -> Maybe a) -> Task` a | iTask` l & iTask` r 
edit` descr lv sh_rw conv			= newTask (edit1 lv)
where
	edit1 lv tn t ev st 
	# (nt,nlv)						= case ev of
										EditEvent` tid dyn -> if (tid == tn) (st.timeStamp,deserialize` dyn) (t,lv)
										_                  -> (t,lv) 
	# (sr,st)						= sh_rw.get st 
	= ( Reduct (ValRes` nt (toValue (conv nlv sr))) (edit1 nlv tn nt)
	  , [editResponse tn descr Editing (nlv, sr)]
	  , st
	  )

	toValue							:: (Maybe a) -> Value` a
	toValue (Just a)				= Val` a Unstable`
	toValue Nothing					= NoVal`

editResponse						:: TaskNo` String EditMode (l, g) -> (TaskNo`, Response) | JSONEncode{|*|} l & JSONEncode{|*|} g
editResponse tn descr mode (l,g)	= (tn, EditorResponse { description = descr, editing = mode, editValue = (serialize` l, serialize` g) })

//	specialized version of edit` in case no shared is available (typically at top-level of your task flow)
simplified_edit						:: String a -> Task` a | iTask` a
simplified_edit descr lv			= edit` descr lv (constShare 0) (\l _ -> Just l)

show`								:: String a (RWShared` r w) -> Task` a | iTask` r & iTask` a
show` descr v sh_rw					= newTask show1
where
	show1 tn ts0 ev st
	# (sharedv,st)					= sh_rw.get st
	= ( Reduct (ValRes` ts0 (Val` v Stable`)) (show1 tn ts0)
	  , [editResponse tn descr Displaying (v, sharedv)]
	  , st
	  )

simplified_show						:: String a -> Task` a | iTask` a
simplified_show descr v				= show` descr v (constShare 0)

// core combinators: step & task map

(>>>*) infixl 1						:: (Task` a) [TaskStep` a b] -> Task` b
(>>>*) ta steps						= newTask (step1 ta)
where
	step1 ta tn t0 ev st
	# (Reduct tval nta, rsp, st)	= ta ev st
	= hd (  findTrigger tval
	     ++ findAction  tval ev
	     ++ [step1` tval nta rsp]
	     ) ev st
	where
		findTrigger (ExcRes`   e)	= catchers e ++ [throw` e]
		findTrigger (ValRes` _ v)	= triggers v
		
		findAction (ValRes` _ v) (ActionEvent` tid act)
		| tid == tn					= actions act v
		findAction _ _				= []
		
		step1` (ValRes` t v) nta rsp _ st
									= (Reduct no_tval (step1 nta tn t0), nrsp ++ rsp, st)
		where
			no_tval					= ValRes` t NoVal` 
			as						= [(a,p v) \\ OnAction` a p _ <- steps]
		    nrsp					= if (isEmpty as) [] [(tn, ActionResponse` as)]

	catchers    e					= [etb e \\ OnException`  etb <- steps                  ]
	triggers    v					= [atb v \\ OnValue`    p atb <- steps |             p v]
	actions act v					= [atb v \\ OnAction` a p atb <- steps | act == a && p v]


//Transform combinator

(@@?) infixl 1						:: (Task` a) ((Value` a) -> Value` b) -> Task` b 
(@@?) ta f							= transform ta f
where
	transform ta f ev st			= case ta ev st of
										(Reduct (ValRes` t val) nta, rsp, nst)
											->	case f val of
												Val` b Stable` -> stable` t b ev nst
												nval           -> (Reduct (ValRes` t nval) (nta @@? f), rsp, nst)
										(Reduct (ExcRes` e) _, _, nst)
											-> throw` e ev nst

//Project combinator (writes a task result to a share, when the value changes
(@@>) infixl 1						:: (Task` a) !((Value` a) r -> Maybe w, RWShared` r w) -> Task` a | iTask` a
(@@>) ta upd=:(f,sh_rw)				= update NoVal` ta
where
	update otval ta ev st			= case ta ev st of
										(Reduct (ExcRes` e) nta, _, nst)
											-> throw` e ev nst
										(Reduct (ValRes` t ntval) nta, rsp, nst)
											-> ( Reduct (ValRes` t ntval) (update ntval nta)
											   , rsp
											   , if (ntval === otval) nst (snd (updateMaybeShared (f ntval) sh_rw nst))
											   )

// parallel
parallel`							:: [ParTask a] -> Task` [(TimeStamp`,Value` a)] | iTask` a
parallel` tas						= parallel2
where
	parallel2 ev st					= let (stt,st1) = createTaskTable tas st in evalParTasks stt ev st1

createTaskTable						:: [ParTask a] State -> (STaskTable a,State) | iTask` a
createTaskTable tas st
# (stt,    st)						= createShared [] st
# (reducts,st)						= mapSt (reduct stt) tas st 
# st								= stt.set [(pid,reduct) \\ reduct <- reducts & pid <- [0..]] st
= (stt,st)
where
	reduct stt ta st				= (Reduct no_tval (ta stt), st)
	no_tval							= ValRes` st.timeStamp NoVal`

evalParTasks						:: (STaskTable a) Event` State -> (Reduct [(TimeStamp`,Value` a)],Responses,State) | iTask` a
evalParTasks stt ev st
# (tt,st)							= stt.get st
= case foldl (evalParTask stt ev) (Nothing,[],st) tt of
	(Just (ExcRes` e),_,st)			= throw` e ev st
	(no_exception, rsp, st)
	# (values,st)					= mapSt value (rangeFM tt) st
	# maxt							= foldr max 0 (map fst values)
	# (sf,nta)						= if (all (isStable o snd) values)
									     (Stable`,   stable` maxt values)
									     (Unstable`, evalParTasks stt)
	= (Reduct (ValRes` maxt (Val` values sf)) nta,rsp,st)
where
	value (Reduct (ValRes` t val) _) st
									= ((t,val),st)

	de_serialize_val NoVal`			= NoVal`
	de_serialize_val (Val` a s)		= Val` (deserialize` a) s


evalParTask							:: (STaskTable a) Event` (Maybe (TaskResult` a),Responses,State) (TaskEntry a)
									-> (Maybe (TaskResult` a),Responses,State)
evalParTask stt ev (Nothing,rsp,st) (pid,Reduct _ ta)
# (newr,nrsp,st)					= ta ev st
# (Reduct ntval nta)				= newr
= case ntval of
	ExcRes` _						-> (Just ntval,[],st)
	_								-> (Nothing,nrsp ++ rsp,snd (updateShared (updateFM (pid,newr)) stt st))
evalParTask _ _ exception _			= exception


/* parallel without garbage collection:
*/
:: ParTask2        a				:== (SharedTaskList2 a) -> Task` a
:: SharedTaskList2 a				:== Shared` (TaskList` a)
:: TaskList`       a				:== [(Pid a,Reduct a)]
:: Pid a							:== Int

parallel2							:: [ParTask2 a] -> Task` [(TimeStamp`,Value` a)] | iTask` a
parallel2 pts						= parallel2` pts
where
	parallel2` pts ev st			= let (stt,st1) = createTaskList pts st in parallel`` stt ev st1

createTaskList						:: [ParTask2 a] State -> (SharedTaskList2 a,State) | iTask` a
createTaskList pts st=:{timeStamp=t}
# (stt,st)							= createShared [] st
= (stt,stt.set [  (pid,Reduct (ValRes` t NoVal`) (ta stt))
               \\ ta <- pts & pid <- [0..]
               ] st
  )

parallel``							:: (SharedTaskList2 a) -> Task` [(TimeStamp`,Value` a)] | iTask` a
parallel`` stt
	= \ev st -> 
	case evalParTasks` stt ev st of
	  (Left (ExcRes` e),st)			= throw` e ev st
	  (Right rsp,st)
	  # (values,st)					= get_task_values stt st
	  # maxt						= foldr max 0 (map fst values)
	  | all (isStable o snd) values	= stable` maxt values ev st
	  | otherwise					= (Reduct (ValRes` maxt (Val` values Unstable`)) (parallel`` stt),rsp,st)

evalParTasks`						:: (SharedTaskList2 a) Event` State -> (Either (TaskResult` a) Responses,State) | iTask` a
evalParTasks` stt ev st				= let (tt,st1) = stt.get st in foldl (evalParTask` stt ev) (Right [],st1) tt

evalParTask`						:: (SharedTaskList2 a) Event` (Either (TaskResult` a) Responses,State) (Pid a,Reduct a)
									-> (Either (TaskResult` a) Responses,State)
evalParTask` stt ev (Right rsp,st) (pid,Reduct _ ta)
# (newr,nrsp,st)					= ta ev st
# (Reduct ntval nta)				= newr
| isExcRes ntval					= (Left ntval,st)
# (_,st)							= updateShared (updateFM (pid,newr)) stt st
= (Right (nrsp ++ rsp),st)
evalParTask` _ _ (Left e,st) _		= (Left e,st)

get_task_values :: (SharedTaskList2 a) State -> ([(TimeStamp`, Value` a)],State)
get_task_values stt st				= let (tt,st1) = stt.get st in ([(t,val) \\ (_,Reduct (ValRes` t val) _) <- tt],st1)

isExcRes (ExcRes` _)				= True
isExcRes _							= False
