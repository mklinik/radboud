module assignment11klinik

import StdMisc
import confSM
import iTask_semantics
import derived_combinators

// simple example to remember how it worked:

// inputs and outputs are the same for spec and impl
:: Input :== Int
:: Output :== Int
:: MklState :== Int

// as states, we just use integers
specStartState :: MklState
specStartState = 0

implStartState :: MklState
implStartState = 0

implResetFunction :: MklState -> MklState
implResetFunction _ = 0

// Spec state input output is a function from
//   State Input -> [Trans Output State]
//
// and Trans is either a single point
//   Pt [Output] State
// or a function
//   Ft ([Output] -> [State])
//spec :: Spec State Input Output
spec state input = [Pt [state + input] (state + input)]

// The implementation is a curried function
//   State -> Input -> ([Output], State)
//impl :: IUTstep State Input Output
impl 30 = \input -> ([0], 30 + input)
impl state = \input -> ([state + input], state + input)


//Start world =
  //testConfSM
    //[ Nsequences 10
    //, InputFun (const (const [1..100]))
    //] // options
    //spec
    //specStartState
    //impl
    //implStartState
    //implResetFunction
    //world

initState = { State | mem = [], taskNo = 0, timeStamp = 0 }

taskNoOfEditor responses label =
  case [taskNo \\ (taskNo, EditorResponse er) <- responses | er.EditorResponse.description == label] of
    [] = abort ("taskNoOfEditor: no such editor: " +++ label)
    [x:_] = x

labelOfTask responses taskNo =
  case [er.EditorResponse.description \\ (currentTaskNo, EditorResponse er) <- responses | taskNo == currentTaskNo] of
    [] = abort "labelOfTask: no such task"
    [x:_] = x

foo responses =
  [(taskId, er.EditorResponse.description) \\ (taskId, EditorResponse er) <- responses]

Start world = taskConformance world (task1 .||. task2) (task2 .||. task1)
//Start = properEvents (task, initState)
//Start = foo responses
  where
    (_, responses, _) = task (RefreshEvent`) initState
    task = ((simplified_edit "edit1" 42 .||. simplified_edit "edit4" 10) >>>*
      [ OnAction` (Action "BLAHBAR") (const True) (const (simplified_edit "edit2" (-42)))
      , OnAction` (Action "MOOOOO!") (const True) (const (simplified_edit "edit3" (142)))
      ])

//task1 :: Task` Int
task1 = (simplified_edit "edit500" 42)
task2 = (simplified_edit "edit500" 42) >>>* [OnAction` (Action "Ok") (const True) (\_ -> return` 0)]

:: UserEvent
  = UserEditEvent String Int
  | UserActionEvent Int Action

// I always wondered what list comprehensions are good for when you have map,
// fold and filter.  Thank's for teaching me, Peter!
properEvents :: (Task` a, State) -> [UserEvent]
properEvents (task, state) =
  // proper events are edit events (we only toggle between a few values), ...
  [  UserEditEvent er.EditorResponse.description value
  \\ (_, EditorResponse er) <- responses
  ,  value <- [0, 1, 42]
  ]
  ++
  // ... and action events of all available actions.
  // Actions belong to the sequential combinator task, which is not labelled
  // and hence must be addressed by its taskId.  At least that's what I
  // think.
  [  UserActionEvent taskId action
  \\ (taskId, ActionResponse` actionResponses) <- responses
  ,  (action, enabled) <- actionResponses
  |  enabled
  ]
where
  (_, responses, _) = task RefreshEvent` state

toRealEvent :: UserEvent (Task` a, State) -> Event`
toRealEvent (UserEditEvent label value) (task, state) = EditEvent` taskId (serialize` value)
where
  taskId = case [taskId \\ (taskId, EditorResponse er) <- responses | er.EditorResponse.description == label] of
    [] = abort ("toRealEvent: no such editor:" +++ label)
    [x:_] = x
  (_, responses, _) = task RefreshEvent` state
toRealEvent (UserActionEvent taskId action) _ = ActionEvent` taskId action

specTask :: (Task` a, State) UserEvent -> [Trans Response (Task` a, State)]
specTask (task, state) input
  # ((Reduct (ValRes` _ taskValue) newTask), responses, newState) = task (toRealEvent input (task, state)) state
  = [Pt (map snd responses) (newTask, newState)]

iutTask :: (Task` a, State) -> UserEvent -> ([Response], (Task` a, State))
iutTask (task, state) = iutTask` (task, state)

iutTask` (task, state) input
  # ((Reduct (ValRes` _ taskValue) newTask), responses, newState) = task (toRealEvent input (task, state)) state
  = ((map snd responses), (newTask, newState))

taskConformance :: *World (Task` a) (Task` a) -> *World  | gEq {| * |} a & genShow {| * |} a
taskConformance world task1 task2
  = snd (testConfSM options
                    specTask
                    (task1, initState)
                    iutTask
                    (task2, initState)
                    (const (task2, initState))
                    world
        )
  where
    options =
      [ InputFun (const properEvents)
      , Nsequences 10
      ]

derive ggen UserEvent, Action
derive bimap []
derive genShow Event`, State, Reduct, Response, UserEvent, Action, JSONNode,
  TaskResult`, EditorResponse, Value`, EditMode, Stability`
