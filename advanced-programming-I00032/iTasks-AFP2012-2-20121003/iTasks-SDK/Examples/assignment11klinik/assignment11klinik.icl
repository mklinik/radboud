module assignment11klinik

import StdMisc
import confSM
import iTask_semantics
import derived_combinators

initState = { State | mem = [], taskNo = 0, timeStamp = 0 }

taskNoOfEditor responses label =
  case [taskNo \\ (taskNo, EditorResponse er) <- responses | er.EditorResponse.description == label] of
    [] = abort ("taskNoOfEditor: no such editor: " +++ label)
    [x:_] = x

labelOfTask responses taskNo =
  case [er.EditorResponse.description \\ (currentTaskNo, EditorResponse er) <- responses | taskNo == currentTaskNo] of
    [] = abort "labelOfTask: no such task"
    [x:_] = x

:: UserEvent
  = UserEditEvent String Int
  | UserActionEvent Action

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
  // Actions belong to the sequential combinator (>>>*) task, which is not
  // labelled and hence must be addressed by its taskId.
  [  UserActionEvent action
  \\ (_, ActionResponse` actionResponses) <- responses
  ,  (action, enabled) <- actionResponses
  |  enabled
  ]
where
  (_, responses, _) = task RefreshEvent` state

toRealEvent :: UserEvent (Task` a, State) -> Maybe Event`
toRealEvent (UserEditEvent label value) (task, state) =
  case [taskId \\ (taskId, EditorResponse er) <- responses
                | er.EditorResponse.description == label] of
    [] = Nothing
    [taskId:_] = Just (EditEvent` taskId (serialize` value))
where
  (_, responses, _) = task RefreshEvent` state
toRealEvent (UserActionEvent action) (task, state) =
  case [taskId \\ (taskId, ActionResponse` actions) <- responses
                | isMember action (map fst actions)] of
    [] = Nothing
    [taskId:_] = Just (ActionEvent` taskId action)
where
  (_, responses, _) = task RefreshEvent` state

eventTaskId :: Event` -> TaskNo
eventTaskId event = case event of
  EditEvent`   taskId _ = taskId
  ActionEvent` taskId _ = taskId

errorResponse = EditorResponse { EditorResponse
                               | description = "no transition for input"
                               , editValue = (serialize` False, serialize` False)
                               , editing = Displaying
                               }

// specTask and iutTask are essentially the same, only the type of the result
// is a bit different. We pass a function for that.
stepTask :: (Task` a, State) UserEvent ([Response] (Task` a, State) -> b) -> b
stepTask (task, state) input mkResult =
  case (toRealEvent input (task, state)) of
    // The task does not have an editor of action to whom the input event may be sent
    Nothing = mkResult [errorResponse] (task, state)
    (Just realInput)
      # ((Reduct (ValRes` _ _) newTask), responses, newState) = task realInput state
      // return only the responses from the task where we sent the input to
      = mkResult [response \\ (senderTaskId, response) <- responses
                            | senderTaskId == eventTaskId realInput]
                 (newTask, newState)

specTask :: (Task` a, State) UserEvent -> [Trans Response (Task` a, State)]
specTask state input = stepTask state input (\responses newState -> [Pt responses newState])

iutTask :: (Task` a, State) -> UserEvent -> ([Response], (Task` a, State))
iutTask state = \input -> stepTask state input (\responses newState -> (responses, newState))

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
      , Ntests 100
      , Nsequences 10
      ]

derive ggen UserEvent, Action
derive bimap []
derive genShow Event`, State, Reduct, Response, UserEvent, Action,
  TaskResult`, EditorResponse, Value`, EditMode, Stability`

genShow{|SerializedValue|} _ _ _ c = c

//Start world = taskConformance world task5 task6 // should pass, does
//Start world = taskConformance world task1 task2 // should pass, does
//Start world = taskConformance world task2 task1 // should fail, does
//Start world = taskConformance world task3 task4 // should pass, does
//Start world = taskConformance world task4 task3 // should pass, does
//Start world = taskConformance world task5 task6 // should pass, does
//Start world = taskConformance world task6 task5 // should pass, does
Start world = taskConformance world taskX taskY // should fail, does

task1 = (simplified_edit "edit500" 42)
task2 = (simplified_edit "edit500" 42) >>>* [OnAction` (Action "Ok") (isValue) (return` o getValue)]

taskX = (simplified_edit "edit500" 42)
taskY = (simplified_edit "edit501" 42) >>>* [OnAction` (Action "Ok") (isValue) (return` o getValue)]

task3 = (taskX .||. taskY)
task4 = (taskY .||. taskX)

task5 = (simplified_edit "foo" 42    .||. simplified_edit "bar" (-42))
task6 = (simplified_edit "bar" (-42) .||. simplified_edit "foo" 42)
