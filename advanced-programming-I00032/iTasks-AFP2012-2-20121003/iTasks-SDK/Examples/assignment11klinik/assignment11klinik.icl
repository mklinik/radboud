module assignment11klinik

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
  hd [taskNo \\ (taskNo, EditorResponse er) <- responses | er.EditorResponse.description == label]

labelOfTask responses taskNo =
  hd [er.EditorResponse.description \\ (currentTaskNo, EditorResponse er) <- responses | taskNo == currentTaskNo]

foo responses =
  [(taskId, er.EditorResponse.description) \\ (taskId, EditorResponse er) <- responses]

//Start = properEvents (task, initState)
Start = foo responses
  where
    (_, responses, _) = task (RefreshEvent`) initState
    task = ((simplified_edit "edit1" 42 .||. simplified_edit "edit4" 10) >>>*
      [ OnAction` (Action "BLAHBAR") (const True) (const (simplified_edit "edit2" (-42)))
      , OnAction` (Action "MOOOOO!") (const True) (const (simplified_edit "edit3" (142)))
      ])

:: UserEvent
  = UserEditEvent String Int
  | UserActionEvent Int Action

// I always wondered what list comprehensions are good for when you have map,
// fold and filter.  Thank's for teaching me, Peter!
properEvents :: (Task` a, State) -> [UserEvent]
properEvents (task, state) =
  // proper events are edit events (we only toggle between a few values), ...
  [  UserEditEvent label value
  \\ label <- availableEditors
  ,  value <- [0, 1, 42]
  ]
  ++
  // ... and action events of all available actions
  [  UserActionEvent taskId action
  \\ (taskId, action) <- availableActions
  ]
where
  availableActions =
    // Actions belong to the sequential combinator task, which is not labelled
    // and hence must be addressed by its taskId.  At least that's what I
    // think.
    [  (taskId, action)
    \\ (taskId, ActionResponse` actionResponses) <- responses
    ,  (action, enabled) <- actionResponses
    |  enabled
    ]
  availableEditors =
    [  er.EditorResponse.description
    \\ (_, EditorResponse er) <- responses
    ]
  (_, responses, _) = task RefreshEvent` state

specTask :: (Task` a, State) UserEvent -> [Trans UserEvent (Task` a, State)]
specTask (task, state) input = case input of
  UserEditEvent label _ = [Pt [input] (task, state)]
  = [Pt [input] (task, state)]

//taskConformance :: *World (Task` a) (Task` a) -> *World  | gEq {| * |} a & genShow {| * |} a
//taskConformance world task1 task2
  //= snd (testConfSM options
                    //specTask
                    //(task1, initState)
                    //iutTask
                    //(task2, initState)
                    //(const (task2, initState))
                    //world
        //)
  //where
    //options = [InputFun (const properEvents) ]
