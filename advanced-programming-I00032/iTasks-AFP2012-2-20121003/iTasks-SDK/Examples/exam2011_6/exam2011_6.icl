module exam2011_6

import iTasks, UserAdmin, WorkflowAdmin
import StdMisc

basicAPIExamples :: [Workflow]
basicAPIExamples =
  [ workflow ("Hello world") "View a constant string" (edit1by1 [1, 2, 3])
  ]

Start :: *World -> *World
Start world = startEngine (browseExamples basicAPIExamples) world
where
  browseExamples examples = forever (
      (viewTitle "iTasks Example Collection"
    ||-
      enterInformation ("Login","Enter your credentials and login or press continue to remain anonymous") [])
    >>* [WithResult (Action "Login") (const True) (browseAuthenticated examples)
      ,Always (Action "Continue") (browseAnonymous examples)
      ])

  browseAuthenticated examples {Credentials|username,password}
    = authenticateUser username password
    >>= \mbUser -> case mbUser of
      Just user   = workAs user (manageWorklist examples)
      Nothing   = viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void

  browseAnonymous examples
    = manageWorklist examples


edit1by1 :: [a] -> Task [a] | iTask a
edit1by1 list = enterChoice "" [] (decorate list) @ snd >>*
  [ OnAction ActionCancel always (const (return list))
  , OnAction (Action "Insert") always (\val -> insertElement list (case val of
      Value index _ = index
      NoValue       = 0))
  , OnAction (Action "Edit") hasValue (editElement list o getValue)
  , OnAction (Action "Delete") hasValue (deleteElement list o getValue)
  ]

decorate :: [a] -> [(a, Int)]
decorate list = decorate_ list 0
decorate_ [] i = []
decorate_ [x:xs] i = [(x, i) : decorate_ xs (i + 1)]

insertElement :: [a] Int -> Task [a] | iTask a
insertElement list index = enterInformation "" [] >>= (\x -> edit1by1 (init ++ [x] ++ end))
where
  init = take index list
  end = drop index list

editElement :: [a] Int -> Task [a] | iTask a
editElement list index = updateInformation "" [] elem >>= (\x -> edit1by1 (init ++ [x] ++ end))
where
  init = take index list
  elem = hd (drop index list)
  end = drop (index + 1) list

deleteElement :: [a] Int -> Task [a] | iTask a
deleteElement list index = edit1by1 (init ++ end)
where
  init = take index list
  end = drop (index + 1) list

//* utility functions

always = const True

hasValue (Value _ _) = True
hasValue _ = False

getValue (Value v _) = v
