module assignment05klinik

import iTasks

/* === Exercise 1: reallyAllTasks === */

reallyAllTasks :: [Task a] -> Task [a] | iTask a
reallyAllTasks tasks = parallel Void [(Embedded, const t) \\ t <- tasks]
    @ (map snd) // discard TaskTimes
    @? secretAlienTechnology
  where
    secretAlienTechnology :: (TaskValue ([TaskValue a])) -> TaskValue [a]
    secretAlienTechnology x = join (fmap (foldl (liftA2 (flip cons)) (Value [] Stable)) x)
    /* In case the above line gives you a headache (it sure gives me one) here
     * is what it does, from inside to outside. As TaskValue is just a fancy
     * Maybe, let's talk about Maybe for this explanation.
     *  - foldl with the lifted list constructor turns [Maybe a] into
     *    Maybe [a], such that if any element is Nothing, the final result is
     *    Nothing.
     *  - because the list of Maybes is inside another Maybe, we must fmap
     *    the left-fold.
     *  - this leaves us with Maybe (Maybe [a]) which we flatten into Maybe [a]
     *    using the monadic vleeshamer also known as "join".
     * Never mind that TaskValue is probably not a proper Monad or Applicative,
     * but this way of thinking helps.
     */


/* === Exercise 2.1: fixed multi-user chat === */

// The shared data structure for both fixed and flexo multi-user chat: an
// association list which has as key the stringified User, and as value the
// text which the user has typed in the chat.
//
// I'm sure that a different type for the key would be more efficient, but this
// one is easiest for displaying the chat state.
:: Notes :==  [(String, String)]


// How the fixed chat works: One user creates the channel, and selects all
// users that are supposed to participate.  One parallel task is created for,
// and assigned to every chosen participant.
//
// The participants have to log in, and select the already running task that
// was created for them to start chatting.
fixedMultiChat =
  ( enterInformation "Please enter name for the channel" []
    -&&-
    (get currentUser >>= \me -> get users >>= \users -> pickUsers (filter ((=!=) me) users) [me])
  )
  >>= \(channelName, fellas) -> withShared [(toString u, "") \\ u <- fellas] (fixedMultiChatImpl fellas channelName)

fixedMultiChatImpl fellas channelName notes =
  parallel (channelName +++ ": chat control center") [ makeChatTaskForUser channelName notes u \\ u <- fellas ]


// This function is used by both fixed and flexo chat.
makeChatTaskForUser :: String (Shared Notes) User -> (ParallelTaskType, ParallelTask Void)
makeChatTaskForUser channelName notes (u=:(AuthenticatedUser userId _ _)) =
  ( Detached { ManagementMeta
                | title=Just $ userId +++ "@" +++ channelName
                , worker=(UserWithId userId)
                , role=Nothing
                , startAt=Nothing
                , completeBefore=Nothing
                , notifyAt=Nothing
                , priority=NormalPriority
                }
   , (const $ (enterInformation ("[" +++ channelName +++ "] say something") [] @> (updateNotes u, notes))
              ||-
              viewSharedInformation "what everybody says:" [ViewWith Display] notes
              >>| return Void)
   )
where
  updateNotes :: User (TaskValue (Maybe String)) [(String, String)] -> (Maybe [(String, String)])
  updateNotes u val notes = Just $ updateAssoc (toString u) (maybe "" id $ maybeValue Nothing id val) notes


pickUser :: [User] [User] -> Task User
pickUser toPickFrom alreadyPicked =
  enterChoice "Please select a user to chat with" [] toPickFrom
  -||
  viewInformation "Already picked fellas: " [] (map toString alreadyPicked)

// Continue picking users until the Ok butten gets clicked.
// The Ok button can always be clicked, regardless of the current selection.
pickUsers :: [User] [User] -> Task [User]
pickUsers toPickFrom alreadyPicked =
      pickUser toPickFrom alreadyPicked
  >>* [ OnAction (Action "Add another user") hasValue
          (\(Value v _) -> pickUsers (filter ((=!=) v) toPickFrom) [v:alreadyPicked])
      , OnAction ActionOk always (\v -> return $ maybeValue alreadyPicked ((flip cons) alreadyPicked) v)
      ]


/* === Exercise 2.2: dynamic multi-user chat === */

// How the flexo chat works: One user, say A, creates the channel. A is the
// only one who can add and remove users to/from the channel.  Initially, there
// are no users in the channel, not even A.  A starts by adding a few users to
// the channel.
//
// When user B is added, a new task is created and assigned to B. B has to log
// in and open that task to participate in the chat.
//
// When user B is kicked, B's result is removed from the shared data structure
// and all tasks assigned to B are cancelled.
//
// Please ignore that the same user can be added multiple times to the chat.
// Precluding this is just a matter of tedious but straight-forward
// bookkeeping, which is not the point here, is it?
flexoMultiChat =
  enterInformation "Please enter a name for the channel" []
  // initially, the association list is empty, and there is only the controlCenter task
  >>= \channelName -> withShared [] (\notes -> parallel (channelName +++ " control center")
                                                        [(Embedded, controlCenter channelName notes)]
                                    )
where
  controlCenter :: String (Shared Notes) (SharedTaskList Void) -> Task Void
  controlCenter channelName notes taskList =
        get users >>= \us -> enterChoice "Please select a user" [] us
    >>* [ OnAction (Action "Add user") hasValue $
            // add a new entry to the shared association list
            \(Value user _) -> update (cons (toString user, "")) notes
            // spawn a new chat task
            >>| (uncurry appendTask) (makeChatTaskForUser channelName notes user) taskList
            >>| return Void
        , OnAction (Action "Kick user") hasValue $
            // remove all entries for this user from the shared association list
            \(Value user _) -> update (filter (((=!=) (toString user)) o fst)) notes
            // remove all tasks for this user from the task list
            >>| get (taskListMeta taskList) @ onlyDetachedTasksForUser user @ map (\id -> removeTask id taskList)
            >>= sequence "" // do it!
            >>| return Void
        ]
    >>| controlCenter channelName notes taskList // re-spawn control center


// Yields all TaskIds of the given TaskListItems which are detached tasks assigned to the given user.
onlyDetachedTasksForUser :: User [TaskListItem a] -> [TaskId]
onlyDetachedTasksForUser _ [] = []
onlyDetachedTasksForUser user [{TaskListItem | taskId, managementMeta}:rest] =
  case managementMeta of
    Just {ManagementMeta | worker} =
      case worker of
        UserWithId userId =
          if (isUserWithId user userId)
             [taskId:meh]
             meh
        = meh
    = meh
where
  meh = onlyDetachedTasksForUser user rest


isUserWithId :: User UserId -> Bool
isUserWithId (AuthenticatedUser userId _ _) givenId = userId === givenId
isUserWithId _ _ = False


/* === Blurb: The inevitable boilerplate and helper functions === */

// replaces a value in an association list with a new value
updateAssoc :: key value [(key, value)] -> [(key, value)] | gEq{|*|} key
updateAssoc _ _ [] = []
updateAssoc key newValue [x=:(k, v):xs]
  | key === k = [(k, newValue) : xs]
  | otherwise = [x : updateAssoc key newValue xs]


assignment05klinik :: [Workflow]
assignment05klinik =
  [ workflow "reallyAllTasks" "show a demo of the reallyAllTasks combinator" reallyAllTasksDemo
  , workflow "multi-person chat, fixed" "chat with other persons" fixedMultiChat
  , workflow "multi-person chat, dynamic" "chat with other persons" flexoMultiChat
  , workflow "Manage users" "Manage system users..." manageUsers


  // Side note: there is a difference when enterChoice is used in a lambda or
  // directly.  If used directly, there is no "Continue" button.
  , workflow "pick user 1" "pick user 1" pickUser1
  , workflow "pick user 2" "pick user 2" pickUser2
  , workflow "pick int 1" "pick int 1" pickInt1
  , workflow "pick int 2" "pick int 2" pickInt2
  ]

pickUser1 = get users        >>= \xs -> enterChoice "pick one" [] xs >>= viewInformation "you picked: " []
pickUser2 = get users        >>=        enterChoice "pick one" []    >>= viewInformation "you picked: " []
pickInt1  = return [1, 2, 3] >>= \xs -> enterChoice "pick one" [] xs >>= viewInformation "you picked: " []
pickInt2  = return [1, 2, 3] >>=        enterChoice "pick one" []    >>= viewInformation "you picked: " []

Start :: *World -> *World
Start world = startEngine (browseExamples assignment05klinik) world
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


reallyAllTasksDemo = reallyAllTasks [enterInt, enterInt, enterInt] >>=
  viewInts "And now for something completely different."

enterInt :: Task Int
enterInt = enterInformation "Please enter an integer" []

viewInts :: String [Int] -> Task [Int]
viewInts name value = viewInformation name [] value


always = const True

hasValue (Value _ _) = True
hasValue _ = False

maybeValue :: b (a -> b) (TaskValue a) -> b
maybeValue _ f (Value v _) = f v
maybeValue b _ _ = b

cons x xs = [x:xs]

flip f x y = f y x

join :: (TaskValue (TaskValue a)) -> TaskValue a
join NoValue = NoValue
join (Value NoValue _) = NoValue
join (Value (Value v s) _) = Value v s // not sure about the stability

class Applicative f where
  pure :: a -> f a
  (<*>) infixl 1 :: (f (a -> b)) (f a) -> (f b)

instance Applicative TaskValue where
  pure v = Value v Stable
  (<*>) NoValue _ = NoValue
  (<*>) _ NoValue = NoValue
  (<*>) (Value f _) (Value x s) = Value (f x) s // not sure about the stability.

(<$>) infixl 2 :: (a -> b) (f a) -> f b | Applicative f
(<$>) f a = (pure f) <*> a

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f
liftA2 f x y = f <$> x <*> y

($) infixr 0 :: (a -> b) a -> b
($) f a = f a
