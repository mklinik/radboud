implementation module Collection

import iTasks
/*
* General purpose management task for a collection of data
*/
manageCollection :: !d !String (c -> i) (Shared [c]) -> Task (Maybe i) | descr d & iTask c & iTask i
manageCollection desc itemname identify collection
	= manageCollectionWith desc (selectItem Void) (viewItem (Title ("Details of " +++ itemname)))
		[AnyTime ActionNew (\_ -> addItem (Title ("Add " +++ itemname)) collection identify)
		,WithResult ActionEdit (const True) (editItem ("Edit " +++ itemname) collection (itemShare identify) identify)
		,WithResult ActionDelete (const True) (deleteItem ("Delete " +++ itemname,"Are you sure you want to delete the following " +++ itemname +++ "?") collection (itemShare identify) identify)
		]
		identify
		(itemShare identify)
		collection
/*
* Customizable management task for a collection of data
*/
manageCollectionWith ::
	!d																			//Description
	((Shared [c]) (c -> i) -> Task i)											//Make selection
	((Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task a)		//Use selection
	[TaskStep i (Maybe i)]														//Actions
	(c -> i)																	//Identification function
	((Shared [c]) i -> Shared (Maybe c))										//Item share function
	(Shared [c])																//Shared collection
	-> Task (Maybe i) | descr d & iTask c & iTask i & iTask a
manageCollectionWith desc makeSelection useSelection selectionActions identify itemShare collection
	=	feedForward desc (makeSelection collection identify)
		( \mbSel -> 
			forever (
				whileUnchanged mbSel (\sel -> useSelection collection itemShare sel @ const sel @? onlyJust >>* selectionActions)
			)
		)
where
	onlyJust (Value (Just x) s)	= Value x s
	onlyJust _					= NoValue


itemShare :: (c -> i) (Shared [c]) i -> Shared (Maybe c) | gEq{|*|} i & gEq{|*|} c
itemShare identify collection i = mapReadWrite (toItem,fromItem) collection
where
	toItem l	= case [c \\ c <- l | identify c === i] of
		[c]		= Just c
		_		= Nothing
	
	fromItem Nothing l 		= Just l
	fromItem (Just c`) l	= Just [if (identify c === i) c` c \\ c <- l]

selectItem :: !d (Shared [c]) (c -> i) -> Task i | descr d & iTask c & iTask i
selectItem desc collection identify
	=	enterSharedChoice desc [] collection
	@	identify

viewItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task (Maybe i) | descr d & iTask c & iTask i
viewItem desc collection itemShare Nothing	= viewInformation desc [] "Make a selection first..." @ const Nothing
viewItem desc collection itemShare (Just i)	= viewSharedInformation desc [] (itemShare collection i) @ const (Just i)

addItem :: !d (Shared [c]) (c -> i) -> Task (Maybe i) | descr d & iTask i & iTask c
addItem desc collection identify
	=	enterInformation desc []
	>>*	[AnyTime ActionCancel (\_ -> return Nothing)
		,WithResult ActionOk (const True) (\item -> update (\l -> l ++ [item]) collection >>| return (Just (identify item)))
		]

editItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | descr d & iTask c & iTask i
editItem desc collection itemShare identify i
	=	get (itemShare collection i)
	>>= \mbItem -> case mbItem of
			Nothing		= (return Nothing)
			(Just item)	=	updateInformation desc [] item
						>>*	[AnyTime ActionCancel (\_ -> return Nothing)
							,WithResult ActionOk (const True) (\item` -> 
													update (\l -> [if (identify c === i) item` c \\ c <- l ]) collection
													>>| return (Just i)
												 )
							]

deleteItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | descr d & iTask c & iTask i
deleteItem desc collection itemShare identify i
	=	viewSharedInformation desc [] (itemShare collection i)
	>>*	[AnyTime ActionNo (\_ -> return Nothing)
		,AnyTime ActionYes (\_ -> update (\l -> [c \\ c <- l | identify c =!= i]) collection >>| return Nothing)
		]


