implementation module Client

import iTasks
import StdMisc, Text
from Util import timestampToGmDateTime

clientExample :: [Workflow]
clientExample = [workflow "Examples/Client" "This task rebuilds the client." client]

// Authenticated task management
client :: Task Void
client = forever (doAuthenticated manageTasks)

// Authentication
:: Credentials =
	{ username	:: String
	, password	:: Password
	}

derive class iTask Credentials

doAuthenticated :: (Task a) -> Task (Maybe a) | iTask a
doAuthenticated task
	=	(appIdentity ||- enterInformation "Log in" []) <<@ plainInteractionLayout
	>>= \credentials ->
		authenticateUser credentials.username (toString credentials.Credentials.password)
	>>= \mbUser -> case mbUser of
		Nothing
			= showInformation "Log in failed" [] Nothing
		Just user
			=	workAs user task
			>>= transform Just
where
	appIdentity :: Task Void
	appIdentity = showSharedInformation "Application identity" [] applicationName Void >>+ noActions
	
// Task management
manageTasks :: Task Void
manageTasks = mainLayout @>>  parallel "Client" {selectedProcess = Nothing, selectedWorkflow = Nothing} (\_ _ -> Void)
	[ (BodyTask,	\list	-> infoBar 								<<@ infoBarLayout)
	, (BodyTask,	\list	-> chooseWorkflow (taskListState list)	<<@ treeLayout)
	, (BodyTask,	\list	-> showDescription (taskListState list)	<<@ descriptionLayout)
	, (BodyTask,	\list	-> workTabPanel list					<<@ tabParallelLayout)
	, (BodyTask,	\list	-> processTable list					<<@ processTableLayout)
	, (HiddenTask,	\_		-> controlClient)
	]

infoBar :: Task ParallelControl
infoBar = showSharedInformation "Info" [ShowView (GetShared view)] currentUser Void >>+ (\_ -> UserActions [(Action "Log out",Just Stop)])
where
	view user = HtmlDisplay ("<b>Welcome " +++ toString (Text (toString user)) +++ "</b>")
	
chooseWorkflow :: !(Shared ClientState) -> Task ParallelControl
chooseWorkflow state = updateSharedInformation "Tasks" [UpdateView (GetLocalAndShared mkTree, Putback putback)] (state >+| allowedWorkflowTree) Nothing >>+ noActions
where
	mkTree sel (_,flows) = mkTreeChoice (fmap (\{path,description,workflowId} -> (last (split "/" path),(workflowId,description))) flows) sel
	putback tree _ (state,_) = (Just (Just selection), Just {state & selectedWorkflow = Just selection})
	where
		selection = getSelection tree

showDescription :: !(Shared ClientState) -> Task ParallelControl
showDescription state = forever (
		showSharedInformation "Task description" [ShowView (GetShared view)] state Void
	>?*	[(Action "Start workflow", Sometimes \{modelValue=m=:({selectedWorkflow},_)} -> if (isJust selectedWorkflow) (Just (addWorkflow (fromJust selectedWorkflow))) Nothing)])
where			
	view {selectedWorkflow} = case selectedWorkflow of
		Nothing			= ""
		Just (_,descr)	= descr
		
	addWorkflow (wid,_) =
									get (workflowTask wid)
		>>=	\(WorkflowTask task) ->	get currentUser
		>>= \user ->				appendTask (DetachedTask {initManagerProperties & worker = user}, \_ -> task >>| return Continue) topLevelTasks

processTable :: !(TaskList ClientState) -> Task ParallelControl	
processTable taskList = updateSharedInformation "process table" [UpdateView (GetLocalAndShared mkTable, Putback putback)] (processes |+< state) Nothing >>+ noActions
where
	state = taskListState taskList
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapSharedRead (\(procs,ownPid) -> filter (\{processId} -> processId <> ownPid) procs) (processesForCurrentUser |+| currentProcessId)
	
	mkTable mbSel (procs,_) = Table ["Title", "Priority", "Date", "Deadline"] (map mkRow procs) mbSel
	mkRow {Process|properties=p=:{taskProperties,managerProperties,systemProperties},processId} =
		[ html taskProperties.TaskMeta.title
		, formatPriority managerProperties.ManagerProperties.priority
		, visualizeAsHtml AsDisplay (timestampToGmDateTime systemProperties.issuedAt)
		, visualizeAsHtml AsDisplay managerProperties.ManagerProperties.deadline
		, Text (toString processId)
		]
		
	putback (Table _ cells mbSel) _ (_,state) = (Just mbSel,Just {state & selectedProcess = fmap (getProcId cells) mbSel})
	getProcId cells idx = case cells !! idx !! 4 of
		Text procId	= WorkflowProcess (toInt procId)
		_ = abort "getProcId"

workTabPanel :: !(TaskList ClientState) -> Task ParallelControl
workTabPanel taskList = parallel "Work tab panel" [] (\_ _ -> Continue) [(HiddenTask, controlWorkTabs (taskListState taskList)),(BodyTask, homeTab)]
where
	homeTab _ = showInformation "Home" [] (HtmlInclude "skins/default/welcome.html") <<@ fullShowInteractionLayout >>+ noActions 
	
controlWorkTabs :: !(Shared ClientState) !(TaskList [ProcessId]) -> Task ParallelControl
controlWorkTabs state taskList = forever (
					chooseActionDyn openTabTrigger (state >+< openProcs)
	>>= \proc ->	appendTask (BodyTask, \_ -> workTab proc openProcs <<@ singleControlLayout) taskList
	>>|				update (\state -> {state & selectedProcess = Nothing}) state
	>>|				update (\procs -> [proc:procs]) openProcs)
where
	openProcs = taskListState taskList
	
	openTabTrigger ({selectedProcess},procs) = case selectedProcess of
		Just selectedProcess | not (isMember selectedProcess procs)
			= StopInteraction selectedProcess
		_
			= UserActions []

workTab :: !ProcessId !(Shared [ProcessId]) -> Task ParallelControl											
workTab procId openProcs =
		update (\procs -> [procId:procs]) openProcs
	>>|	(workOn procId >>+ \{modelValue} -> if (modelValue =!= WOActive) (StopInteraction Void) (UserActions [(ActionClose, Just Void)]))
	>>|	update (filter ((<>) procId)) openProcs
	>>|	return Continue

controlClient :: Task ParallelControl										
controlClient = chooseAction [(ActionQuit, Stop)]

:: ClientState =
	{ selectedProcess	:: !Maybe ProcessId
	, selectedWorkflow	:: !Maybe (!WorkflowId, !String)
	}

derive class iTask ClientState
derive bimap Maybe, (,)

// Layouts

mainLayout {TUIParallel | items=i=:[(_,Just infoBar, logoutAction), (_,Just tree,_), (_,Just description,_), (_,Just workTabPanel,_), (_,Just processTable,_), (_,_,controlActions)]} =
	({ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [left,right] & orientation = Horizontal}
	, width		= FillParent 1 (FixedMinSize 0)
	, height	= FillParent 1 (FixedMinSize 0)
	, margins	= Nothing
	},controlActions ++ logoutAction)
where
	left =	{ content	= TUILayoutContainer (defaultLayoutContainer [tree,description])
			, width		= Fixed 260
			, height	= FillParent 1 (FixedMinSize 0)
			, margins	= Nothing
			}
	right = { content	= TUILayoutContainer (defaultLayoutContainer [infoBar,processTable,workTabPanel])
			, width		= FillParent 1 (FixedMinSize 0)
			, height	= FillParent 1 (FixedMinSize 0)
			, margins	= Nothing
			}

infoBarLayout :: TUIInteraction -> (TUIDef,[TaskAction])
infoBarLayout {title,editorParts,actions=actions=:[(ltask,laction,_)]} = (
	{ content	= TUILayoutContainer {defaultLayoutContainer [{hd editorParts & width = WrapContent 0, margins = Nothing},{logoutButton & margins = Nothing}]
									 &orientation = Horizontal, hGravity = HGRight, vGravity = VGCenter, baseCls = Nothing}
	, width		= FillParent 1 (ContentSize)
	, height	= Fixed 30
	, margins	= Nothing
	}, [])
where
	logoutButton =
		{content = TUIButton { TUIButton | name = actionName laction, taskId = ltask, disabled = False
							 , text = actionName laction, iconCls = "icon-logout", actionButton = True }
		, width = WrapContent 0, height = WrapContent 0, margins = Nothing }

treeLayout {title,editorParts,actions} = (	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [{hd editorParts & width = FillParent 1 ContentSize, height = FillParent 1 ContentSize}] & title = Just title, iconCls = Just "icon-newwork"}
											, width		= FillParent 1 (FixedMinSize 100)
											, height	= FillParent 1 (FixedMinSize 0)
											, margins	= Nothing
											}, actions)

descriptionLayout {title,editorParts,actions} = (	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer (defaultContent editorParts (fst (defaultButtons actions))) & title = Just title, iconCls = Just "icon-description"}
													, width		= FillParent 1 (FixedMinSize 100)
													, height	= Fixed 150
													, margins	= Nothing
													}, actions)

processTableLayout interaction	= ({hd interaction.editorParts & width = FillParent 1 ContentSize, height = Fixed 200, margins = (Just (sameMargins 0))},interaction.TUIInteraction.actions)	 
singleControlLayout interaction	= ({hd interaction.editorParts & width = FillParent 1 ContentSize, height = FillParent 1 ContentSize},interaction.TUIInteraction.actions)