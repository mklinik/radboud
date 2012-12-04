implementation module WorkflowAdmin

import iTasks
import StdMisc, Tuple, Text, Shared
from StdFunc import seq
from Util import mb2list, kvGet
from Map import qualified newMap

// SPECIALIZATIONS
derive gVisualizeText	Workflow
derive gVisualizeEditor	Workflow
derive gHeaders			Workflow
derive gGridRows		Workflow
derive gUpdate 			Workflow
derive gVerify			Workflow
derive JSONEncode		Workflow
derive JSONDecode		Workflow
derive gEq				Workflow

gVisualizeText{|WorkflowTaskContainer|} _ _	= []
gVisualizeEditor{|WorkflowTaskContainer|} _ vst = noVisualization vst
gHeaders{|WorkflowTaskContainer|} _ = ["Workflow task container"]
gGridRows{|WorkflowTaskContainer|} _ _ = Nothing
gUpdate{|WorkflowTaskContainer|} mode ust = basicUpdate mode (\Void x -> x) (WorkflowTask defTask) ust
where
	defTask :: Task Void
	defTask = abort "default task container"

gVerify{|WorkflowTaskContainer|} _ vst = alwaysValid vst

JSONEncode{|WorkflowTaskContainer|} c		= [dynamicJSONEncode c]
JSONDecode{|WorkflowTaskContainer|} [c:r]	= (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} r		= (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _			= True

// SHARES
// Available workflows

workflows :: Shared [Workflow]
workflows = sharedStore "Workflows" []

workflowByPath :: !String -> Shared Workflow
workflowByPath path = mapReadWriteError (toPrj,fromPrj) workflows
where
	toPrj wfs = case [wf \\ wf <- wfs | wf.Workflow.path == path] of
		[wf:_]	= Ok wf
		_		= Error ("Workflow " +++ path +++ " could not be found")

	fromPrj nwf wfs
		= Ok (Just [if (wf.path == path) nwf wf \\ wf <- wfs])

allowedWorkflows :: ReadOnlyShared [Workflow]
allowedWorkflows = mapRead filterAllowed (workflows |+| currentUser)
where
	filterAllowed (workflows,user) = filter (isAllowedWorkflow user) workflows
	
workflowTree :: ReadOnlyShared (Tree (Either WorkflowFolderLabel Workflow))
workflowTree = mapRead mkFlowTree (toReadOnly workflows)

allowedWorkflowTree :: ReadOnlyShared (Tree (Either WorkflowFolderLabel Workflow))
allowedWorkflowTree = mapRead mkFlowTree allowedWorkflows

mkFlowTree :: ![Workflow] -> Tree (Either WorkflowFolderLabel Workflow)
mkFlowTree workflows = Tree (seq [insertWorkflow w \\ w <- workflows] [])
where
	insertWorkflow wf=:{Workflow|path} nodeList = insertWorkflow` (split "/" path) nodeList
	where
		insertWorkflow` [] nodeList = nodeList
		insertWorkflow` [title] nodeList = nodeList ++ [Leaf (Right wf)]
		insertWorkflow` path=:[nodeP:pathR] [node=:(Node (Left nodeL) nodes):nodesR]
			| nodeP == nodeL	= [Node (Left nodeL) (insertWorkflow` pathR nodes):nodesR]
			| otherwise			= [node:insertWorkflow` path nodesR]
		insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
		insertWorkflow` [nodeP:pathR] [] = [Node (Left nodeP) (insertWorkflow` pathR [])]

// SERVICE TASKS
viewTaskList :: Task [TaskListItem Void]
viewTaskList 
	=	doAuthenticated (viewSharedInformation "Tasks" [] processesForCurrentUser)

viewTask :: Task WorkOnStatus
viewTask
	=	doAuthenticated (
			enterInformation "Enter task identification" []
		>>= workOn 
		)
		
externalTaskInterface :: [PublishedTask]
externalTaskInterface
	= [publish "/external/tasklist" WebApp (\_ -> viewTaskList)
	  ,publish "/external/task" WebApp (\_ -> viewTask)
	  ]

// MANAGEMENT TASKS
manageWorkflows :: ![Workflow] ->  Task Void
manageWorkflows iflows
	=	installInitialWorkflows iflows
	>>| forever (catchAll (doAuthenticated workflowDashboard) viewError)
where
	viewError error = viewInformation "Error" [] error >>! \_ -> return Void

manageWorklist :: ![Workflow] -> Task Void
manageWorklist iflows
	=	installInitialWorkflows iflows
	>>| workflowDashboard

installInitialWorkflows ::[Workflow] -> Task Void
installInitialWorkflows [] = return Void
installInitialWorkflows iflows
	= get workflows
	>>= \flows -> case flows of
		[]	= addWorkflows iflows >>| return Void
		_	= return Void

// Application specific types
:: ClientPart
	= Logout								//Produced by the control task
	| SelWorkflow 	!String					//Produced by the workflow chooser & workflow details
	| SelProcess 	!TaskId					//Produced by the worklist
	| OpenProcess			
		
:: WorklistRow =
	{ title		:: Maybe String
	, priority	:: TaskPriority
	, date		:: DateTime
	, deadline	:: Maybe DateTime
	}

derive class iTask ClientPart, WorklistRow
	
workflowDashboard :: Task Void
workflowDashboard
	=  parallel Void
		[ (Embedded, startWork)
		, (Embedded, controlDashboard)
		, (Embedded, manageWork)
		] <<@ SetLayout layout 
	>>* [WhenValid (\results -> isValue (snd (results !! 1))) (\_ -> return Void)]
where
	isValue (Value _ _) = True
	isValue _			= False
	
	layout = customMergeLayout (sideMerge LeftSide 260 (sideMerge TopSide 30 (sideMerge TopSide 200 tabbedMerge)))

controlDashboard :: !(SharedTaskList ClientPart) -> Task ClientPart
controlDashboard list
	=	(viewSharedInformation Void [ViewWith view] currentUser	
			>>* [AnyTime ActionRefresh		(\_ -> return Nothing)
				,AnyTime (Action "Log out")	(\_ -> return (Just Logout))
				]															
		) <! isJust	<<@ AfterLayout (uiDefSetDirection Horizontal)
	@	fromJust	
where
	view user	= "Welcome " +++ toString user		

startWork :: !(SharedTaskList ClientPart) -> Task ClientPart
startWork list
	= (chooseWorkflow >&> viewAndStart) <<@ SetLayout (sideLayout BottomSide 200 sequenceMerge)
where
	viewAndStart sel = forever (
			viewWorkflowDetails sel
		>>* [WithResult (Action "Start Task") (const True) (startWorkflow list)]
		@	\wf -> SelWorkflow wf.Workflow.path
		)

chooseWorkflow :: Task Workflow
chooseWorkflow
	=	enterSharedChoice [Att (Title "Tasks"), Att IconEdit] [ChooseWith ChooseFromTree toView] (allowedWorkflowTree) <<@ AfterLayout (tweakControls (map noAnnotation))
	@? onlyRight
where
	toView (Left label)				= label
	toView (Right wf)				= workflowTitle wf
	
	onlyRight (Value (Right wf) s)	= Value wf s
	onlyRight _						= NoValue

	noAnnotation (c,_) = (c,'Map'.newMap)
	
viewWorkflowDetails :: !(ReadOnlyShared (Maybe Workflow)) -> Task Workflow
viewWorkflowDetails sel
	= viewSharedInformation [Att (Title "Task description"), Att IconView] [ViewWith view] sel
	@? onlyJust
where
	view = fmap (\wf -> Note wf.Workflow.description)
	
	onlyJust (Value (Just v) s) = Value v s
	onlyJust _					= NoValue

startWorkflow :: !(SharedTaskList ClientPart) !Workflow -> Task Workflow
startWorkflow list wf
	= 	get currentUser
	>>=	\user ->
		appendTopLevelTask {noMeta & worker = toUserConstraint user, title = Just (workflowTitle wf)} (fromContainer wf.Workflow.task)
	>>= \procId ->
		openTask list procId
	@	const wf
where
	fromContainer (WorkflowTask t) = t @ const Void
	fromContainer (ParamWorkflowTask tf) = (enterInformation "Enter parameters" [] >>= tf @ const Void)		

manageWork :: !(SharedTaskList ClientPart) -> Task ClientPart	
manageWork taskList = forever
	(	enterSharedChoice Void [ChooseWith ChooseFromGrid mkRow] processes 														
	>>* [WithResult (Action "Open") (const True) (\proc -> openTask taskList proc.TaskListItem.taskId @ const OpenProcess)
		,WithResult (Action "Delete") (const True) (\proc -> removeTask proc.TaskListItem.taskId topLevelTasks @ const OpenProcess)]
	)
where
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapRead (\(procs,ownPid) -> filter (show ownPid) (filter isActive procs)) (processesForCurrentUser |+| currentTopTask)
	where
		show ownPid {TaskListItem|taskId,progressMeta=Just pmeta,managementMeta=Just _} = taskId <> ownPid
		show ownPid _ = False
		
	isActive {progressMeta=Just {status=Unstable}}	= True
	isActive _										= False	

	mkRow {TaskListItem|progressMeta=Just pmeta,managementMeta=Just mmeta} =
		{WorklistRow
		|title = mmeta.ManagementMeta.title	
		,priority = mmeta.ManagementMeta.priority
		,date = pmeta.issuedAt
		,deadline = mmeta.completeBefore
		}
	
openTask :: !(SharedTaskList ClientPart) !TaskId -> Task ClientPart
openTask taskList taskId
	=	appendOnce taskId (workOnTask taskId) taskList @ const OpenProcess

workOnTask :: TaskId -> Task ClientPart
workOnTask taskId
	= (workOn taskId @ const OpenProcess) -||- chooseAction [(ActionClose,OpenProcess)]

appendOnce identity task taskList
	= 	appendTask Embedded (\_ -> task) taskList @ const Void

addWorkflows :: ![Workflow] -> Task [Workflow]
addWorkflows additional
	=	update (\flows -> flows ++ additional) workflows

// UTIL FUNCTIONS
workflow :: String String w -> Workflow | toWorkflow w
workflow path description task = toWorkflow path description [] task

restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedWorkflow path description roles task = toWorkflow path description roles task

inputWorkflow :: String String String (a -> Task b) -> Workflow | iTask a & iTask b
inputWorkflow name desc inputdesc tfun
	= workflow name desc (enterInformation inputdesc [] >>= tfun)  
	
instance toWorkflow (Task a) | iTask a
where
	toWorkflow path description roles task = toWorkflow path description roles (Workflow noMeta task)
	
instance toWorkflow (WorkflowContainer a) | iTask a
where
	toWorkflow path description roles (Workflow managerP task) = mkWorkflow path description roles (WorkflowTask task) managerP

instance toWorkflow (a -> Task b) | iTask a & iTask b
where
	toWorkflow path description roles paramTask = toWorkflow path description roles (ParamWorkflow noMeta paramTask)
	
instance toWorkflow (ParamWorkflowContainer a b) | iTask a & iTask b
where
	toWorkflow path description roles (ParamWorkflow managerP paramTask) = mkWorkflow path description roles (ParamWorkflowTask paramTask) managerP
	
mkWorkflow path description roles taskContainer managerProps =
	{ Workflow
	| path	= path
	, roles	= roles
	, task = taskContainer
	, description = description
	, managerProperties = managerProps
	}

workflowTitle :: Workflow -> String
workflowTitle {Workflow|path} = last (split "/" path)

isAllowedWorkflow :: !User !Workflow -> Bool
isAllowedWorkflow _ {Workflow|roles=[]}		= True								//Allow workflows without required roles
isAllowedWorkflow (AuthenticatedUser _ hasRoles _) {Workflow|roles=needsRoles}	//Allow workflows for which the user has permission
	= or [isMember r hasRoles \\ r <- needsRoles]
isAllowedWorkflow _ _ 						= False								//Don't allow workflows in other cases
