implementation module BugReport

import iTasks

:: BugReport =
	{ application	:: String
	, version		:: Maybe String
	, yourName		:: String
	, date			:: Date
	, occursAt		:: BugOccurance
	, severity		:: BugSeverity
	, description	:: Note
	, attachment	:: Maybe [Document]
	}

:: BugSeverity	= Low | Medium | High | Critical	
:: BugOccurance	= Startup | Shutdown | Other Note

:: Bug =
	{ bugNr			:: BugNr
	, status		:: BugStatus
	, reportedAt	:: (Date,Time)
	, reportedBy	:: User
	, report		:: BugReport
	, analysis		:: Maybe BugAnalysis
	}

:: BugStatus = Reported | Assigned User | Repaired

:: BugAnalysis =
	{ cause				:: Note
	, affectedVersions	:: [String]
	}

:: BugNr :== Int

derive class iTask	BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis
	
instance DB Bug where
	databaseId					= sharedStore "Bug" []
	getItemId bug=:{bugNr}		= DBRef bugNr
	setItemId (DBRef bugNr) bug	= {bug & bugNr = bugNr}

bugReportExample :: [Workflow]
bugReportExample
	= [ workflow "Examples/Miscellaneous/Bug report (simple)" "A simple bug submission form" reportBugVerySimple
	  , workflow "Examples/Miscellaneous/Bug report (simple 2)" "A more complex bug submission form" reportBugSimple
	  , workflow "Examples/Miscellaneous/Bug report (simple 3)" "A more complex bug submission form" bugReport
	  , workflow "Examples/Miscellaneous/Bug report (advanced)" "A workflow for reporting and handling bugs" reportBug
	  ]
	 
reportBugVerySimple :: Task Note
reportBugVerySimple
	=	enterInformation ("Describe bug","Please describe the bug you have found") []
	>>=	\report ->
		"alice" @:
			(Title "Bug Report" @>> viewInformation ("Fix bug","The following bug has been reported, please fix it.") [] report)

reportBugSimple :: Task BugReport
reportBugSimple
	=	enterInformation ("Describe bug","Please describe the bug you have found") []
	>>=	\report ->
		"alice" @:
			(Title "Bug Report" @>> viewInformation ("Fix bug","The following bug has been reported, please fix it.") [] report)

//Different variant of simple reportBug
bugReport :: Task BugReport
bugReport = reportBug >>= fixBug
where
	reportBug :: Task BugReport
	reportBug = enterInformation ("Describe bug","Please describe the bug you found") []
	
	fixBug :: BugReport -> Task BugReport
	fixBug bug = "alice" @: (Title "Bug Report" @>> viewInformation ("Fix bug","The following bug has been reported, please fix it.") [] bug)

//Main workflow	  
reportBug :: Task Bug
reportBug
	=	enterBugReport
	>>= \report ->
		fileBug report
	>>= \bug ->
		case report.severity of
			Critical 
				= (confirmCritical bug.report
					>>= \critical ->
					assignBug bug critical)
			_
				=	assignBug bug False

assignBug :: Bug Bool -> Task Bug
assignBug bug critical
	=	selectDeveloper bug.report.BugReport.application
	>>=	\developer ->
		updateBug (\b -> {Bug| b & status = Assigned developer}) bug
	>>= \bug ->
		assign {noMeta & worker = toUserConstraint developer, priority = priority} (Title subject @>> resolveBug bug critical)
where
	priority = if critical HighPriority NormalPriority
	subject  = if critical "Critical bug!" "Bug"

resolveBug :: Bug Bool -> Task Bug
resolveBug bug critical
	=	analyzeBug bug
	>>= \bug ->
		developBugFix bug
	>>| if critical
		( makePatches bug -&&- mergeFixInMainLine bug
		  >>| wrapUp bug)
		( mergeFixInMainLine bug
		  >>| wrapUp bug)

wrapUp :: Bug -> Task Bug
wrapUp bug
	=	updateBug (\b -> {Bug| b & status = Repaired}) bug
	>>= \bug ->
		notifyReporter bug

//Sub tasks

enterBugReport :: Task BugReport
enterBugReport
	=	enterInformation ("Describe bug","Please describe the bug you have found") []
	
fileBug :: BugReport -> Task Bug
fileBug report
	=	 get currentUser
	>>= \user ->
		dbCreateItem {defaultValue & report = report, reportedBy = user}

updateBug :: (Bug -> Bug) Bug -> Task Bug
updateBug f bug = dbUpdateItem (f bug)

confirmCritical :: BugReport -> Task Bool
confirmCritical report
	=	selectDeveloper report.BugReport.application
	>>= \assessor ->
		assign {noMeta & worker =toUserConstraint assessor, priority = HighPriority}
			( Title "Bug report assessment" @>>
			  viewInformation ("Confirmation","Is this bug really critical?") [] report >>* [AnyTime ActionNo (const (return False)),AnyTime ActionYes (const (return True))]
			)

selectDeveloper :: String -> Task User
selectDeveloper application
	=	findAppDevelopers application
	>>= \developers -> case developers of
		[]	= get currentUser
		_	= selectLeastBusy developers
where
	findAppDevelopers :: String -> Task [User]
	findAppDevelopers _			= return []
		
	selectLeastBusy :: [User] -> Task User
	selectLeastBusy []
		=	get currentUser
	selectLeastBusy names
		= 	allTasks [getNumTasksForUser name \\ name <- names]
		>>= \activity -> 
			return (snd (minimum (zip (activity,names))))
	where	
		minimum l = foldl min (hd l) (tl l) 
		
	getNumTasksForUser :: User -> Task Int
	getNumTasksForUser name = return 42			//TODO: Use API function
	 
analyzeBug :: Bug -> Task Bug
analyzeBug bug
	=	determineCause bug 
	>>=	\cause ->
		dbUpdateItem {bug & analysis = Just {cause = cause, affectedVersions = []}}
where
	determineCause bug
		= enterInformation ("Cause","What is the cause of the following bug?") [] -|| viewInformation Void [] bug
		
developBugFix :: Bug -> Task Bug
developBugFix bug = viewInformation ("Bug fix","Please implement a fix for the following bug:") [] bug

mergeFixInMainLine :: Bug -> Task Bug
mergeFixInMainLine bug = viewInformation ("Merge","Please merge the bugfix in the main line of version control") [] bug

makePatches :: Bug -> Task Void
makePatches bug =
	case bug.analysis of
		Nothing
			= return Void
		Just {affectedVersions = []}
			= return Void
		Just {affectedVersions = versions}
			= allTasks	[
						viewInformation ("Patch" ,"Please make a patch of bugfix " <+++ bug.bugNr <+++ " for the following version of " <+++ bug.Bug.report.BugReport.application)
								[] version
						\\ version <- versions
					   ]
			>>| return Void
		
notifyReporter :: Bug -> Task Bug
notifyReporter bug = bug.reportedBy @: (viewInformation ("Bug Report Result","The bug you reported has been fixed") [] bug)

//notifyUser "The bug you reported has been fixed" bug.reportedBy
