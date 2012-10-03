module textEdit

import iTasks
import directories

derive bimap (,), Maybe

root :== Directory "MyDirectory"

Start w = startEngine [ workflow "text editor1" "simple text editor" textEditor1 // bug: don't use the same name twice !!!
					  , workflow "text editor2" "advanced text editor" (textEditor2 "aap.txt")
					  , workflow "shell" "simple shell" shell
					  ] w

// ---------

textEditor1 
	= 					updateInformation "Edit the text:" (Note "")
		>>= 			showMessage "Resulting text is:"

// ---------

import Text

derive class iTask Replace, TextStatistics, EditorState

:: Replace			=	{ search 		:: String
						, replaceBy 	:: String
						}
:: TextStatistics 	=	{ lines			:: Int
						, words			:: Int
						, characters	:: Int
						}
:: EditorState		=	{ mytext		:: String
						, replace		:: Bool
						, statistics	:: Bool
						}

initEditorState text 	= 	{mytext = text, replace = False, statistics = False}
updateReplace b  		=  	update (\s ->{s & replace = b}) 
updateStat b 			=	update (\s -> {s & statistics = b}) 
updateText f 			=	update (\s -> {s & mytext = f s.mytext}) 

voidResult _ _ = Void 

onlyIf :: Bool a -> Maybe a
onlyIf b do
	| b 		= Just do
	| otherwise	= Nothing

normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

ActionReplace 		:== Action "Replace" "Replace"
ActionStatistics	:== Action "Statistics" "Statistics"

textEditor2 :: String -> Task Void
textEditor2 fileName
	=						getInitialPath root
		>>= \pwd ->			readTextFile pwd (TextFile fileName)
		>>= \(_,text) -> 	parallel "Editor" (initEditorState text) voidResult [taskKind (editor pwd fileName)]
where

	taskKind = InBodyTask
	
	taskKind2 = DetachedTask (normalTask  RootUser) myMenu // window does not work yet
	
	myMenu s =  [ Menu "File" 	[ MenuItem ActionSave (ctrl 's')
								, MenuSeparator
								, MenuItem ActionQuit (ctrl 'q')
								]
				, Menu "Edit" 	[ MenuItem "Replace"  (ctrl 'r')
								, MenuItem "Statistics" (ctrl 's')
								]				
				]
	where
		ctrl c = Just {key=c,ctrl=True,alt=False,shift=False}

	editor :: DirPath String (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	editor pwd fileName ls os 
		= 			updateSharedInformationA (fileName,"Edit text file " +++ fileName) (toView,fromView) ls
			>?* 	[ (ActionSave, 		IfValid	save)
			  		, (ActionQuit,		Always 	quit)
			  		, (ActionReplace,	Sometimes (\s -> onlyIf (not s.modelValue.replace)    replace))
			  		, (ActionStatistics,Sometimes (\s -> onlyIf (not s.modelValue.statistics) statistics))
			  		]
	where	
		toView state = Note state.mytext
		fromView (Note text) state = {state & mytext = text} 

		save val
			=		safeTextFile pwd (TextFile fileName) val.mytext
				>>|	editor pwd fileName ls os
		quit
			=		set os [StopParallel] 
				>>| return Void

		replace 
			=		updateReplace True ls
				>>| set os [AppendTask (InBodyTask (replaceTask {search = "", replaceBy = ""}))]
				>>| editor pwd fileName ls os

		statistics 
			=		updateStat True ls
				>>|	set os [AppendTask (InBodyTask statisticsTask)]
				>>| editor pwd fileName ls os

	replaceTask :: Replace (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	replaceTask replacement ls os
		=			updateInformationA ("Replace","Define replacement...") idView replacement
			>?*		[(ActionOk,   IfValid 	(\r -> 		updateText (replaceSubString r.search r.replaceBy) ls
								 					>>|	replaceTask r ls os))
					,(ActionQuit, Always 	(	updateReplace False ls 
												>>| return Void))
					]

	statisticsTask :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	statisticsTask ls os 
		= 			monitorA ("Statistics","Statistics of your document") toView ls
					(\_ -> UserActions [(ActionQuit, Just (updateStat False ls >>| return Void))]) >>= id
	where
		toView state=:{mytext} 
			=	{ lines 	 = length (split "\n" mytext)
				, words 	 = length (split " " (replaceSubString "\n" " " mytext))
				, characters = textSize mytext
				}

// ---------


ActionNewFile 		:== Action "New File" "New File"
ActionNewDirectory	:== Action "New Directory" "New Directory"
ActionUp			:== Action "Up" "Up"
ActionDelete		:== Action "Delete" "Delete"
ActionNewShell		:== Action "New Shell" "New Shell"
ActionRefresh		:== Action "Refresh" "Refresh"

shell :: Task Void
shell 
	=					get currentUser
			>>= \me ->	getInitialPath root 
			>>= \pwd -> parallel  ("Shell","pwd = " <+++ pwd)  Void voidResult [newShell me pwd]
where
	newShell me pwd
		= 				DetachedTask (normalTask me) noMenu (interpret me pwd)

interpret :: User DirPath (SymmetricShared Void) (ParallelInfo Void) -> Task Void
interpret me pwd ls os
	= 					shellCommand pwd
where
	shellCommand pwd
		=					readDir pwd
		>>= \(ok,names) -> 	updateInformationA ("Current directory: " <+++ pwd) idView (choice names)
		>?*		[ (ActionOpen, 		IfValid    (\val -> 	open pwd (getChoice val)))
				, (ActionUp, 		Sometimes  (\_ -> 		Just (getParent pwd >>= shellCommand)))
				, (ActionNewFile, 	Always (				updateInformation "Choose file name:" ""
											>>= \newName -> safeTextFile pwd (TextFile (newName +++ ".txt")) ""
											>>|				shellCommand pwd)) 
				, (ActionRefresh,	Always (				shellCommand pwd))
				, (ActionDelete,	IfValid (\val ->		delete pwd (getChoice val)
											>>| 			shellCommand pwd))
				,(ActionNewDirectory, Always (				updateInformation "Choose directory name:" ""
											>>= \dirName -> newDir pwd (Directory dirName)
											>>|				shellCommand pwd)) 
				,(ActionNewShell, 	Always (				set os [AppendTask (DetachedTask (normalTask me) noMenu (interpret me pwd))] 
											>>|				shellCommand pwd))
				]

	getChoice (Choice elem i) = elem!!i

	open pwd (TextFile name) 
		=				set os [AppendTask (DetachedTask (normalTask me) noMenu (\ls os -> textEditor2 name))] 
			>>|			shellCommand pwd
	open pwd dirname=:(Directory name) =	openDirectory pwd dirname >>= shellCommand
	
	delete pwd (TextFile name)
		=				deleteTextFile pwd (TextFile name)
	delete pwd (Directory name)
		= 				removeDir pwd (Directory name)


