implementation module textEditor

import iTasks, StdMisc, Text

textEditor :: [Workflow]
textEditor = [workflow "Examples/Miscellaneous/Text Editor" "A simple text editor, demonstrating the 'application'-like capabilities of iTasks" (textEditorApp <<@ Title "Text Editor")]

// global workflows
textEditorApp :: Task Void
textEditorApp = mdiApplication (AppState 0 []) groupActions actionGenFunc menuGenFunc
where
	groupActions = [(ActionNew, Always), (ActionOpen, Always), (Action OpenFile "", Always), (ActionAbout, Always), (ActionQuit, Always), (Action FocusWindow "", Always)]
	actionGenFunc ref mdiTasks=:{createEditor, iterateEditors} action
		# appStateRef = mapShared (fst,\app (_,editors) -> (app,editors)) ref
		= case action of
			ActionNew				= GExtend [newFile appStateRef createEditor]
			ActionOpen				= GExtend [openDialog appStateRef mdiTasks]
			//Action OpenFile _		= GExtend [open (DBRef (toInt data)) mdiTasks Nothing]
			ActionAbout				= GExtend [about]
			ActionQuit				= GExtend [quit iterateEditors]
			//Action FocusWindow _	= GFocus (Tag data)
		
	hotkey :: !Key -> Maybe Hotkey
	hotkey key = Just {ctrl = True, alt = False, shift = True, key = key}
	
	menuGenFunc :: (AppState,EditorCollection EditorState) -> MenuDefinition
	menuGenFunc (AppState _ recOpenedFiles,editorCollection) =
		[ Menu "File"	[ MenuItem ActionNew		(hotkey N)
						, MenuItem ActionOpen		(hotkey O)
						, SubMenu "Recently Opened"	recentlyOpenedMenu
						, MenuSeparator
						, MenuItem ActionSave		(hotkey S)
						, MenuItem ActionSaveAs		(hotkey A)
						, MenuSeparator
						, MenuItem ActionClose		(hotkey C)
						, MenuItem ActionQuit		(hotkey Q)
						]
		, Menu "Edit"	[ MenuItem Replace			(hotkey R)
						]
		, Menu "Tools"	[ MenuItem Stats			(hotkey T)
						]
		, Menu "Help"	[ MenuItem ActionAbout		Nothing
						]
		, Menu "Window"	windowMenu
		]
	where
		recentlyOpenedMenu = [MenuItem (OpenFile, toString name) Nothing \\ (DBRef id, name) <- recOpenedFiles]
		windowMenu = [MenuItem (FocusWindow, titleListener est) Nothing \\ (eid,est) <- toList editorCollection]
			
Replace			:== "replace"
Stats			:== "stats"
RecOpenedMenu	:== "recOpened"
OpenFile		:== "open-file"
FocusWindow		:== "focus-window"

newFile :: !AppStateRef !(MDICreateEditor EditorState) -> Task GAction
newFile aid createEditor =
								updateShared (\(AppState num recOpened) -> AppState (inc num) recOpened) aid
	>>= \(AppState newNum _).	createEditor (EditorState (Note "") (NewFile newNum)) textEditorFile
	>>|							return GContinue

openDialog :: !AppStateRef !(MDITasks EditorState a) -> Task GAction
openDialog gid mdiTasks =
				getAllFileNames
	>>= \files.	if (isEmpty files)
					(showMessage ("Open File","No files to open!") GContinue)
					(				enterChoiceA ("Open file","Open File") id buttons files
						>>= \res.	case res of
										(ActionOk,Just (_, Hidden fid)) =
											open fid mdiTasks (Just gid)
										_ =
											continue
					)
where
	buttons = [(ActionCancel, always), (ActionOk, ifvalid)]
					
open :: !(DBRef TextFile) !(MDITasks EditorState a) !(Maybe AppStateRef) -> Task GAction
open fid {createEditor, existsEditor} mbGid =
				existsEditor isEditingOpenendFile
	>>= \mbEid.	case mbEid of
		Nothing =
						getFile fid
			>>= \file.	case mbGid of // determine if to add to list of recently opened files
							Nothing = stop
							Just gid =
									updateShared (\(AppState n files) -> AppState n (take 5 [(fid, file.TextFile.name):files])) gid
								>>| stop
			>>|			return (GExtend [editor file])
		Just eid = return (GFocus (Tag eid))
where
	isEditingOpenendFile :: !EditorState -> Bool
	isEditingOpenendFile (EditorState _ file) =	case file of
		NewFile _		= False
		OpenedFile file	= (fid == file.fileId)
	
	editor :: !TextFile -> Task GAction					
	editor file = createEditor (EditorState file.TextFile.content (OpenedFile file)) textEditorFile >>| continue

about :: Task GAction
about = showMessageA ("About","iTextEditor January 2011") [(ActionOk,always)] GContinue >>= transform snd

quit :: !(MDIIterateEditors EditorState Bool) -> Task GAction	
quit iterateEditors =
					iterateEditors False checkForUnsavedData
	>>= \cancel.	if cancel continue stopGroup
where
	checkForUnsavedData :: !Bool !EditorStateRef -> Task Bool
	checkForUnsavedData True editor		= return True
	checkForUnsavedData False editor	= requestClosingFile editor
	
// editor workflows
textEditorFile :: !(EditorId EditorState) !EditorStateRef -> Task Void
textEditorFile eid ref = dynamicGroupA [editorWindow eid ref] actions actionsGenFunc
where
	actions =	[ (ActionSave, SharedPredicate ref noNewFile), (ActionSaveAs, Always)
				, (Action Replace "Replace", SharedPredicate ref contNotEmpty)
				, (Action Stats "Statistics", Always), (ActionClose, Always)
				]
	actionsGenFunc action = case action of
		ActionSave			= GExtend [save ref]
		ActionSaveAs		= GExtend [saveAs ref]
		Action Replace _	= GExtend [replaceT ref]
		Action Stats _		= GExtend [statistics ref]
		ActionClose			= GExtend [close ref]
	
	noNewFile :: !EditorState -> Bool					
	noNewFile (EditorState _ file) = case file of
		(OpenedFile _)	= True
		_				= False
	
	contNotEmpty :: !EditorState -> Bool
	contNotEmpty (EditorState (Note cont) _) = cont <> ""
	
editorWindow :: !(EditorId EditorState) !EditorStateRef -> Task GAction
editorWindow eid ref =
		(Tag eid @>>
		updateSharedInformationA ("Text Editor","You can edit the text.") mainEditor [] ref)
	>>|	continue
where		
	mainEditor =	( \st=:(EditorState cont _)			-> (Display (titleListener st),cont)
					, \(_,newCont) (EditorState _ file)	-> EditorState newCont file
					)
						
save :: !EditorStateRef -> Task GAction
save eid =
					readShared eid
	>>= \editor.	case editor of
						EditorState txt (OpenedFile file) =
										setFileContent txt file
							>>= \file.	writeShared eid (EditorState file.TextFile.content (OpenedFile file))
							>>|			continue
						_ = saveAs eid

saveAs :: !EditorStateRef -> Task GAction
saveAs eid =
				enterInformationA ("Save as","Save As: enter name") id buttons <<@ NoMenus
	>>= \res.	case res of
					(ActionOk,Just name) =
														readShared eid
						>>= \(EditorState txt _).		storeFile name txt
						>>= \file=:{TextFile|content}.	writeShared eid (EditorState content (OpenedFile file))
						>>|								continue
					_ =
						continue
where
	buttons = [(ActionCancel, always),(ActionOk, ifvalid)]
							
:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}

replaceT :: !EditorStateRef -> Task GAction
replaceT eid = replaceT` {searchFor = "", replaceWith = ""}
where
	replaceT` :: !Replace -> Task GAction
	replaceT` repl =
					updateInformationA ("Replace","Replace") idView buttons repl <<@ NoMenus
		>>= \res.	case res of
						(ActionReplaceAll,Just repl) =
								updateShared (dbReplaceFunc repl) eid
							>>|	replaceT` repl
						_ =
							continue
									
	buttons = [(ActionClose, always), (ActionReplaceAll, ifvalid)]
	
	dbReplaceFunc repl (EditorState (Note txt) file) = EditorState (Note (replaceSubString repl.searchFor repl.replaceWith txt)) file
								
ActionReplaceAll :== Action "replace-all" "Replace all"
												
:: TextStatistics =	{ lines			:: Int
					, words			:: Int
					, characters	:: Int
					}

statistics :: !EditorStateRef  -> Task GAction
statistics eid = 
		showMessageSharedA ("Statistics","Statistics of your document") statsListener [(ActionOk, always)] eid <<@ NoMenus
	>>|	continue
where
	statsListener (EditorState (Note text) _) =
			let txt = trim text
			in
				{lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}
					
titleListener :: EditorState -> String					
titleListener st=:(EditorState _ file) =
	if (hasUnsavedData st) "*" "" 
	+++
	getFileName file
								
close :: !EditorStateRef -> Task GAction
close eid =
					requestClosingFile eid
	>>= \cancel.	if cancel continue stopGroup
	
// helper functions
continue :: Task GAction
continue = return GContinue

stopGroup :: Task GAction
stopGroup = return GStop

hasUnsavedData :: !EditorState -> Bool
hasUnsavedData (EditorState continue file) = case file of
	NewFile _		= True
	OpenedFile file	= continue <> file.TextFile.content
	
requestClosingFile :: !EditorStateRef -> Task Bool
requestClosingFile eid =
										readShared eid
	>>= \state=:(EditorState _ file).	if (hasUnsavedData state)
										(					showMessageAboutA ("Save changes","Save changes?") id buttons (question file) <<@ NoMenus
											>>= \(action,_). case action of
																ActionCancel	= return True
																ActionNo		= return False
																ActionYes		= save eid >>| return False
										)
										(return False)
where
	buttons = [(ActionCancel, always), (ActionNo, always), (ActionYes, always)]
	question file = "Save changes to '" +++ getFileName file +++ "'?"
	
// global application state
:: AppState = AppState !Int ![(!(DBRef TextFile), !String)]
:: AppStateRef :== SymmetricShared AppState
:: EditorState = EditorState !Note !EditorFile
:: EditorFile = NewFile !Int | OpenedFile !TextFile
:: EditorStateRef :== SymmetricShared EditorState

// text files database
:: FileName :==	String
:: TextFile =	{ fileId	:: !(DBRef TextFile)
				, name		:: !FileName
				, content	:: !Note
				}

instance DB TextFile where
	databaseId			= sharedStore "TextFiles"
	getItemId file		= file.fileId
	setItemId id file	= {file & fileId = id}
	
storeFile :: !FileName !Note -> Task TextFile
storeFile name txt =
				getDefaultValue
	>>= \file.	dbCreateItem {TextFile| file & name = name, content = txt}
	>>= \file.	return file
	
setFileContent :: !Note !TextFile -> Task TextFile
setFileContent continue file = dbUpdateItem {TextFile | file & content = continue}
	
getFile :: !(DBRef TextFile) -> Task TextFile
getFile id =
				dbReadItem id
	>>= \res.	case res of
					Nothing		= undef
					(Just file)	= return file
					
getFileName :: !EditorFile -> String
getFileName file = case file of
	NewFile idx		= "New Text Document " +++ toString idx
	OpenedFile file	= file.TextFile.name

getAllFileNames :: Task [(FileName, Hidden (DBRef TextFile))]
getAllFileNames =
				dbReadAll
	>>= \files.	return (map (\f -> (f.TextFile.name, Hidden f.fileId)) files)
	
derive class iTask AppState, EditorState, EditorFile, TextFile, TextStatistics, Replace
derive bimap Maybe, (,)