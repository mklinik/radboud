implementation module ideExample
 
Start :: *World -> *World
Start world = startEngine ideExample world

import iTasks
import CommonDomain
				
:: MenuOptions	= New
				| Delete	
				| Open
				| Save
				| Cancel	
				| Exit

:: FileSystem	= 	{ fileName		:: String
					, fileContent 	:: String
					, fileOpen		:: Bool
					, fileDbref		:: DBRef FileSystem
					}

derive gPrint 		MenuOptions, FileSystem
derive gParse 		MenuOptions, FileSystem
derive gVisualize 	MenuOptions, FileSystem
derive gUpdate 		MenuOptions, FileSystem

ideExample :: [Workflow]
ideExample
= [	{ Workflow 
	| name		= "Examples/Miscellaneous/ideExample"
	, label		= "simple IDE"
	, roles		= []
	, mainTask	= ide >>| return Void
	}
  ]

ide :: Task Bool
ide
	=					getCurrentUser 
		>>= \user ->	(handleMenu user "Main menu..." [New, Open, Delete, Exit]  
						 -||- 
						 cancel user
						) <! id
	
	
handleMenu user prompt choices
	=					enterChoice prompt choices
		>>=				dispatch user

cancel user = handleMenu user "Cancel..." [Cancel]

dispatch :: User MenuOptions -> Task Bool
dispatch user Cancel	= return False
dispatch user Exit		= exit user		>>| return True
dispatch user New		= newFile user	>>| return False
dispatch user Open		= openFile user	>>| return False
dispatch _ _			= return False

exit :: User -> Task Void
exit user
	=					getProcessesForUser user.userId [Active, Suspended, Finished]
		>>= \procs ->	deleteAll [process.processId \\ process <- procs]
where
	deleteAll []		= return Void
	deleteAll [p:ps] 	= deleteProcess p >>| deleteAll ps

newFile :: User -> Task Void
newFile user
	=						readAllFiles
		>>= \files ->		let allFileNames = [file.fileName \\ file <- files] in
							enterInformationAbout (show allFileNames) "Enter file name:"
		>>= \name ->		if (isMember name allFileNames) 
								(showMessageAbout "Error. The file already existed:" name) 
								(create name)
where
	create name
		=					dbCreateItem
			>>= \item ->	dbUpdateItem {item & fileName = name, fileContent = ""}
			>>= \file ->	spawnProcess user.userId True (editor user file <<@ "Editor for " +++ name)
			>>|				return Void

	show [] 	= [] 
	show names 	= [Text "Already created files:", BrTag [], BrTag []] ++ (flatten [[Text name, BrTag []] \\ name <- names])

openFile :: User -> Task Void
openFile user
	=						readAllFiles
		>>= \files ->		let notInUse = [file \\ file <- files | not file.fileOpen] in
								if (isEmpty files) 
								(showMessage "Error. No files created yet.") 
								(if (isEmpty notInUse)
									(showMessage "Error. All files in use.")
									(chooseAFile notInUse)
								)
where
	chooseAFile files
	 =						enterChoice "Choose file to open..." (map (\file -> file.fileName) files)
		>>= \name ->		return (hd [file \\ file <- files | file.fileName == name])
		>>= \file ->		spawnProcess user.userId True (editor user file <<@ "Editor for " +++ name)
		>>|					return Void

editor user file
	=					(forever edit -||- handleMenu user "Main menu..." [Save, Exit] -||- cancel user) <! id					
where
	edit 
		= 							dbReadItem (getItemId file)
			>>= \mbFile ->			if (isNothing mbFile)
										(return True)
										(readAndEdit (fromJust mbFile) >>| return False)
										
readAndEdit file
	=							dbUpdateItem {file & fileOpen = True}
		>>= \file ->			updateInformation ("Edit file \"" +++ file.fileName +++ "\"") (Note file.fileContent)
		>>= \(Note content) ->	dbUpdateItem {file & fileContent = content, fileOpen = False}

showFileNames :: [String] -> [HtmlTag]
showFileNames [] = [Text "No files created yet...", BrTag []]
showFileNames names = [Text "Existing file names:", BrTag [], BrTag []] ++ flatten [[Text name, BrTag []] \\ name <- names]

readAllFiles :: Task [FileSystem]
readAllFiles = dbReadAll

// file system

instance DB FileSystem
where	
	databaseId 			= mkDBid "FileSystem"
	getItemId fs		= fs.fileDbref
	setItemId dbref val = {val & fileDbref = dbref}
	

