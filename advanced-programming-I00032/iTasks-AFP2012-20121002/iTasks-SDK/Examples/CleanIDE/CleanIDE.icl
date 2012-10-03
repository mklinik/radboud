module CleanIDE

import iTasks, Text
from StdFunc import flip
import PmProject, UtilStrictLists
import CompilerInterface, AppState, Configuration, GUI, StdMisc
from Directory import pathToPD_String

Start world = startEngine	[ workflow "Clean IDE (using formatted text type)" "Clean IDE (using formatted text type)"	(cleanIDEFT <<@ Subject "Clean IDE (using formatted text type)")
							, workflow "Clean IDE (using source code type)"	"Clean IDE (using source code type)" (cleanIDESC <<@ Subject "Clean IDE (using source code type)")
							] world

cleanIDEFT :: Task Void
cleanIDEFT = try cleanIDE` handleErrors
where
	cleanIDE` =
						setMenus menuStructure
		>>|				loadConfig
		>>= \mbConfig.	case mbConfig of
							Just config =	
												createDB (initAppState config)
								>>= \sid.		isDirectory (config.projectsPath +< [PathDown "test"])
								>>= \prjExists.	if prjExists (return Void) (createTestPrj sid)
								>>|				openFile (config.projectsPath +< [PathDown "test", PathDown "test.icl"]) sid
								>>|				dynamicGroupAOnly [srcEditor sid] (actions sid)
								>>|				deleteDB sid
								>>|				stop
							Nothing = stop
							
	actions :: !(DBId AppState) -> [GroupAction GOnlyAction Void Void]
	actions sid =	[ GroupAction ActionQuit					GOStop GroupAlways
					, GroupAction ActionSave					(GOExtend [save sid]) GroupAlways
					, GroupAction ActionCompile					(GOExtend [saveAndCompile sid <<@ GBModal]) GroupAlways
					, GroupAction ActionFind					(GOExtend [findAndReplace sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditCodeGenOptions		(GOExtend [editProjectOptions	"Code Generation Options"		PR_GetCodeGenOptions		PR_SetCodeGenOptions					sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditAppOptions			(GOExtend [editProjectOptions	"Application Options"			PR_GetApplicationOptions	PR_SetApplicationOptions				sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditLinkOptions			(GOExtend [editProjectOptions	"Linker Options"				PR_GetLinkOptions			(flip PR_SetLinkOptions)				sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditLinkOptions			(GOExtend [editProjectOptions	"Linker Options"				PR_GetLinkOptions			(flip PR_SetLinkOptions)				sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditSyntaxColOptions	(GOExtend [editAppStateOptions	"Syntax Highlighter Colours"	(\s -> s.syntaxHighlColors)	(\c s -> {s & syntaxHighlColors = c})	sid <<@ GBAlwaysFloating]) GroupAlways
					]
					
	srcEditor :: !(DBId AppState) -> Task Void
	srcEditor sid =
			updateShared "Clean Source" "Clean Source" [] sid [srcEditorView]
		>>|	return Void
	where
		srcEditorView = editor	{ editorFrom	= \state		-> highlightSyntax state.srcEditorContent state.syntaxHighlColors
								, editorTo		= \ft state		-> {state & srcEditorContent = toUnformattedString ft True}
								}
					
cleanIDESC :: Task Void
cleanIDESC = try cleanIDE` handleErrors
where
	cleanIDE` =
						setMenus menuStructure
		>>|				loadConfig
		>>= \mbConfig.	case mbConfig of
							Just config =	
												createDB (initAppState config)
								>>= \sid.		isDirectory (config.projectsPath +< [PathDown "test"])
								>>= \prjExists.	if prjExists (return Void) (createTestPrj sid)
								>>|				openFile (config.projectsPath +< [PathDown "test", PathDown "test.icl"]) sid
								>>|				dynamicGroupAOnly [srcEditor sid] (actions sid)
								>>|				deleteDB sid
								>>|				stop
							Nothing = stop
							
	actions :: !(DBId AppState) -> [GroupAction GOnlyAction Void Void]
	actions sid =	[ GroupAction ActionQuit					GOStop GroupAlways
					, GroupAction ActionSave					(GOExtend [save sid]) GroupAlways
					, GroupAction ActionCompile					(GOExtend [saveAndCompile sid <<@ GBModal]) GroupAlways
					//, GroupAction ActionFind					(GOExtend [findAndReplace sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditCodeGenOptions		(GOExtend [editProjectOptions	"Code Generation Options"		PR_GetCodeGenOptions		PR_SetCodeGenOptions					sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditAppOptions			(GOExtend [editProjectOptions	"Application Options"			PR_GetApplicationOptions	PR_SetApplicationOptions				sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditLinkOptions			(GOExtend [editProjectOptions	"Linker Options"				PR_GetLinkOptions			(flip PR_SetLinkOptions)				sid <<@ GBAlwaysFloating]) GroupAlways
					, GroupAction ActionEditLinkOptions			(GOExtend [editProjectOptions	"Linker Options"				PR_GetLinkOptions			(flip PR_SetLinkOptions)				sid <<@ GBAlwaysFloating]) GroupAlways
					//, GroupAction ActionEditSyntaxColOptions	(GOExtend [editAppStateOptions	"Syntax Highlighter Colours"	(\s -> s.syntaxHighlColors)	(\c s -> {s & syntaxHighlColors = c})	sid <<@ GBAlwaysFloating]) GroupAlways
					]
					
	srcEditor :: !(DBId AppState) -> Task Void
	srcEditor sid =
			updateShared "Clean Source" "Clean Source" [] sid [srcEditorView]
		>>|	return Void
	where
		srcEditorView = editor	{ editorFrom	= \state		-> mkSourceCode state.srcEditorContent Clean
								, editorTo		= \sc state		-> {state & srcEditorContent = getSource sc}
								}
							
menuStructure =	[ Menu "File"		[ MenuItem "Save"							ActionSave					Nothing
									, MenuItem "Save & Compile..."				ActionCompile				Nothing
									, MenuSeparator
									, MenuItem "Quit"							ActionQuit					Nothing
									]
				, Menu "Search"		[ MenuItem "Find & Replace..."				ActionFind					Nothing 
									]
				, Menu "Options"	[ MenuItem "Application..."					ActionEditAppOptions		Nothing
									, MenuItem "Code Generation..."				ActionEditCodeGenOptions	Nothing
									, MenuItem "Linker..."						ActionEditLinkOptions		Nothing
									, MenuSeparator
									, MenuItem "Syntax Highlighter Colours..."	ActionEditSyntaxColOptions	Nothing
									]
				]
						
editProjectOptions desc get putback sid =
						readDB sid
	>>= \state.			pathToPDString (state.ideConfig.projectsPath +< [PathDown "test", PathDown "test.prj"])
	>>= \prjPath.		accWorld (accFiles (ReadProjectFile prjPath ""))
	>>= \(prj,ok,err).	editOptions desc prj get putback
	>>= \prj.			accWorld (accFiles (SaveProjectFile prjPath prj ""))
	>>|					stop
	
editAppStateOptions desc get putback sid =
				readDB sid
	>>= \state.	editOptions desc state get putback
	>>= \state.	writeDB sid state
	>>|			stop
	
handleErrors :: !FileException -> Task Void
handleErrors (FileException path _) = (showMessageAbout "Error" "Error" ("Could not open '" +++ path +++ "'!") >>| return Void )<<@ ExcludeGroupActions
	
ActionCompile				:== Action "compile" "compile"
ActionEditCodeGenOptions	:== Action "code-gen-opts" "codeGenOpts"
ActionEditAppOptions		:== Action "app-opts" "appOpts"
ActionEditLinkOptions		:== Action "link-opts" "linkOpts"
ActionEditSyntaxColOptions	:==	Action "synt-col-opts" "syntColOpts"

:: Mode = InCode | InString | InChar | InMLComment |InSLComment | InNum
						
highlightSyntax src colors
	# tags	= highlightSyntax` 0 InCode '\0' "" Nothing []
	# src	= foldl (+++) "" (map toString tags)
	= mkFormattedText src noControls
where
	highlightSyntax` n mode prevChar curString mbMarker acc
		| n < textSize src
			# curChar	= select src n
			# m			= inc n
			| curChar == '\0'
				// skip markers
				= highlightSyntax` (inc m) mode prevChar curString (Just {curChar, select src (inc n)}) acc
			# (pos, mode, curString, acc) = case mode of
				InCode
					| curChar == '"'								= (m,							InString,	"\"",					addToAcc 1)
					| curChar == '\''								= (m,							InChar,		"'",					addToAcc 1)
					| prevChar == '/' && curChar == '*'				= (m,							InMLComment,"/*",					addToAcc 2)
					| prevChar == '/' && curChar == '/'				= (m,							InSLComment,"//",					addToAcc 2)
					| isDigit curChar && not (isAlphanum prevChar)	= (m,							InNum,		if (prevChar == '-' || prevChar == '+') (toString [prevChar,curChar]) (toString curChar),	addToAcc (if (prevChar == '-' || prevChar == '+') 2 1))
				InString | (curChar == '"' && prevChar <> '\\') || curChar == '\n'
																	= (if (curChar == '\n') n m,	InCode,		"",						addToAcc (if (curChar == '\n') 1 0))
				InChar | (curChar == '\'' && prevChar <> '\\') || curChar == '\n'
																	= (if (curChar == '\n') n m,	InCode,		"",						addToAcc (if (curChar == '\n') 1 0))
				InMLComment | prevChar == '*' && curChar == '/'
																	= (m,							InCode,		"",						addToAcc 0)
				InSLComment | curChar == '\n'
																	= (n,							InCode,		"",						addToAcc 1)
				InNum | not (isHexDigit curChar || curChar == 'E' || curChar == 'x' || curChar == '.')
																	= (n,							InCode,		"",						addToAcc 1)
				_													= (m,							mode,		appToCurStr mbMarker,	acc)
			= highlightSyntax` pos mode curChar curString Nothing acc
		| otherwise = reverse (addToAcc 0)
	where
		appToCurStr mbMarker
			# curString = case mbMarker of
				Just marker	= curString +++ marker
				Nothing		= curString
			| n < textSize src	= curString +++ toString (select src n)
			| otherwise			= curString
		addToAcc removeChars
			# string = case removeChars of
				0	= appToCurStr Nothing
				1	= curString
				n	= subString 0 (textSize curString - 1) curString
			# string = case mbMarker of
				Just marker	= string +++ marker
				Nothing		= string
			= case mode of
				InString	= [SpanTag [StyleAttr ("color: " +++ toString colors.strings)] 				(mkTextTags string False):acc]
				InChar		= [SpanTag [StyleAttr ("color: " +++ toString colors.characters)]			(mkTextTags string False):acc]
				InNum		= [SpanTag [StyleAttr ("color: " +++ toString colors.numbers)]				(mkTextTags string False):acc]
				InSLComment	= [SpanTag [StyleAttr ("color: " +++ toString colors.singleLineComments)]	(mkTextTags string False):acc]
				InMLComment	= [SpanTag [StyleAttr ("color: " +++ toString colors.multiLineComments)]	(createLineBreaks string False):acc]
				InCode		= reverse (createLineBreaks string True) ++ acc
				
	createLineBreaks src processContent
		# lines	= split "\n" src
		= init (flatten (map (\line -> mkTextTags line processContent ++ [BrTag []]) lines))
			
	mkTextTags str processContent
		#tags = init (flatten [[wordTag word, RawText "&nbsp;"] \\ word <- split " " str])
		| processContent && indexOf "::" str <> -1	= [SpanTag [StyleAttr ("color: " +++ toString colors.typeDefinitions)] tags]
		| otherwise									= tags
	where
		wordTag word
			| processContent && isMember word keywords	= SpanTag [StyleAttr ("color: " +++ toString colors.keywords)] [Text word]
			| otherwise									= Text word
		keywords = ["where", "import", "from", "let", "in", "module", "definition", "implementation", "derive", "class", "True", "False"]

save :: !(DBId AppState) -> Task Void
save sid =
				readDB sid
	>>= \state.	writeTextFile (removeMarkers state.srcEditorContent) (state.ideConfig.projectsPath +< [PathDown "test", PathDown "test.icl"])

saveAndCompile :: !(DBId AppState) -> Task Void
saveAndCompile sid
	# compile` = try compile` handleCompilerExceptions
	# compile` = try compile` handleFileExceptions
	= compile`
where
	compile` =
						save sid
		>>|				compileToExe sid
		>>= \exeDoc.	(showMessageAbout "Download" "Download Executable" exeDoc >>| return Void )<<@ ExcludeGroupActions 
	
	handleCompilerExceptions e = (showMessageAbout "Errors" "Compiler Errors" msg >>| return Void )<<@ ExcludeGroupActions 
	where
		msg = case e of
			CannotRunCompiler msg	= ["Unable to run compiler: " +++ msg]
			CompilerErrors errs		= errs
			
	handleFileExceptions (FileException path _) = showMessageAbout "Save Error" "Save Error" ("Unnable to write to '" +++ path +++ "'") >>| return Void

openFile :: !Path !(DBId AppState) -> Task Void
openFile path sid =
				readDB sid
	>>= \state.	readTextFile path
	>>= \src.	writeDB sid {state & srcEditorContent = src}
	>>|			stop

createTestPrj :: !(DBId AppState) -> Task Void
createTestPrj sid =
				readDB sid
	>>= \state.	createDirectory (state.ideConfig.projectsPath +< [PathDown "test"])
	>>|			accWorld (createPrjFile state.ideConfig.projectsPath)
	>>= \ok.	writeTextFile "module test\n\nimport StdEnv\n\nStart = " (state.ideConfig.projectsPath +< [PathDown "test", PathDown "test.icl"])
where
	createPrjFile path world
		# (pathStr, world) = pathToPD_String (path +< [PathDown "test", PathDown "test.prj"]) world
		= accFiles (\f -> SaveProjectFile pathStr initProj "" f) world
		
	initProj = PR_NewProject "test" editOptions compilerOptions codeGenOptions appOptions ("{Project}" :! Nil) linkOptions
	where
		editOptions		= {eo = {newlines = NewlineConventionNone}, pos_size = NoWindowPosAndSize}
		compilerOptions	= DefaultCompilerOptions
		codeGenOptions	= DefCodeGenOptions
		appOptions		= DefApplicationOptions
		linkOptions		= DefaultLinkOptions
		
:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}

derive class iTask	Replace
derive bimap		Maybe, (,)

ActionReplaceAll	:== Action "replace-all" "Replace All"
		
findAndReplace :: !(DBId AppState) -> Task Void
findAndReplace sid = findAndReplace` {searchFor = "", replaceWith = ""}
where
	findAndReplace` replace =
								ExcludeGroupActions @>>
								updateInformationA "Find & Replace" "Find & Replace" [(ActionCancel, always, AsButton), (ActionReplaceAll, ifvalid, AsButton), (ActionFind, ifvalid, AsButton)] replace
		>>= \(action, replace).	case action of
									ActionReplaceAll =
													readDB sid
										>>= \state.	writeDB sid {state & srcEditorContent = replaceSubString replace.searchFor replace.replaceWith (removeMarkers state.srcEditorContent)}
										>>|			findAndReplace` replace
									ActionFind =
													readDB sid
										>>= \state.	writeDB sid {state & srcEditorContent = find replace state.srcEditorContent}
										>>|			findAndReplace` replace
									_ = stop
									
	find replace src
		# findStr = replace.searchFor
		# (startIdx, markerFound) = case indexOf SelectionEndMarker src of
			-1	= (0, False)
			i	= (i, True)
		# src = removeMarkers src
		# foundIdx = indexOfAfter startIdx findStr src
		| foundIdx == -1
			| markerFound	= find replace src
			| otherwise		= src
		= subString 0 foundIdx src +++ SelectionStartMarker +++ findStr +++ SelectionEndMarker +++ subString (foundIdx + textSize findStr) (textSize src) src