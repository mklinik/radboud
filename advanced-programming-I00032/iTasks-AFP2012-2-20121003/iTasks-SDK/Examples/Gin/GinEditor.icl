implementation module GinEditor

import StdFile

import GenEq
import Text
import iTasks

import GinAbstractSyntax
import GinConfig
import GinCompiler
import GinDomain
import GinParser
from GinPrinter import ::Doc, prettyPrint, instance Printer Doc
import GinStorage
import GinSyntax

import CleanDocParser
import GinDCLImport

import FilePath, File
from Tuple import appFst


/*
* NOTE: In order to add compiled tasks to a running server, 
*       check "Enable dynamics" in Project -> Project options,
*       and replace the readDynamicTask function by the lines below:
*/
/*
from Serialization import qualified serialize, deserialize
readDynamicTask :: !String -> Task (Task a) | iTask a
readDynamicTask filename = importTextFile filename >>= \dynString -> 
	case 'Serialization'.deserialize dynString of
		Ok value = return value
		Error errorString = throw (DynamicIOException errorString)
*/
readDynamicTask filename = return (showInformation "Error: dynamic linker not enabled" [] Void)

ginEditor :: WorkflowContainer Void
ginEditor = Workflow initManagerProperties ginEditor`

getAndSetupConfig :: Task GinConfig
getAndSetupConfig = getConfig >>= \config -> accWorld (ginCheckConfig config) >>= \error = if (isNothing error) (return config) setupDialog 
        
getConfig :: Task GinConfig
getConfig = accWorld ginLoadConfig >>= \mbConfig -> case mbConfig of
	Just config = return config
    Nothing = accWorld ginDefaultConfig >>= \config -> appWorld (ginStoreConfig config) >>| return config

setupDialog :: Task GinConfig
setupDialog = getConfig >>= dialog >>= \config -> appWorld (ginStoreConfig config) >>|return config
where
    dialog config = updateInformation "GiN editor setup" [] config >>= \config = 
                    accWorld (ginCheckConfig config) >>= \error = if (isNothing error) (return config) (dialog config)

:: EditorState = 
    { config		:: !GinConfig
    , name			:: !Maybe String
    , gMod			:: !GModule
    , checkSyntax	:: !Bool
    , changed		:: !Bool
    , dirty			:: !Bool
    , errors		:: ![ORYXError]
    , source		:: !String
    , compiled		:: !Maybe String
    }

derive class iTask EditorState
    
getInitialState :: Task EditorState 
getInitialState = getAndSetupConfig >>= \config -> return
	{ EditorState
	| config		= config
	, name			= Nothing
	, gMod			= updateDiagramExtensions newModule
	, checkSyntax	= True
	, changed		= False
	, dirty			= False
	, errors		= []
	, source		= ""
	, compiled		= Nothing
	}

//-----------------------------------------------------------------------------------        
ActionCompile          :== Action "File/Compile"			
ActionRun              :== Action "File/Run"				
ActionViewDeclaration  :== Action "View/Declaration"		
ActionViewWorkflow     :== Action "View/Workflow"			
ActionViewImports      :== Action "View/Imports"			
ActionViewTypes        :== Action "View/Types"				   
ActionViewSource       :== Action "View/Generated source"   
ActionEnableSC         :== Action "Options/Enable syntax checking"
ActionDisableSC        :== Action "Options/Disable syntax checking"
ActionConfiguration    :== Action "Options/Configuration"

ginEditor` :: Task Void
ginEditor` = 
	getAndSetupConfig >>|
	getInitialState >>= \initialState -> 
	ginParallelLayout @>> 
	parallel
		"GiN Editor"
		initialState
		(\_ _ -> Void)
		[ (BodyTask, \s -> forever (ginInteractionLayout @>>
				(updateSharedInformation "Workflow diagram" 
					[UpdateView (GetShared diagramView, PutbackShared diagramUpdate)] 
					(taskListState s) Void) >>+ noActions`
				))
		, (HiddenTask, \s -> (chooseAction (actions s) >>= id) <! isStop)
		, (HiddenTask, activator)		
		]
where
	noActions` :: (TermFunc a Void) | iTask a
	noActions` = noActions
	
	isStop :: ParallelControl -> Bool
	isStop Stop = True
	isStop _    = False

ginParallelLayout :: ParallelLayouter
ginParallelLayout = \par=:{TUIParallel|title,instruction,items}-> 
	case items of
		[(_,Just editor,_),(_,_,actions),activator]	= (editor,actions)
		_ 											= defaultParallelLayout par

ginInteractionLayout :: InteractionLayouter
ginInteractionLayout = \interaction = 
	case interaction.editorParts of
		[{TUIDef | content = TUIEditControl (TUIORYXControl _) _}] =
			({TUIDef | hd interaction.editorParts & width = Just (FillParent 1 (FixedMinSize 400))},interaction.TUIInteraction.actions)
		_ 	= defaultInteractionLayout interaction

diagramView :: EditorState -> ORYXEditor
diagramView { EditorState | gMod = { moduleKind = GGraphicalModule defs }, errors } = 
	{ ORYXEditor 
	| (ginORYXEditor (hd defs).GDefinition.body)
	& errors = errors 
	}

diagramUpdate :: ORYXEditor Void EditorState -> EditorState
diagramUpdate editor _ state = { EditorState | state & gMod = setDiagram state.gMod editor, dirty = True}
where
	setDiagram :: !GModule !ORYXEditor -> GModule
	setDiagram gMod =:{moduleKind = (GGraphicalModule defs)} editor=:{diagram}
		=	{ GModule 
			| gMod
			& moduleKind = GGraphicalModule
				( [ { GDefinition 
					| hd defs 
					& body = diagram
					}
					: tl defs
				  ]
				)
			}

activator :: (TaskList EditorState) -> Task ParallelControl
activator taskList = forever activator` 
where
	stateShared = taskListState taskList
	
	activator` :: Task ParallelControl
	activator` =	(showSharedInformation "Diagram monitor" [] stateShared Void >? \(state,_) -> state.dirty) //Look for the dirty flag to become True
					>>= \(state,_) -> return { EditorState | state & dirty = False, changed = True }
					>>= generateSource
					>>= \state -> (if state.EditorState.checkSyntax 
									(checkErrors state)
									(return state))
					>>= set stateShared
					>>| return Continue

generateSource :: EditorState -> Task EditorState
generateSource state = accWorld (tryRender state.EditorState.gMod state.EditorState.config POICL) 
	>>= \source -> return { EditorState | state & source = source }


checkErrors :: EditorState -> Task EditorState
checkErrors state=:{ EditorState | gMod = { moduleKind = GGraphicalModule defs } }
	= accIWorld (syntaxCheck state.EditorState.gMod)
	  >>= transform (\compileResult -> { EditorState | state & errors = makeErrorString compileResult })
where
	makeErrorString :: (CompileResult a) -> [ORYXError]
	makeErrorString (CompileSuccess _) = []
	makeErrorString (CompileGlobalError error) = [makeORYXError ((hd defs).GDefinition.body) ([], error)]
	makeErrorString (CompilePathError errors) = map (makeORYXError ((hd defs).GDefinition.body)) errors

actions :: (TaskList EditorState) -> [(Action, Task ParallelControl)]
actions taskList
	=	[ (ActionNew,              actionTask (\s -> askSaveIfChanged s >>| getInitialState))
		, (ActionOpen,             actionTask open)
		, (ActionSave,             actionTask save)
		, (ActionSaveAs,           actionTask saveAs)
		, (ActionCompile,          actionTask compile)
		, (ActionQuit,             return Stop)
		, (ActionViewDeclaration,  moduleEditor "Declaration" (declarationView, declarationUpdate))
		, (ActionViewImports,      importsEditor)
		, (ActionViewTypes,        moduleEditor "Types" (typesView, typesUpdate))
		, (ActionViewSource,       sourceView)
		, (ActionEnableSC,	       actionTask (\s -> checkErrors { s & checkSyntax = True }))
		, (ActionDisableSC,	       actionTask (\s -> return { s & checkSyntax = False }))
		, (ActionConfiguration,	   addTask setupDialog)
		, (ActionAbout,            actionTask showAbout)
		]
	where
		stateShared = taskListState taskList
		
		addTask task
			=	defaultInteractionLayout
			@>> appendTask (BodyTask, \_ -> task >>| return Continue) taskList >>| return Continue

		actionTask task = addTask (get stateShared >>= task >>= set stateShared)

		moduleEditor title v = addTask (updateSharedInformation title [UpdateView (app2 (GetShared,\f -> PutbackShared (\a _ e -> f a e)) (liftModuleView v))] stateShared Void)
		
		declarationEditor = moduleEditor "declaration" (declarationView, declarationUpdate)
		importsEditor = addTask 
			(					get stateShared 
				>>= \state	 ->	accWorld (searchPathModules state.EditorState.config)
				>>= \modules ->	moduleEditor "imports" (importsView modules, importsUpdate)
			)
		sourceView = addTask (showSharedInformation "source view" [ShowView (GetShared (\s -> formatSource s.EditorState.source))] stateShared Void)

liftModuleView :: (GModule -> a, a GModule -> GModule) -> (EditorState -> a, a EditorState -> EditorState)
liftModuleView (toView, fromView) = 
	( \model -> toView model.gMod
	, \view model -> { model & gMod = fromView view model.gMod, changed = True }
	)
	
:: DeclarationView =
	{ title			:: !Maybe String
	, description	:: !Maybe String
	, parameters	:: !Maybe [FormalParameterView]
	, returnType 	:: !TypeExpressionView
	}
	
:: FormalParameterView = 
	{ name	:: !String
	, type	:: !TypeExpressionView
	}

derive class iTask DeclarationView, FormalParameterView

:: TypeExpressionView = TypeExpressionView String

toTypeExpressionView :: GTypeExpression -> TypeExpressionView
toTypeExpressionView te = TypeExpressionView (prettyPrint (printGTypeExpression False te))

fromTypeExpressionView :: TypeExpressionView -> GTypeExpression
fromTypeExpressionView (TypeExpressionView tev) = 
	case parseTypeUnsafe tev of
		Nothing = GUndefinedTypeExpression
		Just te = mapType te

gVisualizeText{|TypeExpressionView|} _ (TypeExpressionView v) = [v]
gVisualizeHtml{|TypeExpressionView|} _ (TypeExpressionView v) = [RawText v]
gVisualizeEditor{|TypeExpressionView|} val vst = visualizeControlSimple TUIStringControl val vst

gUpdate{|TypeExpressionView|} mode ust = basicUpdate mode parseUpdate (TypeExpressionView "") ust
where
	parseUpdate update orig = fromMaybe orig (fmap TypeExpressionView update)
derive gDefaultMask TypeExpressionView
gVerify{|TypeExpressionView|} val vst = wrapperVerify Nothing
	(\(TypeExpressionView value) -> isJust (parseTypeUnsafe value)) (\_ -> "Invalid type") val vst
JSONEncode{|TypeExpressionView|} (TypeExpressionView x) = [JSONString x]
JSONDecode{|TypeExpressionView|} [JSONString s:xs]	= (Just (TypeExpressionView s), xs)
JSONDecode{|TypeExpressionView|} l					= (Nothing, l)
derive gEq TypeExpressionView

declarationView :: !GModule -> DeclarationView
declarationView {moduleKind = (GGraphicalModule [{GDefinition | declaration = 
		{GDeclaration | name, title, description, formalParams, returnType = GTypeApplication [GConstructor "Task",rt] }}:_])} =
	{ DeclarationView
	| title = title
	, description = description
	, parameters = if (isEmpty formalParams) Nothing (Just (map formalParameterView formalParams))
	, returnType = toTypeExpressionView rt
	}
	
declarationUpdate :: !DeclarationView !GModule -> GModule
declarationUpdate {DeclarationView | title, description, parameters, returnType} 
	gMod=:{moduleKind = (GGraphicalModule [def=:{GDefinition | declaration}:defs])} =
	{ gMod
	& moduleKind = GGraphicalModule 
		[	{ GDefinition 
			| def & declaration = 
				{ GDeclaration
				| declaration
				& title = title
				, description = description
				, formalParams = case parameters of
					Nothing = []
					Just pars =	map formalParameterUpdate pars
				, returnType = GTypeApplication [GConstructor "Task", fromTypeExpressionView returnType]
					
				}
			}
			: defs
		]
	}

formalParameterView :: !GFormalParameter -> FormalParameterView
formalParameterView { GFormalParameter | name, type} = 
	{ FormalParameterView
	| name = name
	, type = TypeExpressionView (prettyPrint (printGTypeExpression False type))
	}
	
formalParameterUpdate :: !FormalParameterView -> GFormalParameter
formalParameterUpdate { FormalParameterView | name, type = (TypeExpressionView t) } =
	{ GFormalParameter
	| name = name
	, title = Just name
	, description = Nothing
	, type = case parseTypeUnsafe t of
		Nothing = GUndefinedTypeExpression
		Just te = mapType te
	, defaultValue = Nothing
	, visible = True
	}

importsView :: ![String] !GModule -> CheckMultiChoice String String
importsView allModules gMod = mkCheckMultiChoice [(m,m) \\ m <- allModules] gMod.GModule.imports

importsUpdate :: (CheckMultiChoice String String) GModule -> GModule
importsUpdate choice gMod =
	updateDiagramExtensions { GModule | gMod & imports = getSelections choice }
	
typesView :: !GModule -> Maybe [GTypeDefinition]
typesView gMod = case gMod.GModule.types of
	[] = Nothing
	t  = Just t

typesUpdate :: !(Maybe [GTypeDefinition]) !GModule -> GModule
typesUpdate mbTypes gMod = { GModule | gMod & types = fromMaybe [] mbTypes }

getName :: EditorState -> String
getName state = case state.EditorState.name of
    Just n  -> n
    Nothing -> "(unnamed)"

setChanged :: EditorState EditorState -> EditorState
setChanged old new = if (old.EditorState.gMod =!= new.EditorState.gMod) { new & changed = True } new        

open :: EditorState -> Task EditorState 
open state = getInitialState >>= \initialState -> chooseModule state.EditorState.config >>= \mMod = 
	case mMod of
		Just (name, gMod) = return { EditorState | initialState & name = Just name, gMod = gMod } >>= generateSource
		Nothing			  = return state

save :: EditorState -> Task EditorState
save state = case state.EditorState.name of
    Just name  -> writeModule state.EditorState.config name state.EditorState.gMod >>| 
				  return { state & changed = False }
    Nothing    -> saveAs state

saveAs :: EditorState -> Task EditorState
saveAs state = newModuleName state.EditorState.config >>= \name = 
	save { EditorState | state & name = Just name }

askSaveIfChanged :: EditorState -> Task Void
askSaveIfChanged state = if state.changed
    (		showInformation ("File " +++ (getName state) +++ " has changed, save changes?") [] Void
        >?*	[ (ActionNo,	Always (return Void))
        	, (ActionYes,	Always (save state >>| return Void))
        	]
    )
    (return Void)
where
	requestConfirmation :: !String -> Task Bool 
	requestConfirmation message = showInformation message Void >>+ \_ -> UserActions [(ActionYes, True), (ActionNo, False)]

compile :: EditorState -> Task EditorState
compile state
# state = { state & compiled = Nothing }
= accIWorld (batchBuild state.EditorState.gMod)
  >>= \result = case result of
	CompileSuccess dynfile 	->	showInformation ("Compiler output", "Compiled successfully. Click \"Refresh workflows\" to view the task") [] Void 
								>>| readDynamicTask dynfile 
								>>= \task -> addWorkflow (makeWorkflow state task)
   				  				>>| return { state & compiled = Just dynfile }
    error					-> showInformation "Compiler output" [About error] Void >>| return state
where
	makeWorkflow :: EditorState (Task Void) -> Workflow
	makeWorkflow {EditorState | gMod = { GModule | moduleKind = GGraphicalModule [def:_]}} dyn
	# decl = def.GDefinition.declaration
	= workflow (fromMaybe "(no title)" decl.GDeclaration.title) (fromMaybe "(no description)" decl.GDeclaration.description) dyn

:: DynamicIOException = DynamicIOException !String
derive class iTask DynamicIOException

instance toString DynamicIOException
where
	toString (DynamicIOException errorString) = errorString

formatSource :: String -> HtmlDisplay
formatSource source = toHtmlDisplay (TextareaTag [ColsAttr "80", RowsAttr "25"] [ Text source ])

tryRender :: GModule GinConfig PrintOption *World -> (String, *World)
tryRender gMod config printOption world
# (st, world) = gToAModule gMod config world
# source = case runParse st of
	GSuccess aMod -> prettyPrintAModule printOption aMod
	GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = msg) errors)))
= (source, world)

showAbout :: EditorState -> Task EditorState
showAbout state = showInformation "Gin workflow editor" [] "version 0.2" >>| return state

accIWorld :: !(*IWorld -> *(!a,!*IWorld)) -> Task a | iTask a
accIWorld fun = mkInstantTask ("Run Iworld function", "Run an IWorld function and get result.") eval
where
	eval taskNr iworld
		# (res,iworld)	= fun iworld
		= (TaskFinished res,iworld)

