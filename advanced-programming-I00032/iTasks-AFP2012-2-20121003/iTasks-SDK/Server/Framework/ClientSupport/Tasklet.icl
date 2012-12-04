implementation module Tasklet

import iTasks, Task, TaskState, TUIEncode
import LazyLinker, CodeGeneratorJS, SaplHtml, graph_to_sapl_string
import sapldebug, StdFile, StdMisc //, graph_to_string_with_descriptors

//---------------------------------------------------------------------------------------

mkTask :: (Tasklet st res) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkTask tasklet = Task taskFunc
where
	// Init
	taskFunc mbEdit mbCommit refreshFlag taskRepOpts (TCInit taskId ts) iworld
		# (rep, st, iworld) = genRep taskId taskRepOpts iworld
		# res = tasklet.Tasklet.resultFunc st
		# result = ValueResult res ts rep (TCBasic taskId ts (toJSON res) False)
		= (result, iworld)	

	// Re-Init
	taskFunc Nothing Nothing refreshFlag taskRepOpts context=:(TCBasic taskId ts jsonRes _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# result = ValueResult res ts (placeHolderRep taskId) context
		= (result, iworld)
 
	// Edit: "result"
	taskFunc (Just (TaskEvent targetTaskId ("result", jsonRes))) mbCommit refreshFlag taskRepOpts (TCBasic taskId ts _ _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# rep = placeHolderRep taskId
		# result = ValueResult res ts rep (TCBasic taskId ts jsonRes False)
		= (result, iworld) 
 
	// Edit: "finalize"
	taskFunc (Just (TaskEvent targetTaskId ("finalize", jsonRes))) mbCommit refreshFlag taskRepOpts (TCBasic taskId ts _ _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# rep = TaskRep (appTweak (ViewPart, Nothing, [], [])) []
		# result = ValueResult res ts rep (TCDestroy (TCBasic taskId ts jsonRes False))
		= (result, iworld)  
 
	// Commit
	taskFunc mbEdit mbCommit refreshFlag taskRepOpts (TCBasic taskId ts jsonRes _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# rep = placeHolderRep taskId 
		# result = ValueResult res ts rep (TCBasic taskId ts jsonRes False)
		= (result, iworld)

	// Destroy
	taskFunc mbEdit mbCommit refreshFlag taskRepOpts (TCDestroy _) iworld
		= (DestroyedResult, iworld)

	placeHolderRep taskId 
		= TaskRep (appTweak (ViewPart, Just (defaultDef (TUITaskletPlaceholder (toString taskId))), [], [])) []

	genRep taskId taskRepOpts iworld 
		# (gui, state, iworld) = tasklet.generatorFunc taskId iworld
		= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, rf_js, _, iworld) 
					= linker state 
							 (map (eventHandlerWrapper taskId) gui.eventHandlers)
							 tasklet.Tasklet.resultFunc
						     Nothing
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js rf_js
				# taskTuiRep = appTweak (ViewPart, Just tui, [], [])
				# layout = repLayout taskRepOpts
				# taskTuiRep = appLayout layout SingleTask [taskTuiRep] [] []
						
				# rep = TaskRep taskTuiRep []						
				= (rep, state, iworld)

			TaskletTUI gui

				# (mb_ino, mb_cf) = case gui.eventHandler of
						Just (iNo, eh) = (Just (toString iNo), Just eh)
									   = (Nothing , Nothing)

				# (state_js, script_js, _, rf_js, mb_cf_js, iworld) 
					= linker state 
							 []
							 tasklet.Tasklet.resultFunc
						     (fmap controllerWrapper mb_cf)
						     iworld
					
				# tui = tTUIToTasklet gui taskId state_js script_js mb_ino rf_js mb_cf_js
				# taskTuiRep = appTweak (ViewPart, Just tui, [], [])
				# layout = repLayout taskRepOpts
				# taskTuiRep = appLayout layout SingleTask [taskTuiRep] [] []
						
				# rep = TaskRep taskTuiRep []							
				= (rep, state, iworld)

	tTUIToTasklet {TaskletTUI|tui} taskId state_js script_js mb_ino rf_js mb_cf_js
		 = (defaultDef (TUITasklet  { taskId   		 = toString taskId
									, html     		 = Nothing
								    , tui      		 = tui 
								    , st    		 = Just state_js
								    , script   		 = Just script_js
								    , events   		 = Nothing
								    , resultFunc	 = Just rf_js
								    , instanceNo	 = mb_ino
								    , controllerFunc = mb_cf_js}))

	tHTMLToTasklet {TaskletHTML|width,height,html} taskId state_js script_js events_js rf_js
		= setSize width height 
			(defaultDef (TUITasklet { taskId   		 = toString taskId
								    , html     		 = Just (toString html)
								    , tui      		 = Nothing
								    , st    		 = Just state_js
								    , script   		 = Just script_js
								    , events   		 = Just events_js
								    , resultFunc     = Just rf_js
								    , instanceNo     = Nothing
								    , controllerFunc = Nothing}))

	appTweak taskTuiRep = tweakTUI tasklet.tweakUI taskTuiRep

	/* Controller wrapper to be easier to write controler function:
	 * 1. taskId is parsed
	 * 2. TUI result is stringified 
	 */
	controllerWrapper cf strTaskID st mbEventName mbEventHandler
		# (mbTUI, st) = cf (fromString strTaskID) st mbEventName mbEventHandler
		= (fmap (toString o encodeTUIDefinition) mbTUI, st)

	// it uses the 2. layer (handleJSEvent), because it's created on the server
	eventHandlerWrapper taskId (HtmlEvent id event f) 
		= (id, event, handleJSEvent f (toString taskId))

//---------------------------------------------------------------------------------------

instance toString HtmlDef
where
	toString (HtmlDef a) = toString a

//---------------------------------------------------------------------------------------

linker state eventHandlers resultFunc mbControllerFunc iworld
	
	/* 1. First, we collect all the necessary function definitions to generate ParserState */

	# (ls, iworld) = generateLoaderState iworld
	// link functions indicated by the state structure
	# saplst = graph_to_sapl_string state
	# (ls, a, saplst, iworld) = linkSaplforExprByLoaderState ls newAppender saplst iworld

	// link functions indicated by result func
	# saplRF = graph_to_sapl_string resultFunc
	# (ls, a, saplRF, iworld) = linkSaplforExprByLoaderState ls a saplRF iworld

	// link functions indicated by controller func
	# (ls, a, mbSaplCF, iworld) = case mbControllerFunc of
		Just cf # saplCF = graph_to_sapl_string cf
				# (ls, a, saplCF, iworld) = linkSaplforExprByLoaderState ls a saplCF iworld
				= (ls, a, Just saplCF, iworld)
				= (ls, a, Nothing,  iworld)
				
	// link functions indicated by event handlers
	# (ls, a, eventHandlers, iworld) = foldl (\(ls, a, hs, iworld) (e1,e2,f) = 
				let (ls2, a2, f2, iworld2) = linkSaplforExprByLoaderState ls a (graph_to_sapl_string f) iworld
				 in (ls2, a2, [(e1,e2,f2):hs], iworld2)) 
			(ls, a, [], iworld) eventHandlers

	/* 2. Generate function definitions and ParserState */

	# sapl = toString a	
	# (script, mbPst) = case sapl of
		"" = ("", Nothing)
		   = let (script, pst) = fromOk (generateJS sapl) in (toString script, Just pst)
	
	/* 3. Generate expressions by ParserState */
									
	# statejs = toString (fromOk (exprGenerateJS saplst mbPst))

	# events = map (\(id,event,saplhandler) = (id,event,toString (fromOk 
				(exprGenerateJS saplhandler mbPst)))) eventHandlers
	
	# rfjs = toString (fromOk (exprGenerateJS saplRF mbPst))		
	
	# cfjs = case mbSaplCF of
		Just saplCF = Just (toString (fromOk (exprGenerateJS saplCF mbPst)))
					= Nothing		
					
/* For debugging:

	# (_, iworld) = writeFile "debug_state.sapl" saplst iworld
	# (_, iworld) = writeFile "debug_state.js" statejs iworld	
	# (_, iworld) = writeFile "debug.sapl" sapl iworld
	# (_, iworld) = writeFile "debug.js" script iworld
*/

	= (statejs, script, events, rfjs, cfjs, iworld)
 