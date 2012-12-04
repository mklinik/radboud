implementation module WebService

import StdList, StdBool, StdTuple
import Time, JSON_NG
import SystemTypes, Task, TaskState, TaskEval, TaskStore, UIDiff, Util, HtmlUtil, Map
import Engine, IWorld

//The representation of the JSON service
:: ServiceResponse :== [ServiceResponsePart]
:: ServiceResponsePart =
	{ taskId	:: !String
	, value		:: !JSONNode
	, actions	:: ![String]
	}
	
derive JSONEncode ServiceResponsePart

//TODO: The upload and download mechanism used here is inherently insecure!!!
// A smarter scheme that checks up and downloads, based on the current session/task is needed to prevent
// unauthorized downloading of documents and DDOS uploading.

webService :: !(HTTPRequest -> Task a) !ServiceFormat !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld) | iTask a
webService task defaultFormat req iworld=:{IWorld|timestamp,application}
	//Check for uploads
	| hasParam "upload" req
		# uploads = toList req.arg_uploads
		| length uploads == 0
			= (jsonResponse (JSONArray []),iworld)
		# (documents, iworld) = createDocumentsFromUploads uploads iworld
		// response of uploads must use content-type "text/html" or else iframe upload of extjs does not work
		# rsp = jsonResponse (toJSON documents)
		= ({rsp & rsp_headers = put "Content-Type" "text/html" rsp.rsp_headers},iworld)
		
	//Check for downloads
	| hasParam "download" req
		# (mbContent, iworld)	= loadDocumentContent downloadParam iworld
		# (mbMeta, iworld)		= loadDocumentMeta downloadParam iworld
		= case (mbContent,mbMeta) of
			(Just content,Just {Document|name,mime,size})
			
				# headers	= [("Status","200 OK"),("Content-Type", mime),("Content-Length", toString size),("Content-Disposition","attachment;filename=\"" +++ name +++ "\"")]
				= ({HTTPResponse|rsp_headers = fromList headers, rsp_data = content},iworld)
			_
				= (notFoundResponse req,iworld)
	= case format of
		//Serve start page
		WebApp	
			=  (appStartResponse application, iworld)
		//Serve the user interface representations
		JSONGui
			//Load or create session context and edit / evaluate
			# (mbResult, mbPrevUI, iworld)	= case sessionParam of
				""	
					# (mbResult, iworld) = createSessionInstance (task req) RefreshEvent iworld
					= (mbResult, Nothing, iworld)
				sessionId
					//Check if there is a previous tui definition and check if it is still current
					# (mbPrevUI,iworld)		= loadPrevUI sessionId guiVersion iworld
					# (mbResult, iworld) 	= evalSessionInstance sessionId event iworld
					= (mbResult,mbPrevUI,iworld)
			
			# (json, iworld) = case mbResult of
					Error err
						= (JSONObject [("success",JSONBool False),("error",JSONString err)],iworld)
					Ok (ExceptionResult _ err,_,_)
						= (JSONObject [("success",JSONBool False),("error",JSONString err)], iworld)
					Ok (ValueResult (Value _ Stable) _ _ _,_,_)
						= (JSONObject ([("success",JSONBool True),("done",JSONBool True)]), iworld)
					Ok (ValueResult _ info curRep context,_,sessionId)
						# json = case (mbPrevUI,curRep) of
							(Nothing, TaskRep def _)
								= JSONObject [("success",JSONBool True)
											 ,("session",JSONString sessionId)
											 ,("expiresIn",toJSON info.TaskInfo.expiresIn)
											 ,("content", encodeUIDefinition def)
											 ,("version",toJSON guiVersion)]
							
							(Just prevUI, TaskRep def _)
									= JSONObject [("success",JSONBool True)
												 ,("session",JSONString sessionId)
												 ,("expiresIn",toJSON info.TaskInfo.expiresIn)
												 ,("updates", encodeUIUpdates (diffUIDefinitions prevUI def event))
												 ,("version",toJSON guiVersion)]
							
							_
								= JSONObject [("success",JSONBool True),("done",JSONBool True)]
						//Store gui for later incremental requests
						# iworld = case curRep of
							TaskRep def _	= storeCurUI sessionId guiVersion def iworld
							_				= iworld
						= (json,iworld)
					_
						= (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
			= (jsonResponse json, iworld)
		//Serve the task in easily accessable JSON representation
		JSONService
			# (mbResult,iworld)	= case sessionParam of
				""	= createSessionInstance (task req) RefreshEvent iworld
				sessionId
					= evalSessionInstance sessionId RefreshEvent iworld
			= case mbResult of
				Ok (ExceptionResult _ err,_,_)
					= (errorResponse err, iworld)
				Ok (ValueResult (Value val Stable) _ _ _,_,_)
					= (jsonResponse (serviceDoneResponse val), iworld)
				Ok (ValueResult _ _ (TaskRep def rep) _,_,_)
					= (jsonResponse (serviceBusyResponse rep (uiDefActions def) (toList (uiDefAttributes def))), iworld)
		//Serve the task in a minimal JSON representation (only possible for non-parallel instantly completing tasks)
		JSONPlain
			# (mbResult,iworld) = createSessionInstance (task req) RefreshEvent iworld
			= case mbResult of
				Ok (ExceptionResult _ err,_,_)
					= (errorResponse err, iworld)
				Ok (ValueResult (Value val _) _ _ _,_,_)
					= (jsonResponse val, iworld)
				_
					= (errorResponse "Requested service format not available for this task", iworld)
		//Error unimplemented type
		_
			= (jsonResponse (JSONString "Unknown service format"), iworld)	
where
	format			= case formatParam of
		"webapp"			= WebApp
		"json-gui"			= JSONGui
		"json-service"		= JSONService
		"json-plain"		= JSONPlain
		_					= defaultFormat

	formatParam			= paramValue "format" req

	sessionParam		= paramValue "session" req
	downloadParam		= paramValue "download" req
	uploadParam			= paramValue "upload" req
	versionParam		= paramValue "version" req

	editEventParam		= paramValue "editEvent" req
	actionEventParam	= paramValue "actionEvent" req
	focusEventParam		= paramValue "focusEvent" req
	
	event = case (fromJSON (fromString editEventParam)) of
		Just (taskId,name,value)	= EditEvent (fromString taskId) name value
		_	= case (fromJSON (fromString actionEventParam)) of
			Just (taskId,actionId)	= ActionEvent (fromString taskId) actionId
			_	= case (fromJSON (fromString focusEventParam)) of
				Just taskId			= FocusEvent (fromString taskId)
				_					= RefreshEvent
	
	guiVersion			= toInt versionParam

	jsonResponse json
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}
	errorResponse msg
		= {HTTPResponse | rsp_headers = fromList [("Status", "500 Internal Server Error")], rsp_data = msg}
			
	serviceBusyResponse rep actions attributes
		= JSONObject [("status",JSONString "busy"),("parts",parts),("attributes",JSONObject [(k,JSONString v) \\ (k,v) <- attributes])]
	where
		parts = toJSON [{ServiceResponsePart|taskId = toString taskId, value = value, actions = findActions taskId actions} \\ (taskId,value) <- rep]
		findActions match actions
			= [actionName action \\ {taskId,action,enabled} <- actions | enabled && taskId == match]
	
	serviceDoneResponse val
		= JSONObject [("status",JSONString "complete"),("value",val)]
	serviceErrorResponse e
		= JSONObject [("status",JSONString "error"),("error",JSONString e)]

	appStartResponse appName = {newHTTPResponse & rsp_data = toString (appStartPage appName)}

	appStartPage appName = HtmlTag [] [head,body]
	where
		head = HeadTag [] [TitleTag [] [Text appName]: styles ++ scripts]
		body = BodyTag [] []
	
		styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
		scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
		
		stylefiles = ["lib/extjs-4.1.0/resources/css/ext-all-gray.css"
					 ,"css/icons.css"
					 ,"css/app.css"
					 ,appName +++ ".css"]
		scriptfiles = ["lib/extjs-4.1.0/ext-debug.js",
					   "app/taskeval/utils.js","app/taskeval/itask.js", //UGLY INCLUSION, MUST BE MERGED INTO ITWC FRAMEWORK
					   "app/taskeval/builtin.js","app/taskeval/sapl.js",
					   "app/taskeval/db.js", "app/taskeval/debug.js",				   
					   "app.js"]
		//scriptfiles = ["/lib/ext-4.1.0/ext.js","/app-all.js"]

	createDocumentsFromUploads [] iworld = ([],iworld)
	createDocumentsFromUploads [(n,u):us] iworld
		# (mbD,iworld)	= createDocument u.upl_filename u.upl_mimetype u.upl_content iworld
		| isError mbD	= createDocumentsFromUploads us iworld
		# (ds,iworld)	= createDocumentsFromUploads us iworld
		= ([fromOk mbD:ds],iworld)
		