implementation module HTTP

import StdOverloaded, StdString, StdList, StdArray, StdFile, StdBool
import Maybe, Map, Text, UrlEncoding, MIME

newHTTPRequest :: HTTPRequest
newHTTPRequest		= {	req_method		= ""
					,	req_path		= ""
					,	req_query		= ""
					,	req_version		= ""
					,	req_protocol	= HTTPProtoHTTP
					,	req_headers		= newMap
					,	req_data		= ""
					,	arg_get			= newMap
					,	arg_post		= newMap
					,	arg_cookies		= newMap
					,	arg_uploads		= newMap
					,	server_name		= ""
					,	server_port		= 0
					,	client_name		= ""
					}
					
newHTTPResponse :: HTTPResponse					
newHTTPResponse		= {	rsp_headers		= newMap
					,	rsp_data		= ""
					}

newHTTPUpload :: HTTPUpload
newHTTPUpload		= {	upl_name		= ""
					,	upl_filename	= ""
					,	upl_mimetype	= ""
					,	upl_content		= ""
					}

instance toString HTTPRequest
where
	toString {	req_method
			 ,	req_path
	 		 ,	req_query
			 ,	req_version
			 ,	req_protocol
			 ,	req_headers	
			 ,	req_data		
			 ,	arg_get
			 ,	arg_post
			 ,	arg_cookies
			 ,	arg_uploads	
			 ,	server_name
			 ,	server_port
			 ,	client_name
			 }
			 = "Method: " +++ req_method +++ "\n" +++
			   "Path: " +++ req_path +++ "\n" +++
			   "Query: " +++ req_query +++ "\n" +++
			   "Version: " +++ req_version +++ "\n" +++
			   "Protocol: " +++  toString req_protocol +++ "\n" +++
			   "---Begin headers---\n" +++
			   (foldr (+++) "" [ n +++ ": " +++ v +++ "\n" \\ (n,v) <- toList req_headers]) +++
			   "---End headers---\n" +++
			   "---Begin data---\n" +++
			   req_data +++
			   "--- End data---\n"
			   
instance toString HTTPResponse
where
	toString {	rsp_headers
			 ,	rsp_data
			 }
			 = "---Begin headers---\n" +++
			   (foldr (+++) "" [ n +++ ": " +++ v +++ "\n" \\ (n,v) <- toList rsp_headers]) +++
			   "---End headers---\n" +++
			   "---Begin data---\n" +++
			   rsp_data +++
			   "--- End data---\n"
	
instance toString HTTPProtocol
where
	toString HTTPProtoHTTP = "Http"
	toString HTTPProtoHTTPS = "Https"

//Server utilities
parseRequestLine	:: !String																							-> Maybe (!String, !String, !String, !String)
parseRequestLine line
	# parts						= split " " line
	| length parts <> 3			= Nothing
	# [method,path,version:_]	= parts
	# qindex					= indexOf "?" path
	| qindex <> -1				= Just (method, path % (0, qindex - 1), path % (qindex + 1, size path), version)
								= Just (method, path, "", version)
	
parseHeader			:: !String																							-> Maybe (!String, !String)
parseHeader header
	# index					= indexOf ":" header
	| index < 1				= Nothing
	# name					= trim (header % (0, index - 1))
	# value					= trim (header % (index + 1, size header))
	= Just (name,value)

//Request utilities
parseRequest 		::	!HTTPRequest																					-> HTTPRequest
parseRequest req
	# req 							= {req & arg_get = parseGetArguments req}		//Parse get arguments
	# type							= case (get "Content-Type") req.req_headers of
		(Just ct)	= ct
		(Nothing)	= ""
	| type % (0,32) == "application/x-www-form-urlencoded"
		= {req & arg_post = parsePostArguments req}									//Parse post arguments
	| type % (0,18) == "multipart/form-data"
		# (post,uploads)			= parseMultiPartPostArguments req
		= {req & arg_post = fromList post, arg_uploads = fromList uploads}			//Parse post arguments + uploads
	| otherwise						= req
where	
	parseGetArguments :: !HTTPRequest -> Map String String
	parseGetArguments req
		| req.req_query == ""	= newMap
								= fromList (urlDecodePairs req.req_query)

	parsePostArguments :: !HTTPRequest -> Map String String
	parsePostArguments req		= fromList (urlDecodePairs req.req_data)

	parseMultiPartPostArguments :: !HTTPRequest -> ([(String,String)],[(String,HTTPUpload)])
	parseMultiPartPostArguments req
		# mimetype				= get "Content-Type" req.req_headers
		| isNothing	mimetype	= ([],[]) //Fail
		# mimetype				= fromJust mimetype
		# index					= indexOf "boundary=" mimetype
		| index == -1			= ([],[]) //Fail
		# boundary				= mimetype % (index + 9, size mimetype)
		# parts					= decodeMimeMultipart boundary req.req_data
		= parseParts parts [] []
		where
			parseParts [] arguments uploads	= (arguments, uploads)
			parseParts [(headers, body):xs] arguments uploads
				# disposition		= [v \\ (k,v) <- headers | k == "Content-Disposition" ]
				# type				= [v \\ (k,v) <- headers | k == "Content-Type" ]
				| isEmpty disposition || isEmpty type
					= parseParts xs arguments uploads
				# disposition		= hd disposition
				# type				= hd type
				# name				= getParam "name" disposition
				| name == ""		= parseParts xs arguments uploads
				# filename			= getParam "filename" disposition
				| filename == ""	= parseParts xs [(name,body):arguments] uploads
				| otherwise			= parseParts xs arguments [	(name,	{ newHTTPUpload
																		& upl_name		= name
																		, upl_filename	= filename
																		, upl_mimetype	= type
																		, upl_content	= body
																		}):uploads]
			getParam name header
				# index	= indexOf (name +++ "=") header
				| index == -1	= ""
				# header = header % (index + (size name) + 1, size header)
				# index	= indexOf ";" header
				| index == -1	= removequotes header
								= removequotes (header % (0, index - 1))

			removequotes s
				| size s < 2	= s
				# start	= if (s.[0] == '"') 1 0
				# end = if (s.[size s - 1] == '"') (size s - 2) (size s - 1)
				= s % (start, end) 

//Generating responses
staticResponse		:: !HTTPRequest !*World																				-> (!HTTPResponse, !*World)
staticResponse req world
	# filename				= req.req_path % (1, size req.req_path)		//Remove first slash
	# (type, world)			= fileMimeType filename world
	# (ok, content, world)	= fileContent filename world
	| not ok 				= notfoundResponse req world
							= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   ,rsp_data = content}, world)						
where
	fileContent :: !String !*World -> (!Bool, !String, !*World)
	fileContent filename world
		# (ok, file, world)	= fopen filename FReadData world
		| not ok			= (False, "Could not open file", world)
		# (ok, file)		= fseek file 0 FSeekEnd
		| not ok			= (False, "Seek to end of file does not succeed", world)
		# (pos, file)		= fposition file
		# (ok, file)		= fseek file (~pos) FSeekCur
		| not ok			= (False, "Seek to begin of file does not succeed", world)
		# (content, file)	= freads file pos
		# (ok, world)		= fclose file world
		= (True, content, world)

	fileMimeType :: !String !*World -> (!String, !*World)
	fileMimeType ".jpg" world = ("image/jpeg",world)
	fileMimeType ".png" world = ("image/png",world)
	fileMimeType ".gif" world = ("image/gif",world)
	fileMimeType ".bmp" world = ("image/bmp",world)
	fileMimeType ".htm" world = ("text/html",world)
	fileMimeType ".html" world = ("text/html",world)
	fileMimeType ".txt" world = ("text/plain",world)
	fileMimeType ".css" world = ("text/css",world)
	fileMimeType ".js" world = ("text/javascript",world)
	fileMimeType "" world = ("application/octet-stream",world)
	fileMimeType name world = fileMimeType (name % (1, size name)) world


notfoundResponse	:: !HTTPRequest !*World 																			-> (!HTTPResponse, !*World)
notfoundResponse req world = ({rsp_headers = fromList [("Status","404 Not Found")], rsp_data = "404 - Not found"},world)

forbiddenResponse	:: !HTTPRequest !*World 																			-> (!HTTPResponse, !*World)
forbiddenResponse req world = ({rsp_headers = fromList [("Status","403 Forbidden")], rsp_data = "403 - Forbidden"},world)

customResponse		:: ![((String -> Bool),(HTTPRequest *World -> (HTTPResponse, *World)))] !Bool !HTTPRequest !*World	-> (!HTTPResponse, !*World)
customResponse [] fallback request world 											//None of the request handlers matched
	| fallback						= (staticResponse request world)				//Use the static response handler
									= (notfoundResponse request world)				//Raise an error
customResponse [(pred,handler):rest] fallback request world
	| (pred request.req_path)		= handler request world							//Apply handler function
									= customResponse rest fallback request world	//Search the rest of the list



//Response utilities
encodeResponse		:: !Bool !HTTPResponse !*World																		-> (!String,!*World)
encodeResponse withreply {rsp_headers = headers, rsp_data = data} world  
	# reply = if withreply
			("HTTP/1.0 " +++ (default "200 OK" (get "Status" headers)) +++ "\r\n")
			("Status: " +++ (default "200 OK" (get "Status" headers)) +++ "\r\n")
	# reply = reply +++ ("Server: " +++ (default "Clean HTTP tools" (get "Server" headers)) +++ "\r\n")							//Server identifier	
	# reply = reply +++	("Content-Type: " +++ (default "text/html" (get "Content-Type" headers)) +++ "\r\n")					//Content type header
	# reply = reply +++	("Content-Length: " +++ (toString (size data)) +++ "\r\n")												//Content length header
	# reply = reply +++	(foldr (+++) "" [(n +++ ": " +++ v +++ "\r\n") \\ (n,v) <- toList headers | not (skipHeader n)])		//Additional headers
	# reply = reply +++	("\r\n" +++ data)																						//Separator + data
	= (reply, world)
where
	//Do not add these headers two times
	default def mbval = case mbval of
		Nothing	= def
		(Just val) = val
	skipHeader s = isMember s ["Status","Date","Server","Content-Type","Content-Length","Last-Modified"]
