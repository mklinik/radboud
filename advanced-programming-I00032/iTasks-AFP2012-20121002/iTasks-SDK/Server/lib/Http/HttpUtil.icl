implementation module HttpUtil

import HTTP
import StdArray, StdOverloaded, StdString, StdFile, StdBool, StdInt, StdArray, StdList, StdFunc, StdTuple
import Time, Text, UrlEncoding, Map

http_splitMultiPart :: !String !String -> [([(String,String)], String)]
http_splitMultiPart boundary body
	# startindex		= indexOf ("--" +++ boundary +++ "\r\n") body //Locate the first boundary
	| startindex == -1	= [] //Fail
	# endindex			= indexOf ("\r\n" +++ "--" +++ boundary +++ "--") body //Locate the final boundary
	| endindex == -1	= [] //Fail
	# body				= body % (startindex + (size boundary) + 4, endindex - 1)
	# parts				= split ("\r\n" +++ "--" +++ boundary +++ "\r\n") body
	= map parsePart parts
where
	parsePart :: String -> ([(String,String)], String)
	parsePart part 
		# index 		= indexOf "\r\n\r\n" part
		| index < 1 	= ([], part)
						= ([header \\ (header,error) <- map http_parseHeader (split "\r\n" (part % (0, index - 1))) | not error]
							, part % (index + 4, size part))

//Parsing of HTTP Request messages

//Add new data to a request
http_addRequestData :: !HTTPRequest !Bool !Bool !Bool !String -> (HTTPRequest, Bool, Bool, Bool, Bool)
http_addRequestData req requestline_done headers_done data_done data
	# req	= {req & req_data = req.req_data +++ data}	//Add the new data
	//Parsing of the request line
	| not requestline_done
		# index = indexOf "\r\n" req.req_data
		| index == -1	= (req,False,False,False,False)	//The first line is not complete yet
		| otherwise
			# (method,path,query,version,error) = http_parseRequestLine (req.req_data % (0, index - 1))
			| error	= (req,False,False,False,True)			//We failed to parse the request line
			# req = {req & req_method = method, req_path = path, req_query = query, req_version = version, req_data = req.req_data % (index + 2, size req.req_data) }
			= http_addRequestData req True False False ""	//We are done with the request line but still need to inspect the rest of the data
	//Parsing of headers
	| not headers_done
		# index = indexOf "\r\n" req.req_data
		| index == -1	= (req,True,False,False,False)		//We do not have a full line yet
		| index == 0										//We have an empty line, this means we have received all the headers
			# req = {req & req_data = req.req_data % (2, size req.req_data)}
			= http_addRequestData req True True False ""	//Headers are finished, continue with the data part
		| otherwise
			# (header,error) = http_parseHeader (req.req_data % (0, index - 1))
			| error = (req,True,False,False,True)			//We failed to parse the header
			# req = {req & req_headers = put (fst header) (snd header) req.req_headers, req_data = req.req_data % (index + 2, size req.req_data)}
			= http_addRequestData req True False False ""	//We continue to look for more headers
	//Addition of data
	| not data_done
		# datalength	= toInt (http_getValue "Content-Length" req.req_headers "0")
		| (size req.req_data) < datalength	= (req,True,True,False,False)	//We still need more data
											= (req,True,True,True,False)	//We have all data and are done
	//Data is added while we were already done
	= (req,True,True,True,False) 


http_getValue :: !String !(Map String String) !String -> String
http_getValue key valuemap defaultval
	= fromMaybe defaultval (get key valuemap)

http_parseRequestLine :: !String -> (!String,!String,!String,!String,!Bool)
http_parseRequestLine line
	# parts	= split " " line
	| length parts <> 3	= ("","","","",True)
	# [method,path,version:_]	= parts
	# qindex					= indexOf "?" path
	| qindex <> -1				= (method, path % (0, qindex - 1), path % (qindex + 1, size path), version, False)
								= (method, path, "", version, False)
								
http_parseHeader :: !String -> (!(String,String), !Bool)
http_parseHeader header
	# index					= indexOf ":" header
	| index < 1				= (("",""), False)
	# name					= trim (header % (0, index - 1))
	# value					= trim (header % (index + 1, size header))
	= ((name,value), False)

http_parseArguments :: !HTTPRequest -> HTTPRequest
http_parseArguments req
	# req 							= {req & arg_get = http_parseGetArguments req}		//Parse get arguments
	| isPost req.req_headers		= {req & arg_post = http_parsePostArguments req}	//Parse post arguments
	| isMultiPart req.req_headers
		# (post,uploads)			= http_parseMultiPartPostArguments req
		= {req & arg_post = post, arg_uploads = uploads}								//Parse post arguments + uploads
	| otherwise						= req
where
	isPost headers = (http_getValue "Content-Type" headers "") % (0,32) == "application/x-www-form-urlencoded"
	isMultiPart headers = (http_getValue "Content-Type" headers "") % (0,18) == "multipart/form-data"
	
http_parseGetArguments :: !HTTPRequest -> Map String String
http_parseGetArguments req
	| req.req_query == ""	= fromList []
							= fromList (http_parseUrlEncodedArguments req.req_query)

http_parsePostArguments :: !HTTPRequest -> Map String String
http_parsePostArguments req	= fromList (http_parseUrlEncodedArguments req.req_data)

http_parseUrlEncodedArguments :: !String -> [(String,String)]
http_parseUrlEncodedArguments s = [(urlDecode name, urlDecode (join "=" value)) \\ [name:value] <- map (split "=") (split "&" s)]

http_parseMultiPartPostArguments :: !HTTPRequest -> (Map String String, Map String HTTPUpload)
http_parseMultiPartPostArguments req
	# mimetype		= http_getValue "Content-Type" req.req_headers ""
	# index			= indexOf "boundary=" mimetype
	| index == -1	= (newMap,newMap)
	# boundary		= mimetype % (index + 9, size mimetype)
	# parts			= http_splitMultiPart boundary req.req_data
	= parseParts parts newMap newMap
where
	parseParts [] arguments uploads	= (arguments, uploads)
	parseParts [(headers, body):xs] arguments uploads
		# disposition		= http_getValue "Content-Disposition" (fromList headers) ""
		| disposition == ""	= parseParts xs arguments uploads
		# name				= getParam "name" disposition
		| name == ""		= parseParts xs arguments uploads
		# filename			= getParam "filename" disposition
		| filename == ""	= parseParts xs (put name body arguments) uploads
		| otherwise			= parseParts xs arguments (put name { newHTTPUpload
														& upl_name		= name
														, upl_filename	= filename
														, upl_mimetype	= http_getValue "Content-Type" (fromList headers) ""
														, upl_content	= body
														} uploads)
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

//Construction of HTTP Response messages
http_makeResponse :: !HTTPRequest ![((String -> Bool),(HTTPRequest *st -> (HTTPResponse, *st)))] !Bool !*st -> (!HTTPResponse,!*st) | FileSystem st
http_makeResponse request [] fallback world 										//None of the request handlers matched
	= if fallback
		(http_staticResponse request world)											//Use the static response handler
		(http_notfoundResponse request world)										//Raise an error
http_makeResponse request [(pred,handler):rest] fallback world
	| (pred request.req_path)		= handler request world							//Apply handler function
									= http_makeResponse request rest fallback world	//Search the rest of the list

http_addDateHeaders	:: !HTTPResponse !*World -> (!HTTPResponse,!*World)
http_addDateHeaders rsp=:{rsp_headers} world
	# (tm,world) = gmTime world
	# now = format tm
	# rsp_headers = put "Date" now rsp_headers
	# rsp_headers = put "Last-Modified" now rsp_headers
	= ({rsp & rsp_headers = rsp_headers},world)
where
	//Format the current date/time
	format tm				=	(weekday tm.wday) +++ ", " +++ (toString tm.mday) +++ " " +++ (month tm.mon) +++ " " +++ (toString (tm.year + 1900)) +++ " "
								+++	(toString tm.hour) +++ ":" +++ (toString tm.min) +++ ":" +++ (toString tm.sec) +++ " GMT"
								
	weekday 0					= "Sun"
	weekday 1					= "Mon"
	weekday 2					= "Tue"
	weekday 3					= "Wed"
	weekday 4					= "Thu"
	weekday 5					= "Fri"
	weekday 6					= "Sat"
	
	month	0					= "Jan"
	month	1					= "Feb"
	month	2					= "Mar"
	month	3					= "Apr"
	month	4					= "May"
	month	5					= "Jun"
	month	6					= "Jul"
	month	7					= "Aug"
	month	8					= "Sep"
	month   9					= "Oct"
	month  10					= "Nov"
	month  11					= "Dec"
			
http_encodeResponse :: !HTTPResponse !Bool -> String
http_encodeResponse {rsp_headers = headers, rsp_data = data} withreply //When used directly the 'Status' header should be converted to 
	# reply = if withreply
			("HTTP/1.0 " +++ (http_getValue "Status" headers "200 OK") +++ "\r\n")
			("Status: " +++ (http_getValue "Status" headers "200 OK") +++ "\r\n")				
	# reply = reply +++ ("Server: " +++ (http_getValue "Server" headers "Clean HTTP 1.0 Server") +++ "\r\n")					//Server identifier	
	# reply = reply +++	("Content-Type: " +++ (http_getValue "Content-Type" headers "text/html") +++ "\r\n")					//Content type header
	# reply = reply +++	("Content-Length: " +++ (toString (size data)) +++ "\r\n")												//Content length header	
	# reply = reply +++	(foldr (+++) "" [(n +++ ": " +++ v +++ "\r\n") \\ (n,v) <- toList headers | not (skipHeader n)])		//Additional headers
	# reply = reply +++	"\r\n"																									//Separator
	# reply = reply +++	data																									//data
	= reply
where
	//Do not add these headers two times
	skipHeader s = isMember s ["Status","Server","Content-Type","Content-Length"]

	
//Error responses
http_notfoundResponse :: !HTTPRequest !*st -> (!HTTPResponse, !*st)
http_notfoundResponse req world = ({rsp_headers = put "Status" "404 Not Found" newMap, rsp_data = "404 - Not found"}, world)

http_forbiddenResponse :: !HTTPRequest !*st -> (!HTTPResponse, !*st)
http_forbiddenResponse req world = ({rsp_headers = put "Status" "403 Forbidden" newMap, rsp_data = "403 - Forbidden"}, world)

//Static content
http_staticResponse :: !HTTPRequest !*st -> (!HTTPResponse, !*st) | FileSystem st
http_staticResponse req world
	# filename				= req.req_path % (1, size req.req_path)		//Remove first slash
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	| not ok 				= http_notfoundResponse req world
							= ({rsp_headers = fromList [("Status","200 OK"),
											   			("Content-Type", type),
											   			("Content-Length", toString (size content))]
							   ,rsp_data = content}, world)						
							
http_staticFileContent :: !String !*st -> (!Bool, !String, !*st) | FileSystem st
http_staticFileContent filename world
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

http_staticFileMimeType :: !String !*st -> (!String, !*st)
http_staticFileMimeType ".jpg" world = ("image/jpeg",world)
http_staticFileMimeType ".png" world = ("image/png",world)
http_staticFileMimeType ".gif" world = ("image/gif",world)
http_staticFileMimeType ".bmp" world = ("image/bmp",world)

http_staticFileMimeType ".htm" world = ("text/html",world)
http_staticFileMimeType ".html" world = ("text/html",world)
http_staticFileMimeType ".txt" world = ("text/plain",world)
http_staticFileMimeType ".css" world = ("text/css",world)
http_staticFileMimeType ".js" world = ("text/javascript",world)
http_staticFileMimeType "" world = ("application/octet-stream",world)
http_staticFileMimeType name world = http_staticFileMimeType (name % (1, size name)) world

http_serverControl :: !HTTPResponse -> String
http_serverControl response
	= case (get "X-Server-Control" response.rsp_headers) of
		Just control	= control
		_				= ""
