definition module WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from HTTP	import :: HTTPRequest, :: HTTPResponse
from Engine	import :: ServiceFormat
from IWorld	import :: IWorld
import iTaskClass

webService :: !(HTTPRequest -> Task a) !ServiceFormat !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld) | iTask a