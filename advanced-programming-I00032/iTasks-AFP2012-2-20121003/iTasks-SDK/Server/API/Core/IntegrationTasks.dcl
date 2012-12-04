definition module IntegrationTasks
/**
* This module provides tasks for interaction with other systems.
*/

from Maybe	import :: Maybe
from Void	import :: Void
from Error	import :: MaybeError, :: MaybeErrorString

from Task 			import :: Task
from SystemTypes	import :: Note, :: EmailAddress
from InteractionTasks	import :: ViewOption
import iTaskClass

:: HTTPMethod = GET | POST

/**
* Call a function that interacts with the world
*
* @param The function to call
*/
worldIO :: (*World -> *(!MaybeError e a,!*World)) -> Task a | iTask a & TC e

/**
* Calls an external executable. The call is non-blocking.
*
* @param Task description
* @param Executable: path to the executable
* @param Arguments: a list of command-line arguments
* @return return-code of the process
* @throws CallException
* 
* @gin-title Start executable
* @gin-icon executable
*/
callProcess :: !d ![ViewOption ProcessStatus] !FilePath ![String] -> Task ProcessStatus | descr d

/**
* Calls an external executable. This call blocks task computation, only use when process is known to terminate fast.
* @param Executable: path to the executable
* @param Arguments: a list of command-line arguments
* @return return-code of the process
* @throws CallException
*/
callInstantProcess :: !FilePath ![String] -> Task Int

/**
* Calls an external HTTP webservice.
*
* @param HTTP Method: the HTTP method (GET or POST) to use
* @param URL: The URL of the webservice
* @param Parameters: A list of name/value pairs
* @param Response handler: A parse function that parses the response
* 
* @return A shared reference in which the response will be stored
* 
* @gin-title Call web service
* @gin-icon webservice
*/
callHTTP	:: !HTTPMethod !String !String !(String -> (MaybeErrorString b)) -> Task b | iTask b	
callRPCHTTP :: !HTTPMethod !String ![(String,String)] !(String -> a) -> Task a | iTask a


withTemporaryDirectory :: (FilePath -> Task a) -> Task a | iTask a

/**
* Send an e-mail message.
*
* @param Subject: The subject line of the e-mail
* @param Body: The body of the e-mail
* @param Sender: The sender address
* @param Recipients: The list of recipients
*
* @return The recipients to which the email was sent

* @gin-title Send e-mail
* @gin-icon email
*/
sendEmail :: !String !Note !sndr ![rcpt] -> Task [EmailAddress] | toEmail sndr & toEmail rcpt
