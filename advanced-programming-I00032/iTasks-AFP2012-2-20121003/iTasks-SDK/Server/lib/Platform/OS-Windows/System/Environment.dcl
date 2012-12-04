definition module Environment
/**
* Module for accessing environment variables
*/
import Maybe

getEnvironmentVariable :: !String !*World -> (Maybe String, *World)

setEnvironmentVariable :: !String !String !*World -> *World

unsetEnvironmentVariable :: !String !*World -> *World