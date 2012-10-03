definition module SQLDatabase
/**
* This experimental extension provides tasks and shares
* for interacting with a relational database.
*
* It provides only mimimal functionality and currently only works with MySQL...
*/
import iTasks, SQL
derive class iTask SQLValue, SQLTime, SQLDate

/**
* Run a single query and fetch all results
*/
sqlExecute	:: SQLDatabase SQLStatement ![SQLValue] -> Task [SQLRow]

/** 
* Read only query that is run each time the share is read.
*/
sqlShare	:: SQLDatabase SQLStatement ![SQLValue] -> ReadOnlyShared [SQLRow]

