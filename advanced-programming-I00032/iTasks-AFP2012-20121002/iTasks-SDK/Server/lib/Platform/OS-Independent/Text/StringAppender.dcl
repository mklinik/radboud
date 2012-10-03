definition module StringAppender

/**
* This module provides an iterface for a "string output stream" like
* data structure to efficiently concatenate values as strings.
*/

import StdString

:: StringAppender

/**
* Initializes a new appender.
* @return A StringAppender instance
*/
newAppender :: StringAppender	

/**
* Put an arbitrary value (for which toString has an instance) to the end of the stream.
* @param Output stream (StringAppender instance)
* @param The value to be printed
* @return A StringAppender instance
*/
append :: StringAppender a -> StringAppender | toString a

/**
* Append a list of values to the output stream using a separator string between the elements
* @param Separator string
* @param The list of values
* @param Output stream (StringAppender instance) 
* @return A StringAppender instance
*/
joinList :: !String [a] StringAppender -> StringAppender | toString a

/**
* Combinator to easily append values of different type to the stream.
* (And some basic instances)
*/

instance toString StringAppender	

class Appendable a
where
	(<++) infixl 1 :: StringAppender a -> StringAppender

instance Appendable String
instance Appendable Int
instance Appendable Real
instance Appendable (StringAppender -> StringAppender)
