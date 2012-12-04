definition module streamTasks

import iTasks
from StdMisc import abort
from StdFunc import o

derive class iTask	Stream	


:: Stream a 

//= ES
//			| S a (Task (Stream a))

:: StreamFun a b :== (Task (Stream a)) -> Task (Stream b)

/**
* Binds two stream together
*
* @param First stream
* @param Second stream
*
* @return The combined stream
*/
(|>) infixl 9 :: a (a -> b) -> b

/**
* generator lazily drops list elements one by one into a stream
*
* @param The list 
*
* @return The list elements in the stream
*/
generator :: [a] -> Task (Stream a) | iTask a

/**
* sink shows the stream elements it receives one by one and collects them in a list again
*
* @param The stream 
*
* @return All the received elements (does not terminate for infinite streams)
*/
sink :: (Task (Stream a)) -> Task [a] | iTask a 

/**
* filterS only lets elements pass which obey the predicate
*
* @return Stream with elements obeying the predicate
*/
filterS :: (a -> Bool) -> StreamFun a a  | iTask a 

/**
* toList collects n elements and puts them into a list in the resulting stream
*
* @param The number of elements to collect 
*
* @return Stream with list of n elements, maybe <= n for last elements in stream
*/
toList :: Int -> StreamFun a [a]  | iTask a 

/**
* fromList takes alist and drops them one by one into the resulting stream
*
* @param The list stream
*
* @return The stream with the list elements 
*/
fromList :: StreamFun [a] a   | iTask a 

/**
* mapFun duplicates an element in a list
*
* @param The number of elements to collect 
*
* @return Stream with list of n elements
*/
mapFun :: (a -> b) -> StreamFun a b  | iTask a & iTask b

/**
* mapS applies the task-i to element-i (modulo number of tasks) in the stream (sequential one by one) 
*
* @param The n tasks to apply one by one in a round robin fashion to the next n elements in the stream 
*
* @return Stream with mapped elements
*/
mapS :: [a -> Task b] -> StreamFun a b  | iTask a & iTask b

/**
* mapP applies the task-i to element-i (modulo number of tasks) in the stream in parallel 
*
* @param The n tasks to apply in parallel to the next n elements in the stream 
*
* @return Stream with mapped elements
*/

mapP :: [a -> Task b] -> StreamFun a b  | iTask a & iTask b
		
/**
* splitS takes a stream and produces two streams
* those elements obeying the predicate go into first, the others in the second stream
* *
* @param The predicate
* @param The stream functions for the first stream
* @param The stream functions for the second stream
*
* @return A stream with two streams 
*/
splitS :: (a -> Bool) (StreamFun a b) (StreamFun a c) 
				(Task (Stream a)) -> Task (Task (Stream b),Task (Stream c)) | iTask a & iTask b & iTask c

/**
* joinS takes two streams and joins them into one, order is depending on the order in which the streams deliver items
* *
*
* @return A stream in which the elements of the two streams are combined
*/
joinS :: (Task (Task (Stream a),Task (Stream b))) -> Task (Stream (Either a b)) | iTask a & iTask b

:: DynPipe a = DP (a -> (StreamFun a a, Maybe a, Maybe (DynPipe a)))

/**
* pipeline takes pipeline function delivering a stream function, a value (optional) and a next pipeline function (optional)
* and extends the current stream with the pipeline function, yielding the value 
* *
*
* @return A stream extended with the pipeline function
*/
pipeline :: (DynPipe a) -> (StreamFun a a)  | iTask a & toString a

