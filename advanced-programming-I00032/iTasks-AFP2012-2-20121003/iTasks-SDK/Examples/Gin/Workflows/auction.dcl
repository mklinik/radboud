definition module auction
import StdInt, StdBool, StdString, StdList, StdOrdList, StdTuple, StdEnum, StdOverloaded, StdFile
import CommonCombinators, CoreCombinators, CoreTasks, InteractionTasks
:: Bid = {user :: User, ware :: String, price :: Int}
derive class iTask Bid
/**
* newWorkflow
*
* @param bidf
* @param auctioneer
* @param bidders
* @param current

*/
newWorkflow :: (Bid -> Task Bid) User [User] Bid ->   Task Bid