definition module webauction
import StdInt, StdBool, StdString, StdList, StdOrdList, StdTuple, StdEnum, StdOverloaded, StdFile
import CommonCombinators, CoreCombinators, CoreTasks, InteractionTasks
:: Bid = {user :: User, ware :: String, price :: Int}
derive class iTask Bid
/**
* webAuction
*
* @param auctioneer
* @param bidders
* @param current

*/
webAuction :: User [User] Bid ->   Task Bid