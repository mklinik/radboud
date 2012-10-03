implementation module webauction
import StdInt, StdBool, StdString, StdList, StdOrdList, StdTuple, StdEnum, StdOverloaded, StdFile
import CommonCombinators, CoreCombinators, CoreTasks, InteractionTasks
derive class iTask Bid
webAuction :: User [User] Bid ->   Task Bid
webAuction auctioneer bidders current =
    let     f2 current = anyTask [(b)
                         @: updateInformation ("Make your bid") ([]) ({Bid | current & user = b}) \\ b <-
                         ([auctioneer:bidders]) | (True)]
                         >>= \newBid -> case (newBid.price > current.price) of
                                            True = (\current -> f2 current) (newBid)
                                            False = case (newBid.Bid.user == auctioneer) of
                                                        False = f2 current
                                                        True = return (current)
    in f2 current