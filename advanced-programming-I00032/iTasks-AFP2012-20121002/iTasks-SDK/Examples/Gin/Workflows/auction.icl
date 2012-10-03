implementation module auction
import StdInt, StdBool, StdString, StdList, StdOrdList, StdTuple, StdEnum, StdOverloaded, StdFile
import CommonCombinators, CoreCombinators, CoreTasks, InteractionTasks
derive class iTask Bid
newWorkflow :: (Bid -> Task Bid) User [User] Bid ->   Task Bid
newWorkflow bidf auctioneer bidders current =
    let     f2 current = anyTask [(b @: bidf current) \\ b <- ([bidders:auctioneer]) | (True)]
                         >>= \newBid -> case (newBid.price > current.price) of
                                            True = (\current -> f2 current) (newBid)
                                            False = case (newBid.user == auctioneer) of
                                                        False = f2 current
                                                        True = return (current)
    in f2 current