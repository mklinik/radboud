implementation module webauctionDemo
import StdInt, StdBool, StdString, StdList, StdOrdList, StdTuple, StdEnum, StdOverloaded, StdFile
import CommonCombinators, CoreCombinators, CoreTasks, InteractionTasks, SystemData, SystemTypes, webauction
newWorkflow :: Task Void
newWorkflow =
    get (users)
    >>= \allUsers -> webAuction (RootUser) (allUsers)
    ({ ware = "Pendulum clock", price = 1, user = RootUser }   )
    >>= \result -> showInformation ("Auction result") ([]) (result)
    >>| return (Void)