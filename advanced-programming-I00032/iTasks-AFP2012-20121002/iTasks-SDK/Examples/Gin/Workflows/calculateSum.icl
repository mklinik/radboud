implementation module calculateSum
import StdInt, StdBool, StdString, StdList, StdOrdList, StdTuple, StdEnum, StdOverloaded, StdFile
import CommonCombinators, CoreCombinators, CoreTasks, InteractionTasks
newWorkflow :: Task Void
newWorkflow =
    enterInformation ("Enter a number") ([])
    >>= \num1 -> enterInformation ("Enter another number") ([])
    >>= \num2 -> showInformation ("The sum of those numbers is") ([]) (num1 + num2 + 0)
    >>| return (Void)