definition module GinStorage
 
import Error
import OSError
import Maybe
import Void
from Task import ::Task
from GinSyntax import ::GModule
from GinConfig import ::GinConfig

searchPathModules :: !GinConfig !*World -> ([String], *World)

readModule :: !GinConfig !String !*World -> (MaybeErrorString GModule, *World)
importModules :: !GinConfig ![String] !*World -> (MaybeErrorString [GModule], *World)
writeModule :: !GinConfig !String !GModule -> Task Void
newModuleName :: !GinConfig -> Task String
chooseModule :: !GinConfig -> Task (Maybe (!String, !GModule))

