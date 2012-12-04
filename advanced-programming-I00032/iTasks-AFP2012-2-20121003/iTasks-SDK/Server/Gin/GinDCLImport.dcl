definition module GinDCLImport

from GinSyntax import ::GModule
from GinTypes import ::GTypeExpression
from syntax import ::Type
import Error

importDCL :: !String !String *World -> (MaybeErrorString GModule, *World)

mapType :: Type -> GTypeExpression
