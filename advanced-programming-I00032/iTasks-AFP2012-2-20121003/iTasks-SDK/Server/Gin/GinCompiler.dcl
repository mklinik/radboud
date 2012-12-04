definition module GinCompiler

import iTasks
from GinSyntax import ::GModule
from GinParser import ::GPath, ::GPathNode

:: CompileResult a = CompileSuccess a | CompileGlobalError String | CompilePathError [(GPath,String)]

derive class iTask CompileResult

batchBuild :: !GModule *IWorld -> (CompileResult String, *IWorld)

syntaxCheck :: !GModule *IWorld -> (CompileResult Void, *IWorld)

exitCompiler :: *IWorld -> *IWorld

