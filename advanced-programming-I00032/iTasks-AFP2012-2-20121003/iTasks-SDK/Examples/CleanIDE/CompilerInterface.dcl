definition module CompilerInterface

import iTasks, AppState

:: CompilerException = CannotRunCompiler !String | CompilerErrors ![String]

derive class iTask CompilerException

compileToExe :: !(DBId AppState) -> Task Document