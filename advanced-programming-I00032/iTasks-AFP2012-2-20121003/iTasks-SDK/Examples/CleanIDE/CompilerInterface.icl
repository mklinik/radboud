implementation module CompilerInterface

import iTasks, Text
import Configuration

derive class iTask	CompilerException
derive bimap		Maybe, (,)

compileToExe :: !(DBId AppState) -> Task Document
compileToExe sid
	# compileToExe` = try compileToExe` handleCallException
	# compileToExe` = try compileToExe` handleReadLogException
	# compileToExe` = try compileToExe` handleStringExceptions
	= compileToExe`
where
	compileToExe` =
						getConfig sid
		>>= \config.	getAppPath
		>>= \appPath.	pathToPDString config.projectsPath
		>>= \prjPath.	callProcess "building project..." config.oldIDEPath ["--batch-build \"" +++ appPath +++ prjPath +++ "\\test\\test.prj\""]
		>>= \ret.		case ret of
							0	= 					importDocument (prjPath +++ "\\test\\test.exe")
									>>=				return
							_	=					readTextFile (config.projectsPath +< [PathDown "test", PathDown "test.log"])
									>>= \log.		throw (CompilerErrors (filter ((<>) "") (split "\n" log)))
									
	handleCallException (CallFailed path)			= throw (CannotRunCompiler ("Error creating process '" +++ path +++ "'"))
	handleReadLogException (FileException path _)	= throw (CannotRunCompiler ("Unable to retrieve compiler errors from '" +++ path +++ "'"))
	handleStringExceptions str						= throw (CannotRunCompiler str)