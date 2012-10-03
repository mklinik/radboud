implementation module ExportTasks

import StdBool, FilePath, CSV, File, Map, IWorld, Task, TaskState, TaskStore

exportDocument :: !FilePath !Document -> Task Document
exportDocument filename document = mkInstantTask eval
where
	eval taskId iworld = writeDocument taskId filename document iworld
	
exportTextFile :: !FilePath !String -> Task String
exportTextFile filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content writeAll iworld

createCSVFile :: !String ![[String]] -> Task Document
createCSVFile filename content = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (mbDoc,iworld)	= createDocumentWith filename "text/csv" (writeCSVFile content) iworld
		= case mbDoc of
			Ok doc	= (Ok doc, iworld)
			_	
				# e = "Failed to create csv file"
				= (Error (dynamic e,e),iworld)
			
exportCSVFile :: !FilePath ![[String]] -> Task [[String]]
exportCSVFile filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content writeCSVFile iworld

exportCSVFileWith :: !Char !Char !Char !FilePath ![[String]] -> Task [[String]]
exportCSVFileWith delimitChar quoteChar escapeChar filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content (writeCSVFileWith delimitChar quoteChar escapeChar) iworld

exportJSONFile :: !FilePath a -> Task a | iTask a
exportJSONFile filename content = exportJSONFileWith toJSON filename content
 
exportJSONFileWith :: !(a -> JSONNode) !FilePath a -> Task a | iTask a
exportJSONFileWith encoder filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content (writeJSON encoder) iworld

fileTask taskId filename content f iworld=:{IWorld|taskTime,world}
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# file				= f content file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (Ok content, {IWorld|iworld & world = world})
	
writeAll content file
	= fwrites content file

writeJSON encoder content file
	= fwrites (toString (encoder content)) file

writeDocument taskId filename document iworld
	# (mbContent,iworld=:{IWorld|taskTime,world})
							= loadDocumentContent document.Document.documentId iworld
	| isNothing mbContent	= (ioException filename, {IWorld|iworld & world = world})
	# (ok,file,world)		= fopen filename FWriteData world
	| not ok				= (openException filename,{IWorld|iworld & world = world})
	# file					= fwrites (fromJust mbContent) file
	# (ok,world)			= fclose file world
	| not ok				= (closeException filename,{IWorld|iworld & world = world})	
	= (Ok document, {IWorld|iworld & world = world})

ioException s
	# e = FileException s IOError
	= Error (dynamic e, toString e)
openException s	
	# e = FileException s CannotOpen
	= Error (dynamic e, toString e)
closeException s
	# e = FileException s CannotClose
	= Error (dynamic e, toString e)
