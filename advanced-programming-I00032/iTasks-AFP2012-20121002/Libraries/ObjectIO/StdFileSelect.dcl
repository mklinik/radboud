definition module StdFileSelect


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdFileSelect defines the standard file selector dialogue.
//	********************************************************************************


import StdMaybe, StdString

class FileSelectEnv env where
	selectInputFile ::                 !*env -> (!Maybe String,!*env)
	selectOutputFile:: !String !String !*env -> (!Maybe String,!*env)
	selectDirectory ::                 !*env -> (!Maybe String,!*env)
/*	selectInputFile
		opens a dialogue in which the user can browse the file system to select an 
		existing file. 
		If a file has been selected, the String result contains the complete 
		pathname of the selected file. 
		If the user has not selected a file, Nothing is returned.
	selectOutputFile
		opens a dialogue in which the user can browse the file system to save a 
		file.
		The first argument is the prompt of the dialogue (default: "Save As:")
		The second argument is the suggested filename. 
		If the indicated directory already contains a file with the indicated name, 
		selectOutputFile opens a new dialogue to confirm overwriting of the existing
		file.
		If either this dialogue is not confirmed or browsing is cancelled then 
		Nothing is returned, otherwise the String result is the complete pathname of
		the selected file.
	selectDirectory
		opens a dialogue in which the user can browse the file system to select a
		directory.
		If a directory has been selected, the String result contains the complete
		pathname of the selected directory.
		If the user has not selected a directory, Nothing is returned.
*/

instance FileSelectEnv World
