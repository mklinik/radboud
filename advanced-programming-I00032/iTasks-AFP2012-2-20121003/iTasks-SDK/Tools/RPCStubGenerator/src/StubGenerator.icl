implementation module StubGenerator

import StdEnv
import JSON
import Directory
import RPC
import GenPrint

import StdDebug

derive gPrint RPCOperation, RPCInterface, RPCCallType, RPCParam, RPCMessageType, RPCProtocol, RPCParameterType, RPCHttpMethod

wStdOut :: !String !*World -> *World
wStdOut s world
	# (c,world)  = stdio world
	# c 		 = fwritec '\n' (fwrites ("> "+++s) c)
	# (ok,world) = fclose c world
	| not ok	 = abort "> (wStdOut) Cannot write to the console."
	= world

generateStubs :: !String !String !*World -> *World
generateStubs rd wd world
	# ((ok,rpath),world) 	= pd_StringToPath rd world
	| not ok 				= abort "Illegal directory format in read directory"
	# ((ok,wpath),world) 	= pd_StringToPath wd world
	| not ok 				= abort "Illegal directory format in write directory"
	# ((err,fi),world)		= getFileInfo rpath world
	| err <> NoDirError		= abort ("Cannot open read directory: "+++rd)
	# (err,world) 			= case getFileInfo wpath world of
		((DoesntExist,fileinfo),world) = createDirectory wpath world
		(_,world)					   = (NoDirError,world)
	| err <> NoDirError		= abort ("Cannot create write directory: "+++wd)
	# ((err,files),world)	= getDirectoryContents rpath world
	| err <> NoDirError		= abort ("Cannot read files from directory: "+++rd)
	# (rpcDescs, world)		= readFiles files [] rpath world
	# world = writeFiles rpcDescs wpath world writeICL
	# world = writeFiles rpcDescs wpath world writeDCL
	# world = writeWrapper rpcDescs wpath world
	= wStdOut "Done" world
	
readFiles :: ![DirEntry] [RPCDescription] !Path !*World -> ([RPCDescription],*World)
readFiles []     acc rpath world = (acc,world)
readFiles [x:xs] acc rpath world
	# (mbRpcDesc, world) = readFile x.fileName rpath world
	= case mbRpcDesc of
	(Just rpcd) = readFiles xs [rpcd:acc] rpath world
	Nothing
		# world = wStdOut ("Failed to parse "+++x.fileName) world
		= readFiles xs acc rpath world

readFile :: !String !Path !*World -> (Maybe RPCDescription, *World)
readFile fn rpath world
	# (dir,world) = pathToPD_String rpath world
	| ext fn <> ".idl" = (Nothing, world)
	# (ok,file,world)  = fopen (dir+++"\\"+++fn) FReadText world
	= case ok of
		True
			# world			  	= wStdOut ("Reading "+++dir+++"\\"+++fn) world			
			# (mbRPCDesc,file)	= readContents file
			# (ok,world)	  	= fclose file world
			= (mbRPCDesc,world)
		False
			= (Nothing, wStdOut ("Cannot read "+++dir+++"\\"+++fn+++". Ignoring") world)
where
	ext fn = fn % ((size fn)-4,size fn)

readContents :: !*File -> (Maybe RPCDescription, *File)
readContents file
	# (content,file) = readWholeFile "" file
	= (fromJSON (fromString content),file)
where
	readWholeFile acc file
	#(end,file) = fend file
	| not end
		# (line,file) = freadline file
		= readWholeFile (acc+++line) file
	| otherwise = (acc,file)

writeFiles :: ![RPCDescription] !Path !*World (RPCDescription Path *World -> *World) -> *World
writeFiles []     wpath world parsefun = world
writeFiles [x:xs] wpath world parsefun = writeFiles xs wpath (parsefun x wpath world) parsefun

writeICL :: !RPCDescription !Path !*World -> *World
writeICL rpcd wpath world
	# name 				= (prepName rpcd.service.RPCService.name)
	# (fname,world)		= concatFilename wpath name ".icl" world
	# (ok,file,world) 	= fopen fname FWriteText world
	| not ok 			= abort  ("Cannot open file "+++fname)
	# file				= fwrites ("implementation module "+++name+++"\n\nimport JSON\nimport RPC\nimport TSt\n\n") file	
	//write derives if necessary. With only Int, Real, Bool and String this is not yet necessary
	# file				= writeOperationFunctions rpcd.RPCDescription.operations rpcd.RPCDescription.interface file
	# (ok,world) 		= fclose file world
	| not ok 			= abort ("Cannot close file "+++fname)
	= world
	
writeOperationFunctions :: [RPCOperation] RPCInterface *File -> *File
writeOperationFunctions [] _ f = f
writeOperationFunctions [x:xs] rpci f = writeOperationFunctions xs rpci (fwrites "\n\n" (writeOperationFunction x rpci f))

writeOperationFunction :: RPCOperation RPCInterface *File -> *File
writeOperationFunction rpco rpci f
	# f = fwrites "\n" (writeOperationDefinition rpco f)
	# f = fwrites ((prepName rpco.RPCOperation.name)+++" ") f
	# f = writeparams rpco.parameters f
	# f = fwrites ("parsefun = mkRpcTask \""+++rpco.RPCOperation.name+++"\"\n") f
	# f = fwrites  "\t{ RPCExecute\n" f
	# f = fwrites  "\t| taskId = \"\"\n" f
	# f = fwrites ("\t, interface = "+++(printToString rpci)+++"\n") f
	# f = fwrites ("\t, operation = "+++(printToString rpco)+++"\n") f
	# f = fwrites ("\t, paramValues = ["+++(pval2str rpco.RPCOperation.parameters)+++"]\n") f
	# f = fwrites  "\t, status = \"\"\n" f
	# f = fwrites  "\t} parsefun" f
	= f

where
	writeparams []     f = f
	writeparams [x:xs] f = writeparams xs (fwrites (x.RPCParam.name+++" ") f)

	pval2str [] = ""
	pval2str [x] = toString x
	pval2str [x:xs] = (toString x)+++","+++(pval2str xs)

instance toString RPCParam
where
	toString x = "{ RPCParamValue | name=\""+++x.RPCParam.name+++"\", serializedValue = toString "+++x.RPCParam.name+++"}"
	
writeDCL :: !RPCDescription !Path !*World -> *World
writeDCL rpcd wpath world
	# name 				= (prepName rpcd.service.RPCService.name)
	# (fname,world)		= concatFilename wpath name ".dcl" world
	# (ok,file,world) 	= fopen fname FWriteText world
	| not ok 			= abort  ("Cannot open file "+++fname)
	# file				= fwrites ("definition module "+++name+++"\n\nimport iTasks\n\n") file	
	# file				= writeOperationDefinitions rpcd.operations file
	# (ok,world) 		= fclose file world
	| not ok 			= abort ("Cannot close file "+++fname)
	= world

writeOperationDefinitions :: ![RPCOperation] ! *File -> *File
writeOperationDefinitions [] 	 f = f
writeOperationDefinitions [x:xs] f = writeOperationDefinitions xs (fwrites "\n\n" (writeOperationDefinition x f)) 	
	
writeOperationDefinition :: !RPCOperation !*File -> *File
writeOperationDefinition rpco f
	// <name> :: [<paramType>] (String-><returnType>) -> Task <returnType>
	# f = fwrites (prepName rpco.RPCOperation.name) f
	# f = fwrites " :: " f
	# f = writeparams rpco.parameters f
	# f = fwrites ("(String -> "+++rpco.returnType+++") -> Task "+++rpco.returnType) f
	= f
where
	writeparams [] f = f
	writeparams [x:xs] f = writeparams xs (writeparam x.RPCParam.type f)
	
	writeparam RPCString f = fwrites "String " f
	writeparam RPCBool   f = fwrites "Bool " f
	writeparam RPCInt    f = fwrites "Int " f
	writeparam RPCReal   f = fwrites "Real " f

writeWrapper :: ![RPCDescription] !Path !*World -> *World
writeWrapper rpcds wpath world
	# (fname,world)		= concatFilename wpath "RPCStubs" ".dcl" world
	# (ok,file,world) 	= fopen fname FWriteText world
	| not ok			= abort ("Cannot open file "+++ fname)
	# file				= fwrites("definition module RPCStubs\n\n") file
	# file				= writeImports rpcds file
	# (ok,world)		= fclose file world
	# (fname,world)		= concatFilename wpath "RPCStubs" ".icl" world
	# (ok,file,world)	= fopen fname FWriteText world
	| not ok			= abort ("Cannot open file "+++fname)
	# file				= fwrites("implementation module RPCStubs") file;
	# (ok,world)		= fclose file world
	= world	
where
	writeImports []     f = f
	writeImports [x:xs] f = writeImports xs ( fwrites ("import "+++(prepName x.service.RPCService.name)+++"\n") f)

//=== UTILITY =====================================================================

concatFilename :: !Path !String !String !*World -> (String,*World)
concatFilename path fname suffix world
# (dir,world) = (pathToPD_String path world)
= (dir+++"\\"+++fname+++suffix,world)

//Change spaces to underscores and removes Uppercase
prepName :: String -> String
prepName str = { (change c) \\ c <-: str}
where
	change c
	| isSpace c = '_'
	| otherwise = toLower c
	
	
//=== START =======================================================================
Start :: !*World -> *World
Start world 
	# (console,world) = stdio world
	# console		  = fwrites "Input Directory [RPCSDefs] > " console
	# (rdir,console)  = freadline console
	# rdir			  = case size rdir of
							1 = "RPCDefs"
							_ = rdir % (0,((size rdir)-2))
	# console		  = fwrites "Output Directory [RPCStubs] > " console
	# (wdir,console)  = freadline console
	# wdir			  = case size wdir of 
							1 = "RPCStubs"
							_ = wdir % (0,((size wdir)-2))
	# (ok,world)	  = fclose console world	
	= generateStubs rdir wdir world 