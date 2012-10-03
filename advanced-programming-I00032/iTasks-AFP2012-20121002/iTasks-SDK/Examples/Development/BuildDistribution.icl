module BuildDistribution
/**
* This tool automates the task of creating a zip package of the iTask System.
* Not all steps can be automated, so there are some manual steps involved.
*
* It is experimental and incomplete... (but beats making packages completely by hand...)
*/
import iTasks
import Directory, File, Tuple
from Util import pad

:: Platform		= Windows32 | Windows64 | Linux32 | Linux64 | Mac

:: DistroOptions =
	{ exportPath	:: FilePath
	, platform		:: Platform
	, branch		:: String		//Which branch
	}

derive class iTask Platform, DistroOptions

makeDistribution :: Task FilePath
makeDistribution
	=	editOptions
	>>= buildDistro

editOptions :: Task DistroOptions
editOptions
	=		viewTitle "Build an iTasks distribution"
	||-		updateInformation ("Export path","Specify where you want to export the distribution") [] "."
	-&&-	updateChoice ("Target platform","Choose a target platform to make a distribution for") []
				[Windows32,Windows64,Linux32,Linux64,Mac] Windows32
	-&&-	updateInformation ("Svn branch","Choose which svn branch you want to make a distribution from") [] "trunk"	
	@ 	\(path,(platform,branch)) ->
			{exportPath=path,platform=platform,branch=branch}

buildDistro:: DistroOptions -> Task FilePath
buildDistro options
	=	createTargetFolder options.exportPath
	>>=	\target ->
		addCleanSystem options.platform True target
	>>| addITasksSDK options.branch target
	>>| applyPatches options.platform target
	>>| cleanupDistro options.platform target
	>>| zipDistro options.exportPath target
	>>|	viewInformation ("Done","You can find your distribution in the following location") [] target
where
	//Create datestamped folder
	createTargetFolder exportPath 
		=	get currentDate
		>>= \date -> 
			let target = (exportPath </> ("iTasks-dist-" <+++ pad 4 date.year <+++ pad 2 date.mon <+++ pad 2 date.day)) in 
					checkDirectory target
				>>= \exists -> if exists
					(return target)
					(worldIO (createDirectory target) @ const target)

	//Download and add Clean system, remove unnecessary files and libraries
	addCleanSystem platform include target
		=	checkFile zipFile
		>>= \exists -> if exists
			(return zipFile)
			(	callHTTP GET (downloadUrl platform) "" Ok
				>>= \content ->
				exportTextFile zipFile content	//UGLY: We should not have to pass the data through here...
			)
		>>| checkDirectory zipTarget
		>>= \exists -> if exists
			(return zipTarget)
			(callProcess "Unzipping Clean" [] zipExe zipArgs @ const zipTarget)
	where
		zipFile	= target </> "Clean_2.4.zip"
		zipTarget = target </> "Clean 2.4"
		downloadUrl _ = "http://clean.cs.ru.nl/download/Clean24/windows/Clean_2.4.zip"
	
		zipExe = IF_POSIX_OR_WINDOWS "/usr/bin/unzip" "C:\\Program Files\\7-Zip\\7z.exe"
		zipArgs = IF_POSIX_OR_WINDOWS ["-q", zipFile,"-d", target] ["-o"+++target,"x", zipFile]
		
	//Export iTasks SDK from subversion
	addITasksSDK branch target
		=	checkDirectory svnTarget
		>>=	\exists -> if exists
			(return svnTarget)
			(callProcess "Exporting iTasks from subversion" [] svnExe svnArgs @ const svnTarget)
	where
		svnTarget = target </> "Clean 2.4" </> "iTasks-SDK"
		
		svnUrl	= "https://svn.cs.ru.nl/repos/iTask-system/" +++ branch
		svnExe = IF_POSIX_OR_WINDOWS "/usr/bin/svn" "C:\\Program Files\\Subversion\\bin\\svn.exe"
		svnArgs = ["export","--native-eol","CRLF",svnUrl,svnTarget]
	
	//Apply patches
	applyPatches platform target
		=	viewInformation "TEST" [] (base</>"iTasks-SDK"</>"Compiler"</>"StdGeneric.dcl")
		>>| patchLibraries
		>>| addITasksEnvironment
		>>| setDefaultHeapSize
	where
		base = target </> "Clean 2.4"
		
		patchLibraries = viewInformation (Title "Patch libraries") []
			(SpanTag [] [Text "Copy the following files:",BrTag [],
				UlTag [] [LiTag [] [Text "From ",BTag [] [Text fromfile], Text " to ",BTag [] [Text tofile]] \\ (fromfile,tofile) <- patches]
				])
		where
			patches = [(base</>"iTasks-SDK"</>"Compiler"</>"StdGeneric.dcl",base</>"Libraries"</>"StdEnv"</>"StdGeneric.dcl")
					  ,(base</>"iTasks-SDK"</>"Compiler"</>"StdGeneric.icl",base</>"Libraries"</>"StdEnv"</>"StdGeneric.icl")
					  ,(base</>"iTasks-SDK"</>"Compiler"</>"TCPChannels.dcl",base</>"Libraries"</>"TCPIP"</>"TCPChannels.dcl")
					  ,(base</>"iTasks-SDK"</>"Compiler"</>"TCPChannels.icl",base</>"Libraries"</>"TCPIP"</>"TCPChannels.icl")
					  ]
			
		addITasksEnvironment = viewInformation ("Add iTasks environment to IDE","TEST") []
			"Add the iTasks environment to the clean IDE"

		setDefaultHeapSize = viewInformation (Title "Set default heap size") []
			"Set the default heap size of the IDE to 16M (16777216 bytes)" 

	//Remove files not required for the target platform
	cleanupDistro platform target 
		= viewInformation ("Cleanup",
			"Remove all files unneccessary for the " <+++ platform <+++ " platform in " <+++ target)
			[] Void
	//Create a zipped version 
	zipDistro exportPath target
		= viewInformation ("Zip distro", "Create a zip archive of the following folder") [] target
		//= callInstantProcess "/usr/bin/zip" ["-j","-r",exportPath</>addExtension (dropDirectory target) "zip",target]

Start :: *World -> *World
Start world = startEngine makeDistribution world

derive class iTask FileInfo, Tm

checkDirectory :: FilePath -> Task Bool
checkDirectory path
	= catchAll (worldIO (getFileInfo path) @ \info -> info.FileInfo.directory) (\_ -> return False)
	
checkFile :: FilePath -> Task Bool
checkFile path = worldIO exists 
where
	exists:: *World -> (MaybeErrorString Bool, *World)
	exists world = appFst Ok (fileExists path world)
