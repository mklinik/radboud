implementation module tmfile


import	StdArray, StdBool, StdChar, StdClass, StdFile, StdInt, StdList, StdString
from	StdSystem	import dirseparator
from	tm			import :: Turing{..}, :: State, :: Tape{..}, :: Transition{..}, :: Head

    
DummyTuring		:== {transitions=[],tape=DummyTape,state=""}
DummyTape		:== {content="",head=0}
DummyTrans		:== {start="",sigma=' ',end="",move=' '}


//	Write a Turing Machine to a file.
WriteTuringToFile :: Turing !String !*env -> (!Bool,!*env) | FileSystem env
WriteTuringToFile turing fname env
	# (success,file,env)	= fopen fname FWriteText env
	| not success
		= (False,env)
	| otherwise
		# (_,env)			= fclose (file<<<turing) env
		= (True,env)

instance <<< Turing where
	(<<<) :: !*File !Turing -> *File
	(<<<) file {transitions,tape} = file<<<transitions<<<tape

instance <<< Transition where
	(<<<) :: !*File !Transition -> *File
	(<<<) file {start,sigma,end,move}
		  = file<<<String4 start<<<' '<<<sigma<<<"  ->  "<<<String4 end<<<' '<<<move<<<'\n'
		  where
			String4 :: !String -> String
			String4 str
				| len>=4	= str%(0,3)
				| otherwise	= str+++"    "%(0,3-len) 
			where
				len			= size str

instance <<< Tape where
	(<<<) :: !*File !Tape -> *File
	(<<<) file {content}
		  = file<<<"Tape:\n"<<<LimitContents content
		  where
			LimitContents :: !String -> String
			LimitContents cont
				| first>last	= "##"
				| fgood && lgood= cont % (first-1, last+1)
				| lgood			= cont % (0, last+1)
				| fgood			= cont % (first-1, lmin1)
				| otherwise		= cont
			where
				first			= FirstNonEmpty 0 lmin1 cont
				last			= LastNonEmpty lmin1 cont
				fgood			= first>0
				lgood			= last<lmin1
				lmin1			= size cont-1
				
				FirstNonEmpty :: !Int !Int String -> Int
				FirstNonEmpty i len str
					| i>len || str.[i]<>'#'	= i
					| otherwise				= FirstNonEmpty (i+1) len str
				
				LastNonEmpty :: !Int String -> Int
				LastNonEmpty i str
					| i<0 || str.[i]<>'#'	= i
					| otherwise				= LastNonEmpty (i-1) str

instance <<< [x] | <<< x where
	(<<<) :: !*File ![x] -> *File | <<< x
	(<<<) file [x:xs]	= file<<<x<<<xs
	(<<<) file []		= file

//	Read a Turing Machine from a file
ReadTuring :: !String !*env -> (!(!Int,!Turing),!*env) | FileSystem env
ReadTuring filename env
	# (success,file,env)		= fopen filename FReadText env
	| not success
		= ((-2,DummyTuring),env)
	| otherwise
		# (linenr,turing,file)	= ReadTuringFile file
		  (_,env)				= fclose file env
		= ((linenr,turing),env)
where
	ReadTuringFile :: !*File -> (!Int,!Turing,!*File)
	ReadTuringFile file
		# (linenr,trs,file)	= ReadTransitions 1 file
		| linenr<>0
			= (linenr, DummyTuring, file)
		| otherwise
			# (cont,file)	= ReadTape file
			= (linenr, {transitions=trs,tape={content=cont,head=size cont-1},state="S"},file)
	where
		ReadTape :: !*File -> (!String,!*File)
		ReadTape file
			# (line,file)				= freadline file
			| line==""					= ("##",file)
			# first						= line.[0]
			| first<>'|' && first<>'\n'	= (ParseTape 0 (size line) line,file)
			| otherwise					= ReadTape file
		where
			ParseTape :: !Int !Int !String -> String
			ParseTape i l s
				| i>=l							= s
				| c==' ' || c=='|' || c=='\n'	= s%(0,i-1)
				| otherwise						= ParseTape (i+1) l s
			where
				c	= s.[i]
		
		ReadTransitions :: Int !*File -> (!Int,![Transition],!*File)
		ReadTransitions linenr file
			| sfend file				= (-1,[],file)
			# (line,file)				= freadline file
			  (error,tape,comment,trans)= ParseLine line
			| error						= (linenr,[],file)
			| tape						= (0,[],file)
			# (lnr,rest,file)			= ReadTransitions (linenr+1) file
			| comment					= (lnr,rest,file)
			| otherwise					= (lnr,[trans:rest],file)
		where
			ParseLine :: !String -> (!Bool,!Bool,!Bool,!Transition)
			ParseLine s
				| s%(0,3)=="Tape"			= (False,True, False,DummyTrans)
				| first=='|' || first=='\n'	= (False,False,True, DummyTrans)
				| otherwise					= (error,False,False,trans)
			where
				(error,trans)				= ParseTransition s
				first						= s.[0]
				
				ParseTransition :: !String -> (!Bool,!Transition)
				ParseTransition s
					# i					= SkipLayout 0 len s
					  (error,start,i)	= ParseState   i i len s
					| error				= (True, DummyTrans)
					# (error,i)			= DemandLayout i i len s
					| error				= (True, DummyTrans)
					# (error,sigma,i)	= ParseHead    i    len s
					| error				= (True, DummyTrans)
					# (error,i)			= DemandLayout i i len s
					| error				= (True, DummyTrans)
					# (error,end,i)		= ParseState   i i len s
					| error				= (True, DummyTrans)
					# (error,i)			= DemandLayout i i len s
					| error				= (True, DummyTrans)
					# (error,move,i)	= ParseHead    i    len s
					| error				= (True, DummyTrans)
					| otherwise			= (False,{start=start,sigma=sigma,end=end,move=move})
				where
					len					= size s
				
				ParseState :: Int !Int !Int String -> (!Bool,!State,!Int)
				ParseState b i l s
					| i>=l || i-b>4 || (is_layout && i==b)	= (True,"",0)
					| is_layout && i>b						= (False, s%(b,i-1),i)
					| otherwise								= ParseState b (i+1) l s
				where
					is_layout								= IsLayoutChar i s
				
				ParseHead :: !Int !Int String -> (!Bool,!Char,!Int)
				ParseHead i l s
					| i>=l || IsLayoutChar i s			= (True,' ',0)
					| otherwise							= (False,s.[i],i+1)
				
				DemandLayout :: Int !Int !Int String -> (!Bool,!Int)
				DemandLayout b i l s
					| i>=l || (is_no_layout && i==b)	= (True ,0)
					| is_no_layout && i>b				= (False,i)
					| otherwise							= DemandLayout b (i+1) l s
				where
					is_no_layout						= not (IsLayoutChar i s)
				
				SkipLayout :: !Int !Int String -> Int
				SkipLayout i l s
					| i>=l				= i-1
					| IsLayoutChar i s	= SkipLayout (i+1) l s
					| otherwise			= i
				
				IsLayoutChar :: !Int !String -> Bool
				IsLayoutChar i s = isMember s.[i] [' ()->,.[]{}:']


//	Given a pathname, return the filename (remove the path).
RemovePath :: !String -> String
RemovePath s
	| found						= s%(position+1,length_min_1)
	| otherwise					= s
where
	(found,position)			= LastColon s length_min_1
	length_min_1				= size s-1
	
	LastColon :: String !Int -> (!Bool,!Int)
	LastColon s i
		| i<=0					= (False,0)
		| dirseparator==s.[i]	= (True,i)
		| otherwise				= LastColon s (i-1)
