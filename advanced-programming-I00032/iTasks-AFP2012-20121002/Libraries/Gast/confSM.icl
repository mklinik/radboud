implementation module confSM

/*
	GAST: A Generic Automatic Software Test-system
	
	ioco: Input Output COnformance of reactive systems

	Pieter Koopman, 2004, 2005
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdEnv, MersenneTwister, gen, GenEq, genLibTest, testable, StdLib

toSpec :: (state input -> [(state,[output])]) -> Spec state input output // conversion for old specificaions
toSpec fun = \s i = [Pt o t \\ (t,o) <- fun s i]

SpectoIUTstep :: (Spec t i o) (t i -> [[o]]) -> IUTstep (t,RandomStream) i o | genShow{|*|} t & genShow{|*|} i & genShow{|*|} o
SpectoIUTstep spec outputGen = selectTrans
where
	selectTrans (t,[r,r2,r3:rnd]) i
		# tuples = spec t i
		  len = lengthN 353 0 tuples
		| len == 0
			= abort ("\n\nIUT is not input enabled in state "+show1 t+" for input "+show1 i+"\n")
			= case tuples !! ((abs r) rem len) of
				Pt o t = (o,(t,rnd))
				Ft f  #	outputs = outputGen t i
						leno = lengthN 31 0 outputs
					  | leno == 0
						= abort ("\n\nOutput function yields no output in state "+show1 t+" for input "+show1 i+"\n")
						# output = outputs !! ((abs r2) rem leno)
						  states = f output
						  lens = lengthN 37 0 states
						| lens == 0
							= abort ("\n\nIUT is not input enabled in state "+show1 t+" for input "+show1 i+" output "+show1 output+"\n")
							= (output,(states !! ((abs r3) rem lens),rnd))
 
 // =abort "toUIT"

:: NewInput i = NewInput i | Reset | End

:: *TestState s i o
 =	!{	spec	:: !Spec s i o
//	,	specW	:: !SpecWorld s i
	,	iniState:: !s
	,	curState:: ![s]
	,	nRej	:: !Int
	,	nTrun	:: !Int
	,	nPath	:: !Int
	,	nStep	:: !Int
	,	nTotal	:: !Int
	,	maxPath	:: !Int
	,	maxLen	:: !Int
	,	nOnPath	:: !Int
	,	inputs	:: (RandomStream s -> [i])
	,	input	:: !((TestState s i o) -> *(NewInput i, TestState s i o))
	,	n		:: !Int
	,	rnd		:: RandomStream
	,	h_in	:: [i]
	,	h_out	:: [[o]]
	,	h_state	:: [[s]]
	,	fileName:: String
	,	errFile	:: !*File
	,	mesFile	:: !*File
	,	trace	:: !Bool
	,	incons	:: [o] [s] -> Maybe [String]
	,	stop	:: [s] -> Bool
	}
/*
apply :: i (IUT s i o) -> ([o],IUT s i o) | genShow{|*|} s & genShow{|*|} i
apply i (IUT f s [r:rnd])
	= case f s i of
		[]		= abort ("IUT is not input enabled! State: "+++show1 s+++", input: "+++show1 i)
		list
			# n		= length list
			  (t,o)	= list !! ((abs r) rem n)
			= (o,IUT f t rnd)
*/
 
switchSpec :: (Spec s i o) (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
switchSpec spec ts=:{nOnPath} = newSpec spec nOnPath ts

newSpec :: (Spec s i o) !Int (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
newSpec spec2 0 ts=:{spec} = oldSpec spec {ts & spec = spec2}
newSpec spec2 n ts
 # (i,ts) = onTheFly ts
 = case i of
	Reset	= (i,{ ts & input = newSpec spec2 ts.nOnPath })
	NewInput _	= (i,{ ts & input = newSpec spec2 (n-1)})
	_		= (i,ts)

oldSpec :: (Spec s i o) (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
oldSpec spec2 ts
 # (i,ts) = onTheFly ts
 = case i of
	Reset	= (i,{ ts & input = newSpec spec2 ts.nOnPath })
	_		= (i,ts)

onAndOff :: (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
onAndOff ts=:{nOnPath} = onPath nOnPath ts

onPath :: Int (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
onPath 0 ts=:{spec}
 # (rnd1,rnd2) = split ts.rnd
 = offPath spec (ggen{|*|} 2 rnd1) {ts & spec = mkComplete spec, rnd = rnd2} 
onPath n ts
 # (i,ts) = onTheFly ts
 = case i of
	Reset	= (i,{ ts & input = onPath ts.nOnPath })
	NewInput _	= (i,{ ts & input = onPath (n-1)})
	_		= (i,ts)

mkComplete spec s i
	= case spec s i of
		[] = [Pt [] s]
		r  = r

offPath :: (Spec s i o) [i] (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
offPath spec [] ts = (Reset,{ ts & input = onPath ts.nOnPath })
offPath spec [i:r] ts
  | ts.nStep >= ts.maxLen-1 || ts.nRej >= ts.maxLen-1
	= (Reset,{ ts & input = onPath ts.nOnPath })
	= (NewInput i,{ts & input = offPath spec r})

onTheFly :: (TestState s i o) -> *(NewInput i, TestState s i o)
onTheFly ts=:{curState,inputs,rnd,spec}
	# [r1,r2:rnd] = rnd
	= case [ i	\\ s <- randomize curState (genRandInt r1) 2 (\_=[])
				,  i <- inputs (genRandInt r2) s
				| not (isEmpty (spec s i))
			] of
		[]	= (Reset, {ts & rnd=rnd})
		is	# max = 157				// the maximum number of outputs to consider
			  n = lengthN max 0 is	// the number of the selected input
			  [r:rnd] = rnd
			  i = is !! ((abs r) rem n)
			= (NewInput i,{ts & rnd = rnd})

lengthN :: !Int !Int ![a] -> Int
lengthN m n [] = n
lengthN m n [a:x]
	| n<m
		= lengthN m (n+1) x
		= m

fixedInputs []        ts = (End, ts)
fixedInputs [[]   :r] ts = (Reset, { ts & input = fixedInputs r })
fixedInputs [[a:x]:r] ts=:{curState,spec} 
	| isEmpty [t \\ s<- curState, t<-spec s a]
		= (Reset, { ts & input = fixedInputs r , nTrun = ts.nTrun+1})
		= (NewInput a, {ts & input = fixedInputs [x:r]})

genLongInput :: s Int (Spec s i o) [i] [Int] -> [i]
genLongInput s 0 spec inputs [r:x] = randomize inputs x 7 (\_.[])
genLongInput s n spec inputs [r,r2:x]
	= case [ i \\ i <- inputs | not (isEmpty (spec s i)) ] of
		[]	= []
		l	# i = l !! ((abs r) rem (length l))
			= case spec s i of
				[]		= abort "\n\nError in genLongInput, please report.\n\n"
				list	# len		= length list
						  s	= case list !! ((abs r2) rem len) of
								Pt o s = s
								Ft f = abort "genLongInput Ft f"
						= [ i : genLongInput s (n-1) spec inputs x ]	

genLongInputs :: s (Spec s i o) [i] Int [Int] -> [[i]]
genLongInputs s spec inputs n [r:x] = [genLongInput s n spec inputs (genRandInt r): genLongInputs s spec inputs n x]
/*
testConfSM :: [TestOption s i o] (Spec s i o) s (IUTstep .t i o) .t (.t->.t) *File *File -> (.t,*File,*File)
			| ggen{|*|} i & gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
testConfSM opts spec s0 iut t reset console file
	# ts=:{fileName}		= initState file console
	# (t,ts)	= doTest ts iut t reset
	  {mesFile,errFile} = ts
	= (t, mesFile, errFile)
where
	initState file console
	 =	handleOptions opts
	 	{	spec	= spec
	// 	,	specW	= \s i.[]
		,	iniState= s0
		,	curState= [s0]
		,	nRej	= 0
		,	nTrun	= 0
		,	nPath	= 0
		,	nStep	= 0
		,	nTotal	= 0
		,	maxPath	= 100
		,	maxLen	= 1000
		,	nOnPath	= 50
		,	inputs	= (\rnd s -> ggen {|*|} 2 rnd)
		,	input	= onTheFly 
		,	n		= 0
		,	h_in	= []
		,	h_out	= []
		,	h_state	= []
		,	rnd		= aStream
		,	errFile	= file
		,	fileName= outputFile
		,	mesFile	= console <<< "Gast starts testing.\n"
		,	trace	= False
		,	incons	= \o ss -> Nothing
		,	stop	= (\states = False)
		}

findFileName [] name = name
findFileName [ErrorFile s:r] name = findFileName r s
findFileName [_:r] name = findFileName r name

doTest :: (TestState s i o) (IUTstep .t i o) .t (.t->.t) -> (.t,TestState s i o)
		| gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
doTest ts=:{input,curState,spec,incons} step t reset
  | isEmpty ts.curState
	= (t, errorFound ts)
  | ts.nStep >= ts.maxLen || ts.nRej >= ts.maxLen
	= doTest (resetState ts) step (reset t) reset
  | ts.nPath >= ts.maxPath
  	| ts.nRej == 0 && ts.nTrun == 0
	  = (t,{ts & mesFile = ts.mesFile	<<< "\nEnd of testing, maximum paths used. \n"
										<<< ts.nPath
										<<< " test paths executed successful, in total "
										<<< ts.nTotal <<< " transitions.\n"})
	  = (t,{ts & mesFile = ts.mesFile	<<< "\nEnd of testing, maximum paths used. \n"
										<<< ts.nPath
										<<< " test paths executed successful, "
										<<< ts.nTrun <<< " paths truncated, "
										<<< " in total "
										<<< ts.nTotal <<< " transitions.\n"})
  #	(inp,ts) = input ts
  =	case inp of
	  Reset	= doTest (resetState ts) step (reset t) reset
	  End	| ts.nRej == 0 && ts.nTrun == 0
			  = (t,{ts & mesFile = ts.mesFile	<<< "\nAll input paths tested successfully.\n"
												<<< "All " <<< ts.nPath
												<<< " executed test paths successful (Proof), in total "
												<<< ts.nTotal <<< " transitions.\n"})
			  = (t,{ts & mesFile = ts.mesFile	<<< "\nAll inputs tested successfully.\n"
												<<< (ts.nPath-ts.nRej)
												<<< "test path executed successful (Proof), " <<< ts.nRej
												<<< " paths rejected " <<< ts.nTrun
												<<< " paths truncated, "
												<<< "in total " <<< ts.nTotal <<< " transitions.\n"})
	  NewInput i	// assumption: only inputs allowed by spec will be generated:
		#!	(iut_o,t) = step t i
			tuples = [tup \\ s<-curState, tup<-spec s i]
			states = mkset (newStates tuples iut_o)
		| isEmpty states
			#! errFile = ts.errFile <<< "Issue found! Trace:\n"
									<<< "SpecificationStates Input -> ObservedOutput\n"
			   errFile = showError (ts.nStep+1) [curState:ts.h_state] [i:ts.h_in] [iut_o:ts.h_out] errFile
			   errFile = errorInfo ts.nPath ts.nRej ts.nTrun (ts.nTotal+1) (errFile <<< "\n")
			   mesFile = errorInfo ts.nPath ts.nRej ts.nTrun (ts.nTotal+1) ts.mesFile
			   mesFile = mesFile <<< "See file \"" <<< ts.fileName <<< "\" for details about the issue.\n"
			= (t,{ts	& mesFile = mesFile
						, errFile = errFile
						, curState = []
						})
			= case incons iut_o states of
				Nothing
				   # mesFile = ts.mesFile <<< "paths: " <<< ts.nPath <<< ", rejected: " <<< ts.nRej <<< ", truncated: " <<< ts.nTrun <<< "...\r"
				   = doTest {ts	& curState = states
								, nStep = ts.nStep+1
								, nTotal =ts.nTotal+1
								, h_in = [i:ts.h_in]
								, h_out = [iut_o:ts.h_out]
								, h_state = [curState:ts.h_state]
								, mesFile = if ts.trace (mesFile <<< ts.nPath <<< "," <<< ts.nStep <<< ": " <<< show1 ts.curState <<< " " <<< show1 i <<< " " <<< show1 iut_o <<< "\n") mesFile
								}
							step t reset
				Just errors	
					#! errFile = ts.errFile <<< "Inconsistency! Trace:\n"
											<<< "SpecificationStates Input -> ObservedOutput\n"
					   errFile = showError (ts.nStep+1) [curState:ts.h_state] [i:ts.h_in] [iut_o:ts.h_out] errFile
					   errFile = errFile <<< "Inconsistency info:\n" <<< errors <<< "\n"
					   errFile = errorInfo ts.nPath ts.nRej ts.nTrun (ts.nTotal+1) (errFile <<< "\n")
					   mesFile = ts.mesFile <<< "Inconsistency found!\n" <<< errors <<< "\n\n"
					   mesFile = errorInfo ts.nPath ts.nRej ts.nTrun (ts.nTotal+1) mesFile
					   mesFile = mesFile <<< "See file \"" <<< ts.fileName <<< "\" for details.\n"
					= (t,{ts	& mesFile = mesFile
								, errFile = errFile
								, curState = []
								})
where
	errorInfo :: !Int !Int !Int !Int *File -> *File
	errorInfo nPath nRej nTrun nTotal file
	 = file	<<< "Issue found in path " <<< (nPath+1) <<< ", "
			<<< (nPath-nRej) <<< " paths executed, "
			<<< nTrun <<< " tests truncated, in total "
			<<< nTotal <<< " transitions.\n"

outputFile = "testOut.txt"

newStates [] iut_o = []
newStates [Pt o s:r] iut_o
	| o === iut_o
		= [s:newStates r iut_o]
		= newStates r iut_o
newStates [Ft f:r] iut_o = f iut_o ++ newStates r iut_o

resetState ts
  =	{ts	& curState = [ts.iniState]
		, nPath    = ts.nPath+1
		, nStep    = 0
		, h_in     = []
		, h_out    = []
		, h_state  = []
		, mesFile  = if ts.trace (ts.mesFile <<< "End of path reached: reset.\n") ts.mesFile
	}

errorFound ts=:{errFile,mesFile}
 # errFile = errFile <<< "Issue Found!\n"
 # mesFile = mesFile <<< "Issue Found!\n"
 = {ts & errFile = errFile,mesFile=mesFile}

restart testState = { testState & h_in = [], h_out = [], h_state = [] }
*/ /**/
testConfSM :: [TestOption s i o] (Spec s i o) s (IUTstep .t i o) .t (.t->.t) *d -> (.t,*d)
			| FileSystem d & ggen{|*|} i & gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
testConfSM opts spec s0 iut t reset world
	# (console,world) = stdio world
	  filename		  = findFileName opts outputFile
	# (ok,file,world) = fopen filename FWriteText world
	| not ok
		# console = console <<< "Cannot open output file "<<< filename
		= (t, snd (fclose console world))
	# //console	= console <<< ".\n"
	  ts=:{fileName}		= initState file console
	# (t,ts)	= doTest ts iut t reset
	  {mesFile,errFile} = ts
	= (t, snd (fclose mesFile (snd (fclose errFile world))))
where
	initState file console
	 =	handleOptions opts
	 	{	spec	= spec
		,	iniState= s0
		,	curState= [s0]
		,	nRej	= 0
		,	nTrun	= 0
		,	nPath	= 0
		,	nStep	= 0
		,	nTotal	= 0
		,	maxPath	= 100
		,	maxLen	= 1000
		,	nOnPath	= 50
		,	inputs	= (\rnd s -> ggen {|*|} 2 rnd)
		,	input	= onTheFly 
		,	n		= 0
		,	h_in	= []
		,	h_out	= []
		,	h_state	= []
		,	rnd		= aStream
		,	errFile	= file
		,	fileName= outputFile
		,	mesFile	= console <<< "Gast starts testing.\n"
		,	trace	= False
		,	incons	= \o ss -> Nothing
		,	stop	= (\states = False)
		}

findFileName [] name = name
findFileName [ErrorFile s:r] name = findFileName r s
findFileName [_:r] name = findFileName r name

doTest :: (TestState s i o) (IUTstep .t i o) .t (.t->.t) -> (.t,TestState s i o)
		| gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
doTest ts=:{input,curState,spec,stop} step t reset
  | isEmpty ts.curState
	= (t, errorFound ts)
  | ts.nStep >= ts.maxLen || ts.nRej >= ts.maxLen || stop curState
	= doTest (resetState ts) step (reset t) reset
  | ts.nPath >= ts.maxPath
  	| ts.nRej == 0 && ts.nTrun == 0
	  = (t,{ts & mesFile = ts.mesFile	<<< "\nEnd of testing, maximum paths used. \n"
										<<< ts.nPath
										<<< " test paths executed successful, in total "
										<<< ts.nTotal <<< " transitions.\n"})
	  = (t,{ts & mesFile = ts.mesFile	<<< "\nEnd of testing, maximum paths used. \n"
										<<< ts.nPath
										<<< " test paths executed successful, "
										<<< ts.nTrun <<< " paths truncated, "
										<<< " in total "
										<<< ts.nTotal <<< " transitions.\n"})
  #	(inp,ts) = input ts
  =	case inp of
	  Reset	= doTest (resetState ts) step (reset t) reset
	  End	| ts.nRej == 0 && ts.nTrun == 0
			  = (t,{ts & mesFile = ts.mesFile	<<< "\nAll input paths tested successfully.\n"
												<<< "All " <<< ts.nPath
												<<< " executed test paths successful (Proof), in total "
												<<< ts.nTotal <<< " transitions.\n"})
			  = (t,{ts & mesFile = ts.mesFile	<<< "\nAll inputs tested successfully.\n"
												<<< (ts.nPath-ts.nRej)
												<<< "test path executed successful (Proof), " <<< ts.nRej
												<<< " paths rejected " <<< ts.nTrun
												<<< " paths truncated, "
												<<< "in total " <<< ts.nTotal <<< " transitions.\n"})
//	  Input i	// assumption: only inputs allowed by spec will be generated:
	  NewInput i	// assumption: only inputs allowed by spec will be generated:
		#!	(iut_o,t) = step t i
			tuples = [tup \\ s<-curState, tup<-spec s i]
		= case mkset (newStates tuples iut_o) of
			[]	#! errFile = ts.errFile <<< "Issue found! Trace:\n"
										<<< "SpecificationStates Input -> ObservedOutput\n"
				   errFile = showError (ts.nStep+1) [curState:ts.h_state] [i:ts.h_in] [iut_o:ts.h_out] errFile
				   errFile = errFile <<< "\nAllowed outputs and target states: " <<< show tuples <<< "\n"
				   errFile = errorInfo ts.nPath ts.nRej ts.nTrun (ts.nTotal+1) (errFile <<< "\n")
				   mesFile = errorInfo ts.nPath ts.nRej ts.nTrun (ts.nTotal+1) ts.mesFile
				   mesFile = mesFile <<< "See file \"" <<< ts.fileName <<< "\" for details about the issue.\n"

				= (t,{ts	& mesFile = mesFile
							, errFile = errFile
							, curState = []
							})
			states #! mesFile = ts.mesFile <<< "paths: " <<< ts.nPath <<< ", rejected: " <<< ts.nRej <<< ", truncated: " <<< ts.nTrun <<< "...\r"
				   = doTest {ts	& curState = states
								, nStep = ts.nStep+1
								, nTotal =ts.nTotal+1
								, h_in = [i:ts.h_in]
								, h_out = [iut_o:ts.h_out]
								, h_state = [curState:ts.h_state]
								, mesFile = if ts.trace (mesFile <<< ts.nPath <<< "," <<< ts.nStep <<< ": " <<< show1 ts.curState <<< " " <<< show1 i <<< " " <<< show1 iut_o <<< "\n") mesFile
								}
							step t reset
		where
			errorInfo :: !Int !Int !Int !Int *File -> *File
			errorInfo nPath nRej nTrun nTotal file
			 = file	<<< "Issue found in path " <<< (nPath+1) <<< ", "
					<<< (nPath-nRej) <<< " paths executed to bound or end, "
					<<< nTrun <<< " paths truncated, in total "
					<<< nTotal <<< " transitions.\n"

	//  = (t,{ts & mesFile = ts.mesFile <<< "\nAll inputs tested successfully\n"})

outputFile = "testOut.txt"

newStates [] iut_o = []
newStates [Pt o s:r] iut_o
	| o === iut_o
		= [s:newStates r iut_o]
		= newStates r iut_o
newStates [Ft f:r] iut_o = f iut_o ++ newStates r iut_o

resetState ts
  =	{ts	& curState = [ts.iniState]
		, nPath    = ts.nPath+1
		, nStep    = 0
		, h_in     = []
		, h_out    = []
		, h_state  = []
		, mesFile  = if ts.trace (ts.mesFile <<< "End of path reached: reset.\n") ts.mesFile
	}

errorFound ts=:{errFile,mesFile}
 # errFile = errFile <<< "Issue Found!\n"
 # mesFile = mesFile <<< "Issue Found!\n"
 = {ts & errFile = errFile,mesFile=mesFile}

restart testState = { testState & h_in = [], h_out = [], h_state = [] }
/**/
handleOptions [] ts = ts
handleOptions [o:r] ts=:{mesFile}
	# ts = case o of
				Ntests n = {ts & maxLen = n}
				Nsequences n = {ts & maxPath = n}
				Seed n = {ts & rnd = genRandInt n}
				Randoms rnd = {ts & rnd = rnd }
				FixedInputs ll_input = {ts & input = fixedInputs ll_input }
				InputFun f = {ts & inputs = f }
			//	OutputFun f = {test & } //([s] i -> o)
				FSM inp identify = {ts & input = fixedInputs (generateFSMpaths ts.iniState ts.spec inp identify) }
				MkTrace b = { ts & trace = b }
				OnPath n = { ts & nOnPath = n }
				OnAndOffPath = { ts & input = onAndOff }
				SwitchSpec spec = { ts & input = switchSpec spec }
				OnTheFly = { ts & input = onTheFly }
				ErrorFile f = { ts & fileName = f }
				Stop pred = { ts & stop = pred }
	= handleOptions r ts

showError :: Int [a] [b] [c] !*File -> *File | genShow{|*|} a & genShow{|*|} b & genShow{|*|} c
showError n [a:x] [b:y] [c:z] file = showError (n-1) x y z file <<< "    " <<< n <<< ": " <<< show a <<< " " <<< show1 b <<< " -> " <<< show c <<< "\n"
showError _ []    []    []    file = file
showError _ _     _     _     file = file <<< "\n\n\tInternal error in \"showError\", please report to pieter@cs.ru.nl!\n\n"

/*
testConf :: Int (Spec s i o) (Spec t i o) s t ([s] -> Bool) [[i]] *d -> *d 
			| FileSystem d & gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} t & genShow{|*|} i & genShow{|*|} o //& <<< o
testConf max spec imp state stateIUT stop paths world
	# (console,world) = stdio world
	# (ok,file,world) = fopen outputFile FWriteText world
	| not ok
		= abort ("Cannot open output file "+++outputFile)
	# (file,console) = test 0 0 0 1 paths file console
	= snd (fclose console (snd (fclose file world)))
where
	outputFile = "testOut.txt"
	test s t r n [] file console
		= (file
		  ,console	<<< "All (" <<< (n-r-1) <<< ") executed test paths successful (Proof), " <<< ", rejected " <<< r
					<<< ", tests truncated " <<< t <<< ", in total " <<< s <<< " transitions.\n"
		  )
	test s t r n l  file console
		| n > max
			= (file
			  ,console	<<< max <<< " test paths used, testing terminated, " <<< r <<< " rejected, " <<< t <<< " tests truncated, in total "
						<<< (n-r-1) <<< " paths executed, " <<< s <<< " transitions.\n"
			  )
	test s t r n [path:paths] file console
	//	| cbc spec [state] path
			#!console = if ((n - r) bitand /*1023*/ /*31*/ 7 == 0)
							(console <<< "paths: " <<< (n-1) <<< ", rejected: " <<< r <<< " paths executed: " <<< (n-r-1) <<<  ", truncated: " <<< t <<< "...\r")
							console
			# iut = IUT imp stateIUT (genRandInt 195773)
			#! (ok, skipped, steps, file) = conf 0 [state] iut path [] [] [] [] file
			#! file = file
			   s = s+steps
			| ok
		 		| skipped	= test s (t + 1) r (n + 1) paths file console
							= test s t r (n + 1) paths file console
			  	# console = console <<< "Issue found after " <<< n <<< " paths, " <<< (n-r) <<< " paths executed, " <<< t <<< " tests truncated, in total " <<< s <<< " transitions.\n" <<< "See \"" <<< outputFile <<< "\" for details about the issue.\n"
				= (file,console)
	//		= test s t (r+1) (n+1) paths file
	
	conf s [] iut path out this sts ex file
		#! file = file	<<< "ERROR: output to last input was wrong!\n"
						<<< "Expected outputs: " <<< show1 ex <<< "\n"
		   file = showError (length this) this out sts file
		= (False, False, s, file)
	conf s states iut path out this sts ex file
		| stop states
			= (True, True, s, file)
	conf s states iut [] out this sts ex file = (True, False, s, file)
	conf s states iut [i:path] out this sts ex file
	  #	(iutout,iut) = apply i iut
		specifiedResp	= [ tup \\ s1 <- states, tup <- spec s1 i ]
	  = case specifiedResp of
			[]	= (True, True, s, file)
			_	# states2 = mkset [ s2 \\ (s2,specout) <- specifiedResp | specout === iutout ]
				= conf (s+1) states2 iut path [iutout:out] [i:this] [states:sts] (map snd specifiedResp) file

	showError :: Int [a] [b] [s] !*File -> *File | genShow{|*|} a & genShow{|*|} b & genShow{|*|} s
	showError n [a:x] [b:y] [s:z] file = showError (n-1) x y z file <<< "    " <<< n <<< ": " <<< show s <<< " " <<< show1 a <<< " -> " <<< show b <<< "\n"
	showError _ []    []    []    file = file
	showError _ _     _     _     file = file <<< "\n\n\tInternal error in \"showError\", please report!\n\n"
*/
mkset [a:xs] = [a:[x\\x<-xs|not (x===a)]]
mkset []     = []

instance <<< [a] | <<< a
where
	(<<<) file []    = file
	(<<<) file [a:x] = file <<< a <<< x

derive genShow Trans

// ----------------------------------------------

:: Transition state input output :== (state,input,[output],state)

:: LTS state input output // Labelled Transition System
	=	{ trans 	:: [Transition state input output]
		, initial	:: state
		// set of states and labels are not needed, this is determined by the type state.
		}

generateLTS :: (Spec s i o) s (s->[i]) -> LTS s i o | gEq{|*|} s
generateLTS spec s i = generateLTSpred (\_=True) spec s i

generateLTSpred :: (s->Bool) (Spec s i o) s (s->[i]) -> LTS s i o | gEq{|*|} s
generateLTSpred p spec s inputs
 =	{ trans 	= generateTrans [] [s] spec inputs
	, initial	= s
	}
where
//	generateTrans :: [s] [s] (Spec s i o) (s->[i]) -> [(s,i,[o],s)] | gEq{|*|} s
	generateTrans seen [] spec inputs = []
	generateTrans seen [s:todo] spec inputs
		| not (isEmpty [f \\ i <- inputs s, Ft f <- spec s i ])
			= abort "Cannot handle (Ft f) transitions in FSM"
		# trans	= [ (s,i,o,t) \\ i <- inputs s, Pt o t <- spec s i ]
		  seen	= [ s:seen ]
		  new	= [ t \\ (_,_,_,t) <- trans | isNew seen t && p t ]
		= trans ++ generateTrans seen (removeDup (todo++new)) spec inputs
//	isNew :: [s] s -> Bool | gEq{|*|} s
	isNew [] t = True
	isNew [s:seen] t
		| s===t
			= False
			= isNew seen t
//	removeDup :: !.[a] -> .[a] | gEq{|*|} a
	removeDup [x:xs] = [x:removeDup (filter ((=!=) x) xs)]
	removeDup _      = []

A4 :: (LTS state input output) (state -> [input]) -> [[input]] | gEq{|*|} state
A4 lts stateIdent = gen [([],lts.initial)] lts.trans []
where
	gen _ [] _ = []
	gen [(input,state):tups] trans next
		= step input trans state [] tups next False False
	gen [] t [] = []
	gen [] t l  = gen (reverse l) t []
	
	step input [trans=:(s1, toki, toko,s2):r] state seentrans tups next ever now
		| s1 === state
			= step [toki:input] (revApp seentrans r) s2 [] tups [(input,state):next] True True
			= step input r state [trans:seentrans] tups next ever now
	step input [] state seentrans tups next ever now 
		| now	= step input seentrans state [] tups next True False // input is extended
		| ever	= [revApp input (stateIdent state): gen tups seentrans next]
				= gen tups seentrans next
	
	revApp [] acc = acc
	revApp [a:x] acc = revApp x [a:acc]

generateFSMpaths :: s (Spec s i o) [i] (s->[i]) -> [[i]] | gEq{|*|} s
generateFSMpaths start spec inputs identify = A4 (generateLTS spec start (\_=inputs)) identify

//----------------- the after operator from ioco theory -----------------//
//----------------- yields the possible states after an input sequence --//

(after) infix 0 :: [s] (Spec s i o) -> ([i] -> [s])
(after) s spec // = apply s
  = \i = case i of
			[]    = s
			[i:r]
				| not (isEmpty [f \\ u<-s, Ft f <- spec u i ])
					= abort "Cannot handle (Ft f) transitions in (after)"
					= ([t \\ u<-s, Pt o t <- spec u i] after spec) r
/*where
	apply [] i = []
	apply s [] = s
	apply s [i:r] = apply [t \\ u<-s, (t,o) <- spec u i] r
*/
//----------------- properties of specifications -----------------//

propDeterministic :: (Spec s i o) s i -> Bool
propDeterministic m s i = length (m s i) <= 1

propTotal :: (Spec s i o) s i -> Bool
propTotal m s i = not (isEmpty (m s i))
