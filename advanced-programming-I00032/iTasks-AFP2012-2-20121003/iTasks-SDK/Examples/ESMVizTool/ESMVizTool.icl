implementation module ESMVizTool

import iTasks
import ESMSpec
import GenPrint_NG

import Graphviz
import GraphvizVisualization

derive bimap (,), Maybe
derive class iTask	KnownAutomaton, State

finished_state_color :: (!Color,!Color)
finished_state_color	= (Color "blue", Color "white")

default_state_color :: (!Color,!Color)
default_state_color		= (Color "grey90",Color "black")
	
shared_state_color :: (!Color,!Color)
shared_state_color		= (Color "gray",Color "white")
	
shared_active_state_color :: (!Color,!Color)
shared_active_state_color = (Color "gray",Color "red")

active_state_color :: !Int -> (!Color,!Color)
active_state_color nr	= (RGB 255 dim dim,Color "white")
where
	dim	= min 250 (255 - 255 / (min nr 3))
	
fontsize = 12.0 // 18.0

:: State s i o
 =	{ ka	:: !KnownAutomaton s i o
	, ss	:: ![s]
	, trace	:: !Traces s i o
	, n		:: !Int
	, r		:: !Int
	}

esmVizTool :: !(ESM s i o) *World -> *World
			| all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
esmVizTool esm world
	= startEngine (iterateTask (DiGraphFlow esm) newstate) world
where
	newstate = { ka = newKA, ss = [esm.s_0], trace = [], n = 1, r = 20111108}
	 
DiGraphFlow :: !(ESM s i o) (State s i o) -> Task (State s i o) 
				| all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
DiGraphFlow	esm st=:{ka,ss,trace,n,r}
 =	anyTask	[ selectInput
			, state esm st @? const NoValue
// 			, enterChoice "go to state... " [] (map show1 (if (isEmpty nodes) ss nodes)) >>= updateDig st
// 			, chooseTaskComBo "go to state... " [let label = show1 node in (label, updateDig st label) \\ node <- if (isEmpty nodes) ss nodes]
			, chooseTaskComBo ("Actions","Do one of the following actions...")
				[("Back" , back st)
				,("Prune", prune st)
				,("Reset", return newState)
				,("Clear trace", return {st & trace  = []})
				:[let label = render node in ("go to: " + label, updateDig st node) \\ node <- nodes]
				] >>! return
    		, stepN esm st >>! return
			, viewInformation (Title "Trace & legend") [ViewWith traceHtml] trace <<@ traceTweak >>| return st
    		]
    >>* [WhenValid (const True) return]
where
	selectInput
		| isEmpty inputs
			= viewInformation ("Input","no transition from current state") [] "Use another action" >>| return st
			= updateChoice ("Input","Choose an input... ")
					[ChooseWith (if (length inputs > 7) ChooseFromComboBox ChooseFromRadioButtons) fst]
					trans
					(trans !! 0)
				>>* [WithResult (Action "Apply selected input") (const True) (\(l,t) -> t)
					,AnyTime (Action "I'm feeling lucky") (\_ -> systemInput)
					]
	where trans = sortBy (\(a,_) (b,_).a<b) [(render i,step esm st i) \\ i<-inputs]
	
	systemInput
		| isEmpty newInputs
			| isEmpty inputs2
				= return st
				= step esm {st & r = rn} (inputs2!!((abs r) rem (length inputs2)))
			= step esm {st & r = rn} (newInputs!!((abs r) rem (length newInputs)))

	inputs		= possibleInputs esm ss
	inputs2		= [ i \\ i <- inputs, (s,j,_,t) <- ka.trans | i === j && gisMember s ss && ~ (gisMember t ss) ]
	newInputs	= filter (\i.not (gisMember i usedInputs)) inputs
	usedInputs	= [ j \\ (s,j,_,_) <- ka.trans | gisMember s ss ]
	rn			= hd (genRandInt r)
	nodes		= nodesOf ka
	newState 	= { ka = newKA, ss = [esm.s_0], trace = [], n = 1, r = rn}

	traceTweak	=  AfterLayout (tweakUI (\x -> appDeep [0] (fixedWidth 670 o fixedHeight 200) x)) 
	
stepN :: !(ESM s i o) !(State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
stepN esm st=:{ka,ss,trace,n,r}
 =		updateInformation ("Steps","Add multiple steps...") [] n
	>>= doStepN esm st
	
doStepN :: !(ESM s i o) !(State s i o) Int -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
doStepN esm state=:{ka,ss,trace,r} n
	= if (n>0)
  		(return {state & ka = addTransitions n esm ss (possibleInputs esm ss) ka, r = rn })
  		(return {state & n = 1})
where rn = hd (genRandInt r)

chooseTask :: !d ![(String,Task o)] -> Task o | descr d & iTask o 
chooseTask msg tasks = enterChoice msg [] [(l, Hidden t) \\ (l,t) <- tasks] >>= \(l, Hidden t). t

chooseTaskComBo :: !d ![(String,Task o)] -> Task o | descr d & iTask o 
chooseTaskComBo msg tasks
 =	updateChoice msg [ChooseWith ChooseFromComboBox fst] trans (trans !! 0) >>= \(l, Hidden t). t
 >>! return
where
	trans = [(l, Hidden t) \\ (l,t) <- tasks]

prune :: !(State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
prune state=:{ka,ss,trace,n,r}
	= return {state & ka = {ka &trans=[t\\t<-ka.trans|gisMember t onTraces],issues=[i\\i=:(t,_)<-ka.issues|gisMember t onTraces]}, r = rn}
where
	onTraces = flatten trace
	rn		= hd (genRandInt r)

state :: !(ESM s i o) !(State s i o) -> Task (State s i o) | all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
state esm st=:{ka,ss,trace,n,r}
	| isEmpty ka.issues
		=	digraph
		=	digraph -|| viewIssues st 
where
	digraph = updateInformation Void [UpdateWith toView fromView] st <<@ AfterLayout (tweakUI (fixedWidth 700 o fixedHeight 300))
	
	//Make an editable digraph from the esm state
	toView st=:{ka,ss,trace} //TODO: MOVE mkDigraph function to this module as it is essentially the toView of a state
		= includeChanges (mkDigraph "ESM" (ka, esm.s_0, ss, allEdgesFound esm ka, sharedNodesOf ka, map fst ka.issues, flatten trace)) 
	//Map changes in the diagraph back to the esm state
	fromView st dg = st
		
	 //(mkDigraph "ESM" (ka, esm.s_0, ss, allEdgesFound esm ka, sharedNodesOf ka, map fst ka.issues, flatten trace))
	 //	>>= updateDig st

mkDigraph :: String (KnownAutomaton s i o,s,[s],[s],[s],[SeenTrans s i o],[SeenTrans s i o]) -> Digraph | render, gEq{|*|}, genShow{|*|} s & render, gEq{|*|} i & render, gEq{|*|} o
mkDigraph name (automaton,s_0,init_states,finished,shared,issues,trace)
	= Digraph
		(remove_spaces name)
		graphAttributes
		(if (isEmpty automaton.trans)
			if (isEmpty init_states)
				[NodeDef 0 [NStAllEdgesFound False] (nodeAttributes s_0 init_states False False) []]
				[NodeDef i [NStAllEdgesFound False] (nodeAttributes n init_states False False) []
				\\ n <- init_states & i <- [0..]
				]
			[NodeDef (nrOf automaton n) [NStAllEdgesFound (gisMember n   finished)] (nodeAttributes n   init_states (gisMember n   finished) (gisMember n   shared))
			         [ let (s,i,o,t) = trans in
			           (nrOf automaton t	, [ EAtt_label (render i+++"/"+++showList ("[","]",",") o)
			                                , EAtt_fontname "Helvetica"
			                                , EAtt_fontsize fontsize
			                                , EAtt_labelfontname "Helvetica"
			                                , EAtt_labelfontsize fontsize
			                                , EAtt_color
			                                			 (if (gisMember trans issues)
			                                						(Color "red")
			                                			 (if (gisMember trans trace)
			                                						(Color "blue")
			                                						(Color "black")))
			                                , EAtt_arrowsize (if (gisMember trans trace) 2.0 1.2)
			                             //   , EAtt_style (if (isMember trans trace) EStyle_bold EStyle_solid)
			                                ])
			         \\ trans <- edgesFrom n automaton
			         ]
			\\ n <- let nodes = nodesOf automaton in if (gisMember s_0 nodes && hd nodes =!= s_0) [s_0:filter ((=!=) s_0) nodes] nodes
			]
		) Nothing
where
	graphAttributes				= [ GAtt_rankdir  RD_LR // horizontal
	//graphAttributes				= [ GAtt_rankdir  RD_TB // RD_LR
							  	  , GAtt_size     		(Sizef 7.2 3.0 False)
								 // , GAtt_size			(Sizef 5.0 3.0 True)
								  , GAtt_fontsize		9.0 // 12.0
								  , GAtt_bgcolor  		(Color "white")
								  , GAtt_ordering 		"out"
//								  , GAtt_outputorder	OM_nodesfirst	// OM_edgesfirst	//  PK
								  , GAtt_outputorder	OM_nodesfirst	// OM_edgesfirst	//  PK
								  ]
	nodeAttributes n init_states finished shared
								= (if (gisMember n init_states)
										(if shared	[ NAtt_fillcolor shac_backgr, NAtt_fontcolor shac_txt ]
													[ NAtt_fillcolor act_backgr, NAtt_fontcolor act_txt ])
								  (if finished [ NAtt_fillcolor done_backgr,NAtt_fontcolor done_txt]
								  		(if shared	[ NAtt_fillcolor shar_backgr, NAtt_fontcolor shar_txt ]
									              	[ NAtt_fillcolor def_backgr, NAtt_fontcolor def_txt ])
								  )) ++
						          [ NAtt_label		(render n)
						          , NAtt_tooltip	(show1 n)
						          , NAtt_style		NStyle_filled
						          , NAtt_shape		(if (n === s_0) NShape_doublecircle NShape_ellipse /*NShape_circle*/)
						          , NAtt_fontname	"Helvetica"
						          , NAtt_fontsize	fontsize
						          , NAtt_fixedsize	False // True
						          , NAtt_width 1.0,	NAtt_height 1.0
						          , NAtt_margin		(SingleMargin 0.003)
						          ]
	where
		( act_backgr, act_txt)	= active_state_color (length init_states)
		(done_backgr,done_txt)	= finished_state_color
		( def_backgr, def_txt)	= default_state_color
		(shar_backgr,shar_txt)	= shared_state_color
		(shac_backgr,shac_txt)	= shared_active_state_color

	
	showList :: !(!String,!String,!String) ![a] -> String | render a
	showList (open,close,delimit) []  = open +++ close
	showList (open,close,delimit) [x] = open +++ render x +++ close
	showList (open,close,delimit) xs  = open +++ foldr (\x str->render x+++delimit+++str) "" (init xs) +++ render (last xs) +++ close



includeChanges :: !Digraph -> Digraph
includeChanges dg=:(Digraph _ _ _ Nothing)		= dg
includeChanges (Digraph title atts nodes change)= Digraph title atts (map includeNodeChange nodes) Nothing
where
	(SelectedItem nr`)							= fromJust change
	
	includeNodeChange :: !NodeDef -> NodeDef
	includeNodeChange (NodeDef nr st atts edges)
		| nr==nr`								= NodeDef nr st (map replaceNodeAtt atts) edges
		| otherwise								= NodeDef nr st (map defaultNodeAtt atts) edges
	where
		all_edges_found							= not (isEmpty [s \\ s=:(NStAllEdgesFound True) <- st])
		
		replaceNodeAtt (NAtt_fillcolor _)		= NAtt_fillcolor (fst (active_state_color 1))
		replaceNodeAtt (NAtt_fontcolor _)		= NAtt_fontcolor (snd (active_state_color 1))
		replaceNodeAtt att						= att
		
		defaultNodeAtt (NAtt_fillcolor c)		= NAtt_fillcolor (if all_edges_found (fst finished_state_color) (fst default_state_color))
		defaultNodeAtt (NAtt_fontcolor c)		= NAtt_fontcolor (if all_edges_found (snd finished_state_color) (snd default_state_color))
		defaultNodeAtt att						= att

//TODO: Turn this into a (Diagraph State -> State function)
updateDig :: !(State s i o) !s  -> Task (State s i o) | all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
updateDig state=:{ka,ss,trace,n,r} ns
	= return {state & ss = [ns], trace = findSelectedStates ns ka ss trace}
where
	findSelectedStates ns ka ss trace
		| gisMember ns ss
			= narrowTraces trace [ns]
		# oneStep = [tr \\ tr=:(s,i,o,t)<-ka.trans | t===ns && gisMember s ss]
		| not (isEmpty oneStep)
			= trace++[oneStep]
			= partTraces trace ns []

step :: !(ESM s i o) (State s i o) i -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
step esm state=:{ka,ss,trace,n,r} i
		= let next   = nextStates esm i ss
			  ka`    = addTransitions 1 esm ss [i] ka
			  trace` = addStep esm ss i trace
			  rn	 = hd (genRandInt r)
		  in return {state & ka = ka`, ss = next, trace = trace`}

back :: (State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
back state=:{ka,ss,trace,n,r}
	| isEmpty trace
		= return state
		= let next   = startStates (last trace)
			  trace` = init trace
			  rn	 = hd (genRandInt r)
		  in return {state & trace = trace`, ss = next, r = rn}

newKA = {trans=[],issues=[]}

iterateTask :: (a->Task a) a -> Task a | iTask a
iterateTask task a = task a >>= iterateTask task

orTaskL :: [Task a] -> Task a | iTask a
orTaskL l = foldl1 (-||-) l

foldl1 op [a]   = a
foldl1 op [a:x] = op a (foldl1 op x)

traceHtml :: (Traces a b c) -> HtmlTag | render a & render b & render c
traceHtml trace
   = DivTag []
		[ H3Tag [] [Text "Trace:"]
	    , TableTag []
	       [ TrTag [] [TdTag [] (map Text (flatten [transToStrings t [] \\ t <- step]))]
	       \\ step <- trace
	       ]
	    , BrTag []
	    , H3Tag [] [Text "Legend:"]
	    , TableTag []
	       [ TrTag [] [TdTag [] [Text string]]
	       \\ string <- [ "red node: current state"
	       				, "blue node: all transitions from this node shown"
	       				, "grey node: more transitions from this node exists"
	       				, "black arrow: transition not in current trace"
	       				, "blue arrow: transition in current trace"
	       				, "red arrow: transition with an issue"
	       				, "---"
	       				, "Back: go to the previous state"
	       				, "Prune: remove all correct transitions that are not on the trace"
	       				, "Reset: start all over"
	       				, "Clear trace: remove trace, but keep everything else"
	       				]
	       ]
	    ]

viewIssues :: (State s i o) -> Task [(SeenTrans s i o,[String])] | render, iTask s & render, iTask i & render, iTask o
viewIssues st=:{ka}
	= viewInformation "Issues" [ViewWith issuesToHtml] ka.issues
where	
	issuesToHtml :: [(SeenTrans s i o,[String])] -> HtmlTag | render s & render i & render o
	issuesToHtml l
		=	DivTag []
			[ H3Tag [] [Text "Issues found:"]
			: [ TableTag []
				[ TrTag [] [TdTag [] (map Text (transToStrings t [": ":ss])) ]
				\\ (t,ss) <- l
				]
			  ]
			]

transToStrings :: (SeenTrans s i o) [String] -> [String] | render s & render i & render o
transToStrings (s,i,o,t) c = ["(",render s,",",render i,",[":showList "," o ["],",render t,")":c]]

showList :: !String ![a] [String] -> [String] | render a
showList delimit []    c = c
showList delimit [x]   c = [render x:c]
showList delimit [x:r] c = [render x,delimit:showList delimit r c]

partTraces :: (Traces s i o) s (Traces s i o) -> (Traces s i o) | gEq{|*|} s
partTraces [] s seen = []
partTraces [trans:rest] s seen
	| gisMember s (targetStates trans)
		= narrowTraces (reverse [trans:seen]) [s]
		= partTraces rest s [trans:seen]

allEdgesFound :: (ESM s i o) (KnownAutomaton s i o) -> [s] | gEq{|*|} s & ggen{|*|} i
allEdgesFound esm automaton
	= [s \\ s <- nodesOf automaton
	      | length (edgesFrom s automaton) == length [t \\ i<-enumerate,t<-nextStates esm i [s]] 
	  ]

remove_spaces :: !String -> String
remove_spaces str = toString [ c \\ c <- fromString str | not (isSpace c)]

toHtmlString :: a -> String | gVisualizeText{|*|} a
toHtmlString x
	# string = visualizeAsText AsDisplay x
	= toString [checkChar c \\ c <-fromString string]
where
	checkChar '"' = '\''
	checkChar  c  = c

instance render Int    where render i = toString i
instance render String where render s = s
