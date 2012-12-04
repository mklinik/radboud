implementation module GinParser

import StdBool
import StdArray
import StdEnum
from StdFunc import o,flip,seq
//import StdMisc
import StdList
import StdOrdList
import StdTuple

import GenPrint

//Clean-platform:
import Graph
import Map
import Void
import JSON
import Text

//Clean-compiler:
from general	import	::Optional(..)
from syntax		import	
						::ArrayKind,
						::BasicValue,
						::Bind,
						::BoundExpr,
						::CaseAlt,
						::CodeBinding,
						::DynamicType,
						::ElemAssignment,
						::Env,
						::FieldAssignment,
						::FieldNameOrQualifiedFieldName,
						::FieldSymbol,
						::Global,
						::Ident(..),
						::LocalDefs,
						::OptionalRecordName,
						::ParsedExpr(..),
						::ParsedSelection,
						::ParsedSelectorKind,
						::Position,
						::Ptr,
						::Qualifier,
						::Sequence,
						::SymbolPtr,
						::SymbolTableEntry,
						::TypeKind
import CleanDocParser

//GiN:
import Monad
import GinSyntax
import GinAbstractSyntax
import GinFlowLibrary
import GinORYX
import GinStorage

//--------------------------------------------------------------------------------------------------
//GParseState monadic combinators

derive class iTask GPathNode, GParseResult, GParseState

isParseError :: (GParseResult a) -> Bool
isParseError (GError _) = True
isParseError _ = False

getParseSuccess :: (GParseResult a) -> a
getParseSuccess (GSuccess a) = a

getParseError :: (GParseResult a) -> [(GPath, String)]
getParseError (GError b) = b

instance Monad GParseState
where
	ret :: a -> GParseState a
	ret a = GParseState (\path = GSuccess a)
	
	(>>>) infixr 5 :: (GParseState a) (a -> GParseState b) -> GParseState b
    (>>>) (GParseState m) k = GParseState (\path = case m path of
                                         GError errors = GError errors
                                         GSuccess a = strip (k a) path) where
          strip (GParseState f) = f
          
instance PathNode GNode
where
	getPathNode :: GNode GPath -> GPath
	getPathNode { GNode | identifier } _ = [NodePath identifier]

instance PathNode GEdge
where
	getPathNode :: GEdge GPath -> GPath
	getPathNode { GEdge | identifier} _ = [EdgePath identifier]
	
instance PathNode (String,GExpression)
where
	getPathNode :: (String,GExpression) GPath -> GPath
	getPathNode (s,_) path = [ParamPath s : path]
	
instance PathNode TreeGraph
where
	getPathNode :: TreeGraph GPath -> GPath
	getPathNode (TreeGraph graph source sink) path = getPathNode (fromJust (getNodeData source graph)) path

instance PathNode TreeGraphNode
where
	getPathNode :: TreeGraphNode GPath -> GPath
	getPathNode (TGNode node)         path = getPathNode node path 
	getPathNode (TGSubgraph subgraph) path = getPathNode subgraph path
	
parseChild :: a (GParseState b) -> GParseState b| PathNode a
parseChild child (GParseState f) = GParseState (\path = f (getPathNode child path))

parseMap :: (a -> GParseState b) [a] -> GParseState [b]
parseMap _ []     = ret []
parseMap f [x:xs] = f x >>> \x` = 
				                  parseMap f xs >>> \xs` = 
                                  ret [x`:xs`]

parseChildMap :: (a -> GParseState b) [a] -> GParseState [b]| PathNode a
parseChildMap f [] = ret []
parseChildMap f [x:xs] = parseChild x (f x) >>> \x` ->
						 parseChildMap f xs >>> \xs` ->
						 ret [x`:xs`]

orElse :: (GParseState a) (GParseState a) -> GParseState a
orElse (GParseState m) (GParseState k) = 
	GParseState (\p = case (m p) of
		GSuccess a    = GSuccess a
		GError errors = k p)

parseError :: String -> GParseState a
parseError message = GParseState (\path = GError [(path,message)])

parseErrorInChild :: a String -> GParseState b| PathNode a
parseErrorInChild child message = parseChild child (parseError message)

parseErrorInChildren :: [a] String -> GParseState b| PathNode a
parseErrorInChildren children message = GParseState (\path = GError [(getPathNode child path, message) \\ child <- children])

getCurrentPath :: GParseState GPath
getCurrentPath = GParseState (\path = GSuccess path)

runParse :: (GParseState a) -> GParseResult a
runParse (GParseState f) = f []

//-------------------------------------------------------------------------------------------
//Top-level conversion function

gToAModule :: !GModule !GinConfig !*World -> (GParseState AModule, *World)
gToAModule gmod =: { moduleKind = GCleanModule _ } config world
	= (parseError "Module does not have a graphical representation", world)
gToAModule gmod =: { moduleKind = GGraphicalModule definitions } config world
	# (res, world) = importModules config gmod.GModule.imports world
	| isError res = (parseError (fromError res), world)
	# imports = fromOk res
	# bindings = map getDefinitionBinding definitions
				 ++ (flatten o map getModuleBindings) imports
	= ( parseMap (gToADefinition bindings) definitions >>> \definitions = 
	    ret	{ AModule
			| name = gmod.GModule.name
			, types = gmod.GModule.types
			, imports = gmod.GModule.imports
			, definitions = definitions
			}
	  , world)

gToADefinition :: !Bindings !GDefinition -> GParseState ADefinition
gToADefinition bindings gdef
# graph = GGraphExpression (oryxDiagramToGraph bindings gdef.GDefinition.body)
= gToAExpression bindings graph >>> \body =
  ret { ADefinition 
      | name         = gdef.GDefinition.declaration.GDeclaration.name
      , formalParams = gdef.GDefinition.declaration.GDeclaration.formalParams
      , returnType   = gdef.GDefinition.declaration.GDeclaration.returnType
      , body         = body        
      , locals       = []
      }
      
gToAExpression :: !Bindings !GExpression -> GParseState (AExpression Void)
gToAExpression bindings (GUndefinedExpression) = parseError "Undefined expression"
gToAExpression bindings (GGraphExpression graph) = graphToAExpression bindings graph
gToAExpression bindings (GListExpression gexps) =
    parseMap (gToAExpression bindings) gexps >>> \aexps = 
    ret (List aexps)
gToAExpression bindings (GListComprehensionExpression glc) = 
    gToAListComprehension bindings glc >>> \alc = ret (ListComprehension alc)
gToAExpression bindings (GCleanExpression text) | size text == 0 = parseError "Clean expression is empty"
gToAExpression bindings (GCleanExpression text)                  = ret (Unparsed text)

putPathContext :: (AExpression Void) -> GParseState (AExpression Void)
putPathContext exp = getCurrentPath >>> \path -> ret (PathContext path exp)

gToAMaybeExpression :: !Bindings !(Maybe GExpression) -> GParseState (Maybe (AExpression Void))
gToAMaybeExpression bindings (Just exp) = gToAExpression bindings exp >>> \exp` = ret (Just exp`)
gToAMaybeExpression bindings Nothing = ret Nothing
    
gToAListComprehension :: !Bindings !GListComprehension -> GParseState (AListComprehension Void)
gToAListComprehension bindings glc = 
    gToAExpression bindings glc.GListComprehension.output >>> \output` =
    gToAExpression bindings glc.GListComprehension.input >>> \input` = 
    ret { output = output`
        , generators = NestedGeneratorList [Generator glc.GListComprehension.selector input`]
        , guards = map Unparsed (maybeToList glc.GListComprehension.guard)
        }

//--------------------------------------------------------------------------------------------------------------
//GGraph decomposition

graphToAExpression :: !Bindings !GGraph -> GParseState (AExpression Void)
graphToAExpression bindings graph =
	toTreeGraph graph >>> \tg ->
	ret (decompose bindings (tg)) >>> \tg ->
	treeGraphToAExpression bindings tg

:: TreeGraph = TreeGraph (Graph TreeGraphNode GEdge) NodeIndex NodeIndex
:: TreeGraphNode = TGNode GNode | TGSubgraph TreeGraph

isNode :: TreeGraphNode -> Bool
isNode (TGNode _) = True
isNode _          = False

isSubgraph :: TreeGraphNode -> Bool
isSubgraph (TGSubgraph _) = True
isSubgraph _             = False

fromNode :: TreeGraphNode -> GNode
fromNode (TGNode n) = n

fromSubgraph :: TreeGraphNode -> TreeGraph
fromSubgraph (TGSubgraph t) = t

toTreeGraph :: GGraph -> GParseState TreeGraph
toTreeGraph (GGraph graph)
# sources = filterNodes (\pred _ _ -> isEmpty pred) graph 
| isEmpty sources = parseError "No source node found"
| (not o isEmpty) (tl sources) = parseErrorInChildren (getNodes sources graph) "Source node is not unique" 
# sinks = filterNodes (\_ succ _ -> isEmpty succ) graph 
| isEmpty sinks = parseError "No sink node found"
| (not o isEmpty) (tl sinks) = parseErrorInChildren (getNodes sinks graph) "Sink node is not unique" 
= ret (TreeGraph (mapNodes (\n -> TGNode n) graph) (hd sources) (hd sinks))

edgePairs :: (Graph n e) EdgeIndex -> [(EdgeIndex,EdgeIndex)]
edgePairs graph startEdge = (diag2 (successorEdges startEdge graph) (predecessorEdges startEdge graph))

decompose :: Bindings TreeGraph -> TreeGraph
decompose bindings tg=:(TreeGraph g source sink)
	| nodeCount g == 1 = tg //Trivial graph: No decomposition possible
	# loop = (sink,source) 
	# pairs = edgePairs g loop
	= decomp (edgePairs g loop) loop (TreeGraph (addEdge emptyEdge loop g) source sink)
where
	decomp :: [(EdgeIndex,EdgeIndex)] EdgeIndex TreeGraph -> TreeGraph
	decomp [] loop (TreeGraph g source sink) = TreeGraph (removeEdge loop g) source sink
	decomp [(fromEdge,toEdge):edges] loop tg=:(TreeGraph g source sink)
		| not (   isParallelBinding (nodeName (fst fromEdge) g) (nodeName (snd toEdge) g) bindings
			   || isParallelBinding (nodeName (snd fromEdge) g) (nodeName (fst toEdge) g) bindings
			  ) = decomp edges loop tg
		# fromData = fromJust (getEdgeData fromEdge g)
		  toData   = fromJust (getEdgeData toEdge g)
		# comps = components (removeEdge fromEdge (removeEdge toEdge g))
		| isEmpty (tl comps) = decomp edges loop tg //try next pair
		# [g,s:_] = comps
		# (g,s) = if (nodeExists source g) (g,s) (s,g)
		| isTrivialGraph s = decomp edges loop tg
		# g = removeEdge loop g
		# (newNode,g) = addNode (TGSubgraph(decompose bindings (TreeGraph s (snd fromEdge) (fst toEdge)))) g //decompose subgraph s recursively
		# g = addEdge fromData (fst fromEdge,newNode) g
		# g = addEdge toData (newNode, snd toEdge) g
		# source = if (source == snd fromEdge) newNode source
		# sink = if (sink == fst toEdge) newNode sink
		= decompose bindings (TreeGraph g source sink)

treeGraphNodeToAExpression :: !Bindings !TreeGraphNode -> GParseState (AExpression Void)
treeGraphNodeToAExpression bindings (TGNode node) = nodeToAExpression bindings node
treeGraphNodeToAExpression bindings (TGSubgraph graph) = treeGraphToAExpression bindings graph

treeGraphToAExpression :: !Bindings !TreeGraph -> GParseState (AExpression Void)
treeGraphToAExpression bindings tg=:(TreeGraph graph source sink)
	| isEmptyGraph graph = parseErrorInChild tg "Empty graph"
	//Trivial subgraphs
	| isTrivialGraph graph
		= treeGraphNodeToAExpression bindings (fromJust (getNodeData (hd (nodeIndices graph)) graph))
	//Structured parallel subgraphs
	# mbParallel = parallelDecompose bindings tg
	| isJust mbParallel = parallelToAExpression bindings (fromJust mbParallel)
	//Sequential subgraphs
	| isSequential tg = sequenceToAExpression bindings tg
	//Other graphs
	= parseErrorInChild tg "Mapping not supported"

nodeToAExpression :: !Bindings !GNode -> GParseState (AExpression Void)
nodeToAExpression bindings node = 
	parseChild node
		(	( getNodeBinding node.GNode.name bindings >>> \nb = 
		      case nb.NodeBinding.parameterMap of 
		        NBBuiltIn = parseError "Node not allowed here"
		        NBPrefixApp = 
		            if (isEmpty node.GNode.actualParams)
		                (ret (Var node.GNode.name))
		                (argsToExpressions nb >>> \exps = ret (App [(Var node.GNode.name) : exps]))
				NBInfixApp fix prio = 
					argsToExpressions nb >>> \[exp1, exp2: _] -> 
					ret (AppInfix node.GNode.name fix prio exp1 exp2)
			) >>> putPathContext
		)
	where
		argsToExpressions :: NodeBinding -> GParseState [AExpression Void]
		argsToExpressions nb = parseChildMap (\(_,param) -> gToAExpression bindings param >>> putPathContext) 
	                	[(fromMaybe formalParam.GFormalParameter.name formalParam.GFormalParameter.title, actualParam) 
	                	\\ formalParam <- nb.NodeBinding.declaration.GDeclaration.formalParams 
	                	 & actualParam <- node.GNode.actualParams
	                	]

//-------------------------------------------------------------------------------------------
//Mapping of parallel subgraphs

parallelDecompose :: !Bindings !TreeGraph -> Maybe (GNode, GNode, [TreeGraphNode])
parallelDecompose bindings (TreeGraph graph sourceIndex sinkIndex)
	# branchIndices = directSuccessors sourceIndex graph
	| sort branchIndices <> sort (directPredecessors sinkIndex graph) = Nothing
	| not (isParallelBinding (nodeName sourceIndex graph) (nodeName sinkIndex graph) bindings) = Nothing
	# source = fromNode (fromJust (getNodeData sourceIndex graph))
	# sink = fromNode (fromJust (getNodeData sinkIndex graph))
	= Just (source, sink, [ fromJust (getNodeData n graph) \\ n <- branchIndices])

parallelToAExpression :: !Bindings (GNode, GNode, [TreeGraphNode]) -> GParseState (AExpression Void)
parallelToAExpression bindings (split, merge, branches) =
    parseChild split (
        getParallelBinding split.GNode.name merge.GNode.name bindings >>> \pb = 
        checkNrBranches pb (length branches) >>> \_ =
        setParallelParams bindings (split,merge) branches pb.ParallelBinding.parameterMap >>>
        putPathContext
    )

checkNrBranches :: !ParallelBinding !Int -> GParseState Void
checkNrBranches pb i = case pb.fixedNrBranches of
    Just n | n == i = ret Void
    Just n          = parseError ("(" +++ pb.split.GDeclaration.name  +++ "," +++ pb.merge.GDeclaration.name 
                      +++ ") must have " +++ fromInt n +++ " branches")
    Nothing = ret Void

setParallelParams :: !Bindings !(GNode, GNode) ![TreeGraphNode] !(AExpression PBParameter) -> GParseState (AExpression Void)
setParallelParams _ _ _ (Unparsed s) = ret (Unparsed s)
setParallelParams _ _ _ (Lit s) = ret (Lit s)
setParallelParams _ _ _ (Var i) = ret (Var i)
setParallelParams bindings (split,merge) branches (App exps) =
	parseMap (setParallelParams bindings (split,merge) branches) exps >>> \exps` = 
    ret (App exps`)
setParallelParams bindings (split,merge) branches (AppInfix i fix pred exp1 exp2) =
	setParallelParams bindings (split,merge) branches exp1 >>> \exp1` =
	setParallelParams bindings (split,merge) branches exp2 >>> \exp2` =
    ret (AppInfix i fix pred exp1` exp2`)
setParallelParams bindings (split,merge) branches (Case exp casealts) =
	setParallelParams bindings (split,merge) branches exp >>> \exp` =
	parseMap (setParallelParamsCase bindings (split,merge) branches) casealts >>> \casealts` =
	ret (Case exp` casealts`)
setParallelParams bindings (split,merge) branches (Lambda pat exp) =
    setParallelParams bindings (split,merge) branches exp >>> \exp` =
    ret (Lambda pat exp`)
setParallelParams bindings (split,merge) branches (Tuple exps) = 
    parseMap (setParallelParams bindings (split,merge) branches) exps >>> \exps` =
        ret (Tuple exps`)
setParallelParams bindings (split,merge) branches (List exps) = 
    parseMap (setParallelParams bindings (split,merge) branches) exps >>> \exps` =
        ret (List exps`)
setParallelParams bindings (split,merge) branches (Extension ext) = case ext of
    PBSplitParameter i = gToAExpression bindings (split.GNode.actualParams !! i)
    PBMergeParameter i = gToAExpression bindings (merge.GNode.actualParams !! i)
    PBBranch i = 
    	case branches of
    		[TGNode node] = if (node.GNode.name == "list comprehension")
    			(parseError "List comprehension not allowed here")
    			(nodeToAExpression bindings node)
    		_	= (treeGraphNodeToAExpression bindings (branches !! i))
	PBBranchList = 
		case branches of
			[TGNode node]
				= if (node.GNode.name == "list comprehension") (setListComprehension node) setList
			_	= setList
		where
		setListComprehension node = 
			getNodePattern node 0 >>> \generatorPattern -> 
			gToAExpression bindings (node.GNode.actualParams !! 1) >>> \generatorExpression ->
			getNodePattern node 2 >>> \guard ->
			gToAExpression bindings (node.GNode.actualParams !! 3) >>> \output ->
			ret
				( ListComprehension 
					{ AListComprehension
					| output = output
					, generators = NestedGeneratorList [Generator generatorPattern generatorExpression]
					, guards = if (trim guard == "") [] [Unparsed guard]
					}
				)
		setList = parseMap (treeGraphNodeToAExpression bindings) branches >>> \branches` ->
     				      ret (List branches`)

setParallelParamsCase :: !Bindings !(GNode, GNode) ![TreeGraphNode] !(ACaseAlt PBParameter) -> GParseState (ACaseAlt Void)
setParallelParamsCase bindings (split,merge) branches (CaseAlt pat exp) =
	setParallelParams bindings (split,merge) branches exp >>> \exp` =
	ret (CaseAlt pat exp`)

//-------------------------------------------------------------------------------------------
//Mapping of sequential subgraphs

isSequential :: TreeGraph -> Bool
isSequential graph = True //TODO: check if graph does not contain any parallel split or parallel merge node.

sequenceToAExpression :: !Bindings !TreeGraph -> GParseState (AExpression Void) 	
sequenceToAExpression bindings tg
	= parseChild tg (sequenceToAExpression` bindings tg)

sequenceToAExpression` :: !Bindings !TreeGraph -> GParseState (AExpression Void)
sequenceToAExpression` bindings tg=:(TreeGraph graph source sink)
	# mergeNodes = filter (\n -> nodeName n graph == "case merge"
	                       && not (isPathToMergeSink graph n sink)) (nodeIndices graph)
	# nvMap = buildNodeVarMap tg mergeNodes
	= parseMap (\i -> seqNodeToAExpr (hd (directSuccessors i graph)) sink mergeNodes nvMap >>> \iexp -> 
	            ret ("f" +++ toString i +++ " " +++ unwords (fromJust (get i nvMap)), iexp)) mergeNodes >>> \letExps ->
	  seqNodeToAExpr source sink mergeNodes nvMap >>> \exp -> 
	  ret if (isEmpty letExps) exp (Let letExps exp)
where
	seqNodeToAExpr :: !NodeIndex !NodeIndex ![NodeIndex] !NodeVarMap -> GParseState (AExpression Void)
	seqNodeToAExpr current sink mergeNodes nvMap
		# nodeData = (fromJust (getNodeData current graph))
		| current == sink = treeGraphNodeToAExpression bindings nodeData                    //1
		| isNode nodeData
			# node = fromNode nodeData
			# nodename = node.GNode.name
			| isMember current mergeNodes = 
				let f = Var ("f" +++ toString current) in 
				ret (case fromMaybe [] (get current nvMap) of
						[] = f
						xs = App [f : map Var xs]
					)
			| nodename == "case split" = caseNodeToAExpr current sink mergeNodes nvMap //3
			| nodename == "let" = letNodeToAExpr current sink mergeNodes nvMap      //4
			= seqNode` nodeData 
		= seqNode` nodeData
	where
		seqNode` nodeData
			# successors = directSuccessors current graph
			| not (isEmpty (tl successors)) = parseErrorInChild nodeData "Node cannot be used as split node"    //7
			# successor = hd successors
			| isPathToMergeSink graph successor sink = seqNodeToAExpr current current mergeNodes nvMap
			# edgePattern = (fromJust (getEdgeData (current, successor) graph)).pattern                   //5,6
		    = treeGraphNodeToAExpression bindings nodeData        >>> \first`  -> 
		      seqNodeToAExpr successor sink mergeNodes nvMap >>> \second` ->
			    ret (case edgePattern of 
		    			Just p  = (AppInfix ">>=" Infixl 1 first` (Lambda p second`))
				    	Nothing = (AppInfix ">>|" Infixl 1 first` second`)
			    	)

	caseNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] NodeVarMap -> GParseState (AExpression Void)
	caseNodeToAExpr current sink mergeNodes nvMap
		# nodeData = fromJust (getNodeData current graph)
		# node = fromNode nodeData
		# successors = directSuccessors current graph
		| isEmpty successors = parseErrorInChild node "Missing node(s) after case split"
		| or [ isPathToMergeSink graph s sink \\ s <- successors ] = parseErrorInChild node "Missing task in branch" //TODO: Highlight branch
		# caseExpression = node.GNode.actualParams
		= parseChild node (gToAExpression bindings (node.GNode.actualParams !! 0)) >>> \caseExpr -> 
		  parseMap (\n -> seqNodeToAExpr n sink mergeNodes nvMap >>> \exp -> 
		            ret (fromJust (getEdgeData (current,n) graph), exp)) successors >>> \caseAlts ->
		  if (length (filter (\{pattern} -> isNothing pattern) (map fst caseAlts)) > 1)
			(parseErrorInChild node "A case expression can have at most one default case")
			//"otherwise" alternative is put at the end of the list
			//TODO: Sort according to diagram layout.
			(ret (let sortAlts = filter (not o isEmptyEdge o fst) caseAlts ++ filter (isEmptyEdge o fst) caseAlts
			    in Case caseExpr (map mkCaseAlt sortAlts)))
	where
		isEmptyEdge :: GEdge -> Bool
		isEmptyEdge {pattern = Nothing  } = True
		isEmptyEdge {pattern = Just pat } = size (trim pat) == 0
	
		mkCaseAlt :: (GEdge, AExpression Void) -> ACaseAlt Void
		mkCaseAlt ({pattern = Just pat}, exp) = CaseAlt pat exp
		mkCaseAlt ({pattern = Nothing }, exp)  = CaseAlt "otherwise" exp
		  
	letNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] NodeVarMap -> GParseState (AExpression Void)
	letNodeToAExpr current sink mergeNodes nvMap
		# nodeData = (fromJust (getNodeData current graph))
		# node = fromNode nodeData
		# successors = directSuccessors current graph
		| isEmpty successors = parseErrorInChild node "Missing node after let"
		| not (isEmpty (tl successors)) = parseErrorInChild node "Let cannot be used as split node"
		| isPathToMergeSink graph (hd successors) sink = parseErrorInChild node "Missing task in branch"
		= getNodePattern node 0 >>> \pat ->
		  gToAExpression bindings (node.GNode.actualParams !! 1) >>> \exp -> 
		  seqNodeToAExpr (hd successors) sink mergeNodes nvMap >>> \inExpr ->
		  ret (App [Lambda pat inExpr, exp])

nodeName :: NodeIndex (Graph TreeGraphNode e) -> String
nodeName nodeIndex graph = case fromJust (getNodeData nodeIndex graph) of
	TGNode{ GNode | name } = name
	_                     = ""

getNodePattern :: GNode Int -> GParseState String
getNodePattern node index = parseChild node (getTextExpr (node.GNode.actualParams !! index))
where
	getTextExpr :: GExpression -> GParseState String
	getTextExpr (GCleanExpression exp) | exp == "" = parseError "Pattern is empty"
									   | otherwise = ret exp
	getTextExpr f = parseError "Expected: textual expression"
	
getNodes :: [NodeIndex] (Graph n e) -> [n]
getNodes nodes graph = catMaybes [ getNodeData n graph\\ n <- nodes ]

//--------------------------------------------------------------------------------------------------
isPathToMergeSink :: (Graph TreeGraphNode GEdge) NodeIndex NodeIndex -> Bool
isPathToMergeSink graph current sink
	# nodeData = fromJust (getNodeData current graph)
	| isSubgraph nodeData = False
	# node = fromNode nodeData
	| node.GNode.name <> "case merge" = False
	| current == sink = True
	= case directSuccessors current graph of
		[n] = isPathToMergeSink graph n sink
		_   = False

:: NodeVarMap :== Map NodeIndex Vars

buildNodeVarMap :: TreeGraph [NodeIndex] -> NodeVarMap
buildNodeVarMap (TreeGraph graph source sink) mergeNodes
	= fromList [(n, getNodeVars n) \\ n <- mergeNodes ]
where
	getNodeVars :: NodeIndex -> Vars
	getNodeVars node
		# freeVars = freeVarsAfter graph node
		# boundVars = mergeVars [boundVarsBefore graph (p,node) \\ p <- directPredecessors node graph]
		= [ v \\ v <- freeVars | isMember v boundVars ]

freeVarsAfter :: (Graph TreeGraphNode GEdge) NodeIndex -> Vars
freeVarsAfter graph startNode = freeVarsAfter` [(startNode,emptyScope)] newMap
where
	freeVarsAfter` :: [(NodeIndex, Scope)] (Map NodeIndex Scope) -> Vars
	freeVarsAfter` [] visitedMap = []
	freeVarsAfter` [(n,scope):ns] visitedMap
		# newVars = [v \\ v <- freeVarsInNode (fromJust (getNodeData n graph)) | not (isMember v scope)]
		# toVisit = [(n`, addVars (boundVarsInEdge (n,n`) graph) scope)
					 \\ n` <- directSuccessors n graph
					] ++ ns
		= case get n visitedMap of
			Just visitedScope = 
				if (isEmpty [ v \\ v <- visitedScope | not (isMember v scope) ]) 
					[]
					(addVars newVars (freeVarsAfter` toVisit visitedMap))
			Nothing = addVars newVars (freeVarsAfter` toVisit (put n scope visitedMap))

freeVarsInNode :: TreeGraphNode -> Vars
freeVarsInNode (TGNode node) = mergeVars (map freeVarsInExpression node.GNode.actualParams) 
freeVarsInNode (TGSubgraph (TreeGraph graph source sink)) = freeVarsAfter graph source

freeVarsInExpression :: GExpression -> Vars
freeVarsInExpression GUndefinedExpression = emptyVars
freeVarsInExpression (GGraphExpression graph)
	# result = runParse (toTreeGraph graph)
	| isParseError result = emptyVars
	# (TreeGraph graph` source sink) = getParseSuccess result
	= freeVarsAfter graph` source
freeVarsInExpression (GListExpression items) = mergeVars (map freeVarsInExpression items)
freeVarsInExpression (GListComprehensionExpression { GListComprehension | output, input, guard} )
	= mergeVars	[ freeVarsInExpression output
				, freeVarsInExpression input
				, case guard of
				  	Just g = freeVarsInCleanExpression g
				  	Nothing = emptyVars
				]
freeVarsInExpression (GCleanExpression clean) = freeVarsInCleanExpression clean

boundVarsBefore :: (Graph TreeGraphNode GEdge) EdgeIndex -> Vars
boundVarsBefore graph startEdge = boundVarsBefore` [startEdge] []
where
	boundVarsBefore` :: [EdgeIndex] [EdgeIndex] -> Vars
	boundVarsBefore` [] visited = []
	boundVarsBefore` [e:es] visited
		| isMember e visited = boundVarsBefore` es visited
		# preds = directPredecessors (fst e) graph
		= addVars 
			(boundVarsInEdge e graph)
			(boundVarsBefore` ([(p,fst e) \\ p <- preds] ++ es) [e:visited])

boundVarsInEdge :: EdgeIndex (Graph TreeGraphNode GEdge) -> Vars
boundVarsInEdge edgeIndex graph
	# edgeData = fromJust (getEdgeData edgeIndex graph)
	= case edgeData.GEdge.pattern of
		Just p = freeVarsInCleanExpression p
		Nothing = 
			//Check for bound vars in preceding let-node
			case fromJust (getNodeData (fst edgeIndex) graph) of
			TGNode { GNode | name = "let", actualParams } = freeVarsInExpression (actualParams !! 0)
			_ = emptyVars

freeVarsInCleanExpression :: !String -> Vars
freeVarsInCleanExpression input
# mExpr = parseExpressionUnsafe input
| isNothing mExpr = []
= freeVars emptyScope (fromJust mExpr) 
where
	freeVars :: Scope ParsedExpr -> Vars
	freeVars scope (PE_List exprs) = mergeVars (map (freeVars scope) exprs)
	freeVars scope (PE_Ident ident) = if (inScope ident.id_name scope) [] [ident.id_name]
	freeVars scope (PE_Basic _) = []
//	freeVars scope (PE_Bound boundExpr) = abort "freeVars PE_Bound"
//	freeVars scope (PE_Lambda _ pat exp _) = abort "freeVars PE_Lambda"
	freeVars scope (PE_Tuple exprs) = flatten (map (freeVars scope) exprs)
//	freeVars scope (PE_Record expr optionalRecordName fieldAssignments) = abort "freeVars PE_Record"
//	freeVars scope (PE_ArrayPattern elemAssignments) = abort "freeVars PE_UpdateComprehension"
//	freeVars scope (PE_UpdateComprehension expr1 expr2 expr3 qualifiers) = abort "freeVars PE_ArrayDenot"
//	freeVars scope (PE_ArrayDenot kind exprs) = abort "freeVars PE_ArrayDenot"
	freeVars scope (PE_Selection _ expr selections) = freeVars scope expr
//	freeVars scope (PE_Update expr1 selections expr2) = abort "freeVars PE_Update"
//	freeVars scope (PE_Case _ e alts) = abort "freeVars PE_Case"
	freeVars scope (PE_If _ b e1 e2) = mergeVars [freeVars scope e \\ e <- [b,e1,e2]]
//	freeVars scope (PE_Let b defs expr) = abort "freeVars PE_Let"
//	freeVars scope (PE_ListCompr /*predef_cons_index:*/ i1 /*predef_nil_index:*/ i2 expr qualifiers) = abort "freeVars PE_ListCompr"
//	freeVars scope (PE_ArrayCompr arrayKind expr qualifiers) = abort "freeVars PE_ArrayCompr"
//	freeVars scope (PE_Sequ sequence) = abort "freeVars PE_Sequ"
//	freeVars scope (PE_WildCard) = abort "freeVars PE_Wildcard"
//	freeVars scope (PE_Field expr globalFieldSymbol /* Auxiliary, used during checking */) = abort "freeVars PE_Field"
//	freeVars scope (PE_QualifiedIdent ident string) = abort "freeVars PE_QualifiedIdent"
//	freeVars scope (PE_ABC_Code abc b) = abort "freeVars PE_ABC_Code"
//	freeVars scope (PE_Any_Code binding ident strings) = abort "freeVars PE_Any_Code"
//	freeVars scope (PE_DynamicPattern expr dyntype) = abort "freeVars PE_DynamicPattern"
//	freeVars scope (PE_Dynamic expr dyntype) = abort "freeVars PE_Dynamic"
//	freeVars scope (PE_Generic ident kind /* AA: For generics, kind indexed identifier */) = abort "freeVars PE_Generic"
//	freeVars scope (PE_TypeSignature arrayKind expr) = abort "freeVars PE_TypeSignature"
//	freeVars scope (PE_Empty) = abort "freeVars PE_Empty"
//	freeVars scope _ = abort "freeVars: not yet implemented"
	freeVars scope _ = []

//--------------------------------------------------------------------------------------------------
//Utilities

foldl1 :: (a a -> a) [a] -> a
foldl1 f [x:xs]  =  foldl f x xs

unwords :: [String] -> String
unwords [] = ""
unwords [x] = x
unwords [x:xs] = x +++ " " +++ unwords xs
