definition module Graph

from Maybe import ::Maybe

:: Graph n e

:: NodeIndex :== Int
:: EdgeIndex :== (!NodeIndex,!NodeIndex)

//Initialization
emptyGraph :: Graph n e
trivialGraph :: n -> (NodeIndex, Graph n e)

//Query
nodeIndices :: !(Graph n e) -> [NodeIndex]
edgeIndices :: !(Graph n e) -> [EdgeIndex]
nodeCount :: !(Graph n e) -> Int
edgeCount :: !(Graph n e) -> Int
directPredecessors :: !NodeIndex !(Graph n e) -> [NodeIndex]
directSuccessors :: !NodeIndex !(Graph n e) -> [NodeIndex]
predecessorEdges :: !EdgeIndex !(Graph n e) -> [EdgeIndex]
successorEdges :: !EdgeIndex !(Graph n e) -> [EdgeIndex]
getNodeData :: !NodeIndex !(Graph n e) -> Maybe n
getEdgeData :: !EdgeIndex !(Graph n e) -> Maybe e
filterNodes :: !([NodeIndex] [NodeIndex] n -> Bool) !(Graph n e) -> [NodeIndex]

//Predicates
isEmptyGraph :: !(Graph n e) -> Bool
isTrivialGraph :: !(Graph n e) -> Bool
nodeExists :: !NodeIndex !(Graph n e) -> Bool
edgeExists :: !EdgeIndex !(Graph n e) -> Bool

//Graph manipulation
addNode :: n (Graph n e) -> (NodeIndex, Graph n e)
addEdge :: e !EdgeIndex !(Graph n e) -> (Graph n e)
removeNode :: !NodeIndex !(Graph n e) -> Graph n e
removeEdge :: !EdgeIndex !(Graph n e) -> Graph n e
setNodeData :: !NodeIndex n !(Graph n e) -> Graph n e

//Mapping
mapNodes :: !(a -> b) !(Graph a e) -> (Graph b e)
mapEdges :: !(a -> b) !(Graph n a) -> (Graph n b)
mapIndices :: ![(!NodeIndex,!NodeIndex)] !(Graph n e) -> Graph n e

//Connectivity
components :: !(Graph n e) -> [Graph n e]
isConnected :: !(Graph n e) -> Bool

//Two-terminal graphs
sourceNode :: !(Graph n e) -> Maybe NodeIndex
sinkNode :: !(Graph n e) -> Maybe NodeIndex
