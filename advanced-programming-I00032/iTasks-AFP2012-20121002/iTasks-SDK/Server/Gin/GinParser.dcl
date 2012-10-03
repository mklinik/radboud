definition module GinParser

import StdList
import GenPrint
import JSON
import Monad

import GinConfig
import GinSyntax
import GinAbstractSyntax

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, :: StaticVisualizationMode
from iTasks import class iTask, generic gVisualizeText, generic gVisualizeHtml, generic gVisualizeEditor, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

:: GPath :== [GPathNode]

:: GPathNode	= NodePath GResourceId
				| ParamPath String
				| EdgePath GResourceId

:: GParseResult a = GSuccess a | GError [(GPath, String)]
:: GParseState a = GParseState (GPath -> GParseResult a)

derive class iTask GPathNode, GParseResult, GParseState

isParseError :: (GParseResult a) -> Bool
getParseSuccess :: (GParseResult a) -> a
getParseError :: (GParseResult a) -> [(GPath, String)] 

instance Monad GParseState

class PathNode a 
where
	getPathNode :: a GPath -> GPath

instance PathNode GNode
instance PathNode GEdge
instance PathNode (String,GExpression)

parseChild :: a (GParseState b) -> GParseState b | PathNode a
parseMap :: (a -> GParseState b) [a] -> GParseState [b]
parseChildMap :: (a -> GParseState b) [a] -> GParseState [b] | PathNode a
orElse :: (GParseState a) (GParseState a) -> GParseState a

parseError :: String -> GParseState a
parseErrorInChild :: a String -> GParseState b | PathNode a
parseErrorInChildren :: [a] String -> GParseState b | PathNode a

getCurrentPath :: GParseState GPath
//withPath :: GPath (GParseState a) -> GParseState a

runParse :: (GParseState a) -> GParseResult a

gToAModule :: !GModule !GinConfig !*World -> (GParseState AModule, *World)

//Utils

foldl1 :: (a a -> a) [a] -> a
