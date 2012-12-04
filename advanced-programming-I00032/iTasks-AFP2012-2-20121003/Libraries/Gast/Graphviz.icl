// Péter Diviánszky, 2007
// Code extended and adapted for using generics by Peter Achten, 2007

implementation module Graphviz

import StdArray, StdOverloaded, StdList, StdOrdList, StdTuple, StdString, StdBool, StdMisc
import StdMaybe, StdListExtensions
import GenLib
import ESMSpec

derive gEq    EdgeStyle, NodeStyle, DirType, NodeShape, Side, ArrowShape, Maybe, ArrowType, Arrow, Color
derive gPrint EdgeStyle, NodeStyle, DirType, NodeShape, Side, ArrowShape, Maybe, CompassPoint, StartStyle,
              ClusterMode, OutputMode, PageDir, RankDir, RankType
derive printNameValuePair GraphAttribute, NodeAttribute, EdgeAttribute

//	Almost regular toString instances:
instance toString EdgeStyle      where toString es  =  /*quote*/ (skipXXX_InConstructorName (printToString es))
instance toString NodeStyle      where toString ns  = quote (skipXXX_InConstructorName (printToString ns))
instance toString DirType        where toString dir = quote (skipXXX_InConstructorName (printToString dir))
instance toString NodeShape      where toString ns  = skipXXX_InConstructorName (printToString ns)
instance toString Side           where toString s   = skipXXX_InConstructorName (printToString s)
instance toString ArrowShape     where toString s   = skipXXX_InConstructorName (printToString s)
instance toString CompassPoint   where toString cp  = quote (skipXXX_InConstructorName (printToString cp))
instance toString ClusterMode    where toString cm  = quote (skipXXX_InConstructorName (printToString cm))
instance toString OutputMode     where toString om  = quote (skipXXX_InConstructorName (printToString om))
instance toString PageDir        where toString pd  = skipXXX_InConstructorName (printToString pd)
instance toString RankDir        where toString rd  = skipXXX_InConstructorName (printToString rd)
instance toString RankType       where toString rt  = skipXXX_InConstructorName (printToString rt)
instance toString StartStyle     where toString ss  = skipXXX_InConstructorName (printToString ss)
instance toString NodeAttribute  where toString na  = printNameValuePair{|*|} na
instance toString EdgeAttribute  where toString ea  = printNameValuePair{|*|} ea
instance toString GraphAttribute where toString ga  = printNameValuePair{|*|} ga
//	Less regular toString instances:
instance toString Arrow where
	toString {open,side,shape}	= if open "o" "" +++ if (isJust side) (toString (fromJust side)) "" +++ toString shape
instance toString ArrowType where
	toString {closest,furthest}	= quote (toString closest +++ if (isJust furthest) (toString (fromJust furthest)) "")
instance toString Color where
	toString (Color name)		= name

	toString (HSV h s v)		= "\"" $> toS h $> " " $> toS s $> " " $> toS v $> "\""
	where
		toS x
			| x<0.0 || x>1.0	= abort "HSV value out of range.\n" 
			| otherwise			= toString (toReal (toInt (1000.0*x)) / 1000.0)

	toString (RGB r g b)		= "\"#" $> toS r $> toS g $> toS b $> "\""
	where
		toS x 
			| x<0 || x>255		= abort "RGB value out of range.\n" 
			| otherwise			= toString [toC (x/16), toC (x rem 16)] 
		toC x 
			| x < 10			= toChar (x + fromChar '0')
			| otherwise			= toChar (x - 10 + fromChar 'A')
instance toString DotPoint where
	toString (DotPoint x y fix)	= x >$ "," >$ y >$ if fix "!" ""
instance toString LayerId where
	toString layerid			= case layerid of
									LayerAll      = "all"
									LayerNr   nr  = toString nr
									LayerName str = str
instance toString LayerList where
	toString (LayerList names)	= foldr (\next before -> before $> layersep $> next) "" names
instance toString LayerRange where
	toString (LayerRange id ids)= foldr (\next before -> before $> layersep $> next) (toString id) ids
instance toString Margin where
	toString margin				= case margin of
									SingleMargin a   = toString a
									DoubleMargin a b = a >$ "," $> b
instance toString Pad where
	toString pad				= case pad of
									SinglePad a   = toString a
									DoublePad a b = a >$ "," $> b
instance toString Pointf where
	toString (Pointf x y)		= quote (x >$ "," $> y)
instance toString Ratio where
	toString ratio				= case ratio of
									AspectRatio r = quote (toString r)
									R_fill        = quote "fill"
									R_compress    = quote "compress"
									R_expand      = quote "expand"
									R_auto        = quote "auto"
instance toString Rect where
	toString {llx,lly,urx,ury}	= llx >$ "," $> lly >$ "," $> urx >$ "," $> ury
instance toString Sizef where			// PA++
	toString (Sizef x y True)	= "\"" +++ toString x +++ "," +++ toString y +++ "!\""
	toString (Sizef x y False)	= "\"" +++ toString x +++ "," +++ toString y +++ "\""
instance toString StartType where
	toString {startStyle,startSeed}
								= if (isJust startStyle) (toString (fromJust startStyle)) "" +++ 
								  if (isJust startSeed)  (toString (fromJust startSeed )) ""
instance toString ViewPort where
	toString {vp_W,vp_H,vp_Z,vp_xy}
								= (vp_W >$ "," $> vp_H)                          +++ 
								  if (isJust vp_Z ) ("," $> (fromJust vp_Z )) "" +++
								  if (isJust vp_xy) ("," $> (fromJust vp_xy)) ""

//	Print name=value pairs for algebraic data types with unary data constructors in XXX_name constructor name format.
generic printNameValuePair a :: a -> String
printNameValuePair{|Int|}          x			= toString x
printNameValuePair{|Real|}         x			= toString x
printNameValuePair{|Char|}         x			= toString x
printNameValuePair{|String|}       x			= quote    x
printNameValuePair{|Bool|}         x			= firstCharLowerCase (toString x)
printNameValuePair{|UNIT|}         x			= ""
printNameValuePair{|PAIR|}   px py (PAIR x y)	= px x +++ " " +++ py y
printNameValuePair{|EITHER|} pl pr (LEFT   x)	= pl x
printNameValuePair{|EITHER|} pl pr (RIGHT  y)	= pr y
printNameValuePair{|OBJECT|} px    (OBJECT x)	= px x
printNameValuePair{|CONS of d|} px (CONS   x)	= skipXXX_InConstructorName d.gcd_name +++ "=" +++ px x
// Specializations of printNameValuePair:
printNameValuePair{|ArrowType|}    x			= toString x
printNameValuePair{|Color|}        x			= toString x
printNameValuePair{|ClusterMode|}  x			= toString x
printNameValuePair{|CompassPoint|} x			= toString x
printNameValuePair{|DirType|}      x			= toString x
printNameValuePair{|DotPoint|}     x			= toString x
printNameValuePair{|EdgeStyle|}    x			= toString x
printNameValuePair{|LayerList|}    x			= toString x
printNameValuePair{|LayerRange|}   x			= toString x
printNameValuePair{|Margin|}       x			= toString x
printNameValuePair{|NodeShape|}    x			= toString x
printNameValuePair{|NodeStyle|}    x			= toString x
printNameValuePair{|OutputMode|}   x			= toString x
printNameValuePair{|Pad|}          x			= toString x
printNameValuePair{|PageDir|}      x			= toString x
printNameValuePair{|Pointf|}       x			= toString x
printNameValuePair{|RankDir|}      x			= toString x
printNameValuePair{|RankType|}     x			= toString x
printNameValuePair{|Ratio|}        x			= toString x
printNameValuePair{|Rect|}         x			= toString x
printNameValuePair{|Sizef|}        x			= toString x		// PA++
printNameValuePair{|StartType|}    x			= toString x
printNameValuePair{|ViewPort|}     x			= toString x

instance == EdgeStyle where (==) a b			= gEq{|*|} a b
instance == NodeStyle where (==) a b			= gEq{|*|} a b
instance == DirType   where (==) a b			= gEq{|*|} a b
instance == NodeShape where (==) a b			= gEq{|*|} a b
instance == ArrowType where (==) a b			= gEq{|*|} a b
instance == Color     where (==) a b			= gEq{|*|} a b


digraphTitle :: !Digraph -> String
digraphTitle (Digraph title _ _ _)				= title

digraphAtts :: !Digraph -> [GraphAttribute]
digraphAtts (Digraph _ atts _ _)				= atts

digraphNodes :: !Digraph -> [NodeDef]
digraphNodes (Digraph _ _ nodes _)				= nodes

digraphSelectedItem :: !Digraph -> Maybe SelectedItem
digraphSelectedItem (Digraph _ _ _ selected)	= selected

pointNode :: [NodeAttribute]
pointNode										=: [NAtt_shape NShape_point]

hiddenNode :: [NodeAttribute]
hiddenNode										=: [NAtt_shape NShape_point,NAtt_style NStyle_invis]

commaseparatedlist :: [String] -> String
commaseparatedlist []							= ""
commaseparatedlist l							= "[" +++ (foldr (+++) "" (intersperse "," l)) +++ "]"

printDigraph :: !Digraph -> [String]
//printDigraph (Digraph title atts nodes _)		= map (\x->x+++"\n") (prelude title (graphAtts atts) (contents nodes))
printDigraph digraph							= case includeChanges digraph of
													Digraph title atts nodes _ -> map (\x->x+++"\n") (prelude title (graphAtts atts) (contents nodes))

includeChanges :: !Digraph -> Digraph
includeChanges dg=:(Digraph _ _ _ Nothing)		= dg
includeChanges (Digraph title atts nodes change)= Digraph title atts (map includeNodeChange nodes) Nothing
where
	(Node nr`)									= fromJust change
	
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

createGraphName :: !String -> String
createGraphName ""								= "G"
createGraphName x								= x

prelude :: !String ![String] ![String] -> [String]
prelude title graphAtts contents				= [ "digraph " +++ createGraphName title +++ " {"
												  , "label="   +++ quote title 
												  ]            ++ 
												  graphAtts    ++ 
												  contents     ++ 
												  [ "}" ]

graphAtts :: ![GraphAttribute] -> [String]
graphAtts graphAtts								= map printNameValuePair{|*|} graphAtts

contents :: ![NodeDef] -> [String]
contents nodeDefs								= map snd (mergeBy (\(x,_) (y,_)= x<y) nodes` edges`)
where
	(nodes,edges)								= unzip (mapSt f nodeDefs 1)
	where
		f (NodeDef id st na edges) num			= (	((num,id,na)
												    ,[(n,id,id`,ea) \\ (id`,ea)<- edges & n<-[num+1..]]
													)
												  , num + 1 + length edges
												  )
	
	nodes` = map (\(num, id, atts) = (num, id >$ commaseparatedlist (map toString atts))) nodes
	edges` = map (\(num,source,target,atts) = (num,source >$ "->" $> target >$ commaseparatedlist (map toString atts))) (flatten edges)


mkDigraph :: String (KnownAutomaton s i o,s,[s],[s],[SeenTrans s i o],[SeenTrans s i o]) -> Digraph | render, gEq{|*|} s & render, gEq{|*|} i & render, gEq{|*|} o
mkDigraph name (automaton,s_0,init_states,finished,issues,trace)
	= Digraph
		(remove_spaces name)
		graphAttributes
		(if (isEmpty automaton.trans)
			[NodeDef 0                  [NStAllEdgesFound (gisMember s_0 finished)] (nodeAttributes s_0 init_states (gisMember s_0 finished)) []]
			[NodeDef (nrOf automaton n) [NStAllEdgesFound (gisMember n   finished)] (nodeAttributes n   init_states (gisMember n   finished))
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
	graphAttributes				= [ GAtt_rankdir  RD_LR
								  , GAtt_size     		(Sizef 10.0 6.0 True)
								  , GAtt_bgcolor  		(Color "lightsteelblue")
								  , GAtt_ordering 		"out"
								  , GAtt_outputorder	OM_nodesfirst	// OM_edgesfirst	//  PK
								  ]
	nodeAttributes n init_states finished
								= (if (gisMember n init_states) [ NAtt_fillcolor act_backgr, NAtt_fontcolor act_txt ]
								  (if finished                 [ NAtt_fillcolor done_backgr,NAtt_fontcolor done_txt]
									                           [ NAtt_fillcolor def_backgr, NAtt_fontcolor def_txt ]
								  )) ++
						          [ NAtt_label     (render n)
						          , NAtt_style     NStyle_filled
						          , NAtt_shape     (if (n === s_0) NShape_doublecircle NShape_circle)
						          , NAtt_fontname  "Helvetica"
						          , NAtt_fontsize  fontsize
						          , NAtt_fixedsize True
						          , NAtt_width 1.0, NAtt_height 1.0
						          ]
	where
		( act_backgr, act_txt)	= active_state_color (length init_states)
		(done_backgr,done_txt)	= finished_state_color
		( def_backgr, def_txt)	= default_state_color

active_state_color :: !Int -> (!Color,!Color)
active_state_color nr	= (RGB 255 dim dim,Color "white")
where
	dim					= min 250 (255 - 255 / nr)

finished_state_color :: (!Color,!Color)
finished_state_color	= (Color "blue", Color "white")

default_state_color :: (!Color,!Color)
default_state_color		= (Color "grey90",Color "black")

fontsize = 11.0

//	Utility functions:

mapSt :: (a b -> (c,b)) [a] b -> [c]
mapSt f [] st			= []
mapSt f [h:t] st 
	#! (x, st)			= f h st
	= [x : mapSt f t st]

quote :: !String -> String
quote a					= "\"" $> (flatten (map f (fromString a))) >$ "\""
where
	f '\"'				= ['\\\"']
	f x					= [x]

skipXXX_InConstructorName :: !String -> String
skipXXX_InConstructorName str
	= case dropWhile ((<>) '_') [c \\ c<-:str] of
		[]				= str
		underscoreName	= str % (n-length underscoreName+1,n-1)
where
	n					= size str

firstCharLowerCase :: !String -> String
firstCharLowerCase str
	| size str > 0		= str := (0,toLower str.[0])
	| otherwise			= str

($>) infixr 5 :: !String !a -> String | toString a
($>) str arg			= str +++ toString arg

(>$) infixr 5 :: !a !String -> String | toString a
(>$) arg str			= toString arg +++ str

showList :: !(!String,!String,!String) ![a] -> String | render a
showList (open,close,delimit) []  = open +++ close
showList (open,close,delimit) [x] = open +++ render x +++ close
showList (open,close,delimit) xs  = open +++ foldr (\x str->render x+++delimit+++str) "" (init xs) +++ render (last xs) +++ close

remove_spaces :: !String -> String
remove_spaces str		= {c \\ c<-:str | not (isSpace c)}
