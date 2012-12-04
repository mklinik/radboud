implementation module iDataGraphvizForm

import StdEnv
import StdiData, iDataHandler
import Graphviz
import launch
import Directory

derive gUpd   [], Maybe, NodeState, //Digraph, 
						Arrow, ArrowShape, ArrowType, ClusterMode, Color, CompassPoint, DirType, DotPoint, 
						EdgeAttribute, EdgeStyle, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
						NodeAttribute, NodeDef, NodeShape, NodeStyle, OutputMode, Pad, PageDir, Pointf, 
						RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort
derive gPrint Maybe, Digraph, NodeState,
						Arrow, ArrowShape, ArrowType, ClusterMode, Color, CompassPoint, DirType, DotPoint, 
						EdgeAttribute, EdgeStyle, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
						NodeAttribute, NodeDef, NodeShape, NodeStyle, OutputMode, Pad, PageDir, Pointf, 
						RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort
derive gParse Maybe, Digraph, NodeState,
						Arrow, ArrowShape, ArrowType, ClusterMode, Color, CompassPoint, DirType, DotPoint, 
						EdgeAttribute, EdgeStyle, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
						NodeAttribute, NodeDef, NodeShape, NodeStyle, OutputMode, Pad, PageDir, Pointf, 
						RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort
derive gerda  Digraph, NodeState, 
						Arrow, ArrowShape, ArrowType, ClusterMode, Color, CompassPoint, DirType, DotPoint, 
						EdgeAttribute, EdgeStyle, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
						NodeAttribute, NodeDef, NodeShape, NodeStyle, OutputMode, Pad, PageDir, Pointf, 
						RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort
derive read   Maybe, Digraph, NodeState, 
						Arrow, ArrowShape, ArrowType, ClusterMode, Color, CompassPoint, DirType, DotPoint, 
						EdgeAttribute, EdgeStyle, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
						NodeAttribute, NodeDef, NodeShape, NodeStyle, OutputMode, Pad, PageDir, Pointf, 
						RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort
derive write  Maybe, Digraph, NodeState, 
						Arrow, ArrowShape, ArrowType, ClusterMode, Color, CompassPoint, DirType, DotPoint, 
						EdgeAttribute, EdgeStyle, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
						NodeAttribute, NodeDef, NodeShape, NodeStyle, OutputMode, Pad, PageDir, Pointf, 
						RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort

//	Specialization of iData for Digraph values:
gUpd {|Digraph|} (UpdSearch (UpdC node) 0) (Digraph name graphAtts nodeDefs _)
						= (UpdDone, Digraph name graphAtts nodeDefs nodeNr)
where
	nodeNr				= case [nr \\ NodeDef nr _ nodeAtts _ <- nodeDefs | not (isEmpty (filter (isNAtt_label node) nodeAtts))] of
							[nr:_]	= Just (Node nr)
							_		= Nothing
	isNAtt_label l (NAtt_label l`)
						= l==l`
	isNAtt_label _ _	= False
gUpd {|Digraph|} (UpdSearch other cnt) g
						= (UpdSearch other (cnt-1),g)
gUpd {|Digraph|} (UpdCreate l) _
						= (UpdCreate l,Digraph "" [] [] Nothing)
gUpd {|Digraph|} mode g	= (mode,g)

gForm {|Digraph|} (init,formid) hst
	# (value,hst)		= accWorldHSt (obtainValueFromConfig dot_exe_path_name) hst
	| isNothing value	= (result [ Txt ("Could not obtain "+++dot_exe_path_name+++" from "+++config_file_name+++".") ],hst)
	# exe				= fromJust value
	# hst				= appWorldHSt (ensureDirectory (target "")) hst		// PA++
	# (ok,hst)			= accWorldHSt (writefile (target (dotext name)) (printDigraph (enhanceDigraphWithLinks digraph))) hst
	| not ok			= (result [ Txt ("Could not write Digraph to "+++target (dotext name)+++".") ],hst)
	# ((ok,exit),hst)	= accWorldHSt (collect3 (launch exe (toGIF (target name)))) hst
	| not ok			= (result [ Txt ("Creation of "+++gifext (target name)+++" failed. Exit code = "+++toString exit+++".") ],hst)
	# ((ok,exit),hst)	= accWorldHSt (collect3 (launch exe (toMAP (target name) name))) hst
	| not ok			= (result [ Txt ("Creation of "+++mapext (target name)+++" failed. Exit code = "+++toString exit+++".") ],hst)
	# ((ok,lines),hst)	= accWorldHSt (collect3 (readfile (mapext map_source_name))) hst
	| not ok			= (result [ Txt ("Reading of "+++mapext (target name)+++" failed.") ],hst)
	# (cntr,hst)		= CntrHSt hst
	# lines				= map (enhanceMAPlineWithOnClickEvent cntr) lines
	| otherwise			= (result [ 
						           Img [ Img_Src    (gifext img_source_name)
						                , Img_Usemap ("#"+++name)
//						                , Img_Width  (Percent 100)
						                , Img_Height (Percent 60)
						                ]
						          : map InlineCode lines
						          ],incrHSt 1 hst)
where
	digraph				= formid.ival
	name				= formid.id
	clean_name			= clean_up name
	result html			= {changed=False,value=digraph,form=html}
	img_source_name		= ThisExe+++"/"+++name
	map_source_name		= ThisExe+++"/"+++name

	
	clean_up :: !String -> String
//	clean_up name		= name
	clean_up name		= {clean_char c \\ c<-: name}
	clean_char '.'		= 'x'
	clean_char '+'		= 'p'
	clean_char '-'		= 'm'
	clean_char c		= c

	enhanceMAPlineWithOnClickEvent :: !Int !String -> String
	enhanceMAPlineWithOnClickEvent cntr line
		| line%(0,5) == "<area "
			| size line <= 6 || isNothing href_bounds //|| isNothing title_bounds
						= line
			| otherwise	= line%(0,fst href-1)                                       +++ 
						  "onclick=\"toClean(this,'" +++ encodeTriplet(name,cntr,UpdC titletext) +++ "',true,false,false);\" " +++ 
						  "id=\""+++ encodeInputId(name,cntr,UpdC titletext) +++ "\"" +++
						  line%(snd href+1,size line-1)
		| line%(0,4) == "<map "
						= "<map id=\"" +++ name +++ "\" name=\"" +++ name +++ "\">\n"
		| otherwise
						= line
	where
		href_bounds		= boundsOfKeyValue "href="  line
		title_bounds	= boundsOfKeyValue "title=" line
		href			= fromJust href_bounds
		title			= fromJust title_bounds
		titletext		= line%(fst title+7,snd title-1)

boundsOfKeyValue :: !String !String -> Maybe (!Int,!Int)
boundsOfKeyValue key str
	= case [i \\ i<-[0..size str-size key] | str%(i,i+size key-1) == key] of
		[i : _]			= case [j \\ j<-[i..size str-1] | str.[j]=='\"'] of
							[_,close:_] = Just (i,close)
							otherwise	= Nothing
		otherwise		= Nothing

enhanceDigraphWithLinks :: !Digraph -> Digraph
enhanceDigraphWithLinks (Digraph name graphAtts nodeDefs selected)
	= Digraph name graphAtts 
		[  NodeDef nr st [ NAtt_URL ("http://localhost/"+++ThisExe) : nodeAtts ] edges 
		\\ NodeDef nr st nodeAtts edges <- nodeDefs
		] selected

obtainValueFromConfig :: !String !*env -> (!Maybe String,!*env) | FileSystem env
obtainValueFromConfig name env
	# (ok,file,env)		= fopen config_file_name FReadText env
	| not ok			= (Nothing,env)
	# (value,file)		= obtainValueFromFile name file
	# (ok,env)			= fclose file env
	| not ok			= (Nothing,env)
	| otherwise			= (value,  env)
where
	obtainValueFromFile :: !String !*File -> (!Maybe String,!*File)
	obtainValueFromFile name file
		# (lines,file)	= readlines file
		# value			= case [ skipSpace (line%(name_length,size line-2)) \\ line<-lines
						                                                     | line.[0] <> commentsymbol 
						                                                    && size line > name_length
						                                                    && line%(0,name_length-1) == name
						       ] of [v:_]	= Just v
						            _		= Nothing
		= (value,file)
	where
		name_length		= size name

config_file_name		:== "iDataGraphvizForm.config"
commentsymbol			:== '%'
dot_exe_path_name		:== "DOT_PATH"
target file				= MyAbsDir +++ ThisExe +++ "\\" +++ file
toGIF file				= "-Tgif -o "   +++ "\"" +++ gifext file +++ "\" \"" +++ dotext file +++ "\""
//toMAP file				= "-Tcmapx -o " +++ "\"" +++ mapext file +++ "\" \"" +++ dotext file +++ "\""
toMAP file name			= "-Tcmapx" +++ " -Glabel=" +++ name +++ " -o " +++ "\"" +++ mapext file +++ "\" \"" +++ dotext file +++ "\""
gifext file				= file +++ ".gif"
mapext file				= file +++ ".map"
dotext file				= file +++ ".dot"

//	Utility functions:
/*	ensureDirectory path env
		checks whether the directory at path exists. If so, no further actions are taken.
		If not, the directory is created.
*/
import StdDebug

ensureDirectory :: !String !*env -> *env | FileSystem env	// PA++
ensureDirectory pathname env
	# ((ok,path), env)	= pd_StringToPath pathname env
	| not ok			= env
	# ((err,info),env)	= getFileInfo path env
	| err<>NoDirError	= snd (createDirectory path env)
	| otherwise			= env

writefile :: !String ![String] !*env -> (!Bool,!*env) | FileSystem env
writefile fileName content env
	# (ok,file,env)		= fopen fileName FWriteText env
	| not ok			= (False,env)
	# file				= foldl (<<<) file content
	= fclose file env

readfile  :: !String !*env -> (!Bool,![String],!*env) | FileSystem env
readfile fileName env
	# (ok,file,env)		= fopen fileName FReadText env
	| not ok			= (False,[],env)
	# (content,file)	= readlines file
	# (ok,env)			= fclose file env
	= (ok,content,env)

readlines :: !*File -> (![String],!*File)
readlines file
	# (end,file)		= fend file
	| end				= ([],file)
	# (line, file)		= freadline file
	# (lines,file)		= readlines file
	= ([line:lines],file)

collect3 :: (.s -> (.a,.b,.s)) .s -> (.(.a,.b),.s)
collect3 f st
	# (a,b,st)			= f st
	= ((a,b),st)

skipSpace :: !String -> String
skipSpace str			= toString (dropWhile isSpace (fromString str))
