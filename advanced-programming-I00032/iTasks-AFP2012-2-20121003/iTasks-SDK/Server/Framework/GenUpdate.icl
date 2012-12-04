implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, Maybe, StdGeneric, StdEnum, Tuple, List_NG
import SystemTypes, Text, Util
from StdFunc import id, const, o
from UIDefinition import :: UISize(..)

:: DataPath = DataPath [Int]

defaultValue :: a | gUpdate{|*|} a
defaultValue = fst (gUpdate{|*|} UDCreate {searchPath = emptyDataPath, currentPath = emptyDataPath, consPath = [], update = JSONNull, oldMask = [Untouched], newMask = [], iworld = Nothing})

updateValueAndMask :: !DataPath !JSONNode !a !UpdateMask !*IWorld -> (!a,!UpdateMask,!*IWorld) | gUpdate{|*|} a
updateValueAndMask path update a oldMask iworld	
	# (a,ust=:{newMask,iworld}) = gUpdate{|*|} (UDSearch a) {searchPath = path, currentPath = startDataPath, consPath = [], update = update, oldMask = [oldMask], newMask = [], iworld = Just iworld}
	= (a,hd newMask,fromJust iworld)

appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
appIWorldUSt f ust=:{iworld} = case iworld of
	Just iworld	= {ust & iworld = Just (f iworld)}
	Nothing		= abort "no IWorld in USt"
	
accIWorldUSt :: !.(*IWorld -> *(!.a,!*IWorld))!*USt -> (!.a,!*USt)
accIWorldUSt f ust=:{iworld} = case iworld of
	Just iworld
		# (a,iworld) = f iworld
		= (a,{ust & iworld = Just iworld})
	Nothing
		= abort "no IWorld in USt"

//Generic updater
generic gUpdate a :: !(UpdateMode a) !*USt -> (!a,!*USt)

gUpdate{|UNIT|} _ ust = (UNIT,ust)

gUpdate{|OBJECT|} fx UDCreate ust=:{newMask}
	//Empty object mask
	# (nx,ust=:{newMask=objectMask}) = fx UDCreate {ust & newMask = []}
	= (OBJECT nx, {ust & newMask = newMask ++ objectMask})
gUpdate{|OBJECT of {gtd_num_conses,gtd_conses}|} fx (UDSearch (OBJECT x)) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		//Update is a constructor switch
		# (nx,ust) = fx UDCreate {ust & consPath = path}
		= (OBJECT nx, {ust & oldMask = om, currentPath = stepDataPath currentPath, newMask = appendToMask newMask (PartiallyTouched consMask)/*(toggleMask update)*/}) 
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = fx (UDSearch x) {ust & currentPath = shiftDataPath currentPath, oldMask = [cm:om], newMask = []}
		= (OBJECT nx, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask (PartiallyTouched childMask)})
	| otherwise
		//Not on the path, so just put back the current mask (cm)	
		= (OBJECT x, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}) 
where
	path = case update of
		JSONInt consIdx | consIdx < gtd_num_conses
				= consPath consIdx gtd_num_conses
		_		= []

	consPath i n
		| i >= n	
			= []
		| n == 1
			= []
		| i < (n/2)
			= [ ConsLeft : consPath i (n/2) ]
		| otherwise
			= [ ConsRight : consPath (i - (n/2)) (n - (n/2)) ]

	consMask = case update of
		JSONInt consIdx | consIdx < gtd_num_conses
			= repeatn ((gtd_conses !! consIdx).gcd_arity) Untouched
		_	= repeatn (hd gtd_conses).gcd_arity Untouched

gUpdate{|CONS|}	fx UDCreate	ust = appFst CONS (fx UDCreate ust)
gUpdate{|CONS of {gcd_arity}|}	fx (UDSearch (CONS c)) ust=:{oldMask}
	# (cm,om)	= popMask oldMask
	# (nx,ust)	= fx (UDSearch c) {ust & oldMask = childMasksN cm gcd_arity}
	= (CONS nx,ust)

gUpdate{|RECORD|} fx UDCreate ust = appFst RECORD (fx UDCreate ust)
gUpdate{|RECORD of {grd_arity}|} fx (UDSearch (RECORD x)) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = fx (UDSearch x) {ust & currentPath = shiftDataPath currentPath, oldMask = childMasksN cm grd_arity, newMask = []}
		= (RECORD nx, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask (PartiallyTouched childMask)})
	| otherwise
		//Not on the path, so just put back the current mask (cm)	
		= (RECORD x, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}) 

gUpdate{|FIELD|}	fx UDCreate				ust = appFst FIELD	(fx UDCreate ust)
gUpdate{|FIELD|}	fx (UDSearch (FIELD c))	ust = appFst FIELD	(fx (UDSearch c) ust)

gUpdate{|PAIR|} fx fy UDCreate ust
	# (nx,ust) = fx UDCreate ust
	# (ny,ust) = fy UDCreate ust
	= (PAIR nx ny, ust)
gUpdate{|PAIR|} fx fy (UDSearch (PAIR x y)) ust
	# (nx,ust) = fx (UDSearch x) ust
	# (ny,ust) = fy (UDSearch y) ust
	= (PAIR nx ny, ust)

gUpdate{|EITHER|} fx fy UDCreate ust=:{consPath}
	= case consPath of
		[ConsLeft:cl]
			# (nx,ust) = fx UDCreate {ust & consPath = cl}
			= (LEFT nx, ust)
		[ConsRight:cl]
			# (ny,ust) = fy UDCreate {ust & consPath = cl}
			= (RIGHT ny, ust)
		[]
			# (nx,ust) = fx UDCreate ust
			= (LEFT nx, ust)
gUpdate{|EITHER|} fx fy (UDSearch e) ust	
	= case e of
		LEFT x
			# (nx,ust) = fx (UDSearch x) ust
			= (LEFT nx, ust)
		RIGHT y
			# (ny,ust) = fy (UDSearch y) ust
			= (RIGHT ny,ust)
			
gUpdate{|Maybe|} _ UDCreate ust=:{newMask}
	//Specialized instance Maybe that chooses the non-recursive constructor 
	= (Nothing,{ust & newMask = appendToMask newMask Untouched})
gUpdate{|Maybe|} fx (UDSearch m) ust=:{currentPath,searchPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath && (update === JSONNull || update === JSONBool False)
		= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask Blanked, oldMask = om}) //Reset
	| otherwise
		= case m of
			Nothing
				| searchPath <== currentPath
					// Create a default value
					# (x,ust) 	= fx UDCreate ust
					// Search in the default value
					# (x,ust=:{newMask=[nmSearch:_]}) 	= fx (UDSearch x) {ust & currentPath = currentPath, searchPath = searchPath, update = update, oldMask = [Untouched], newMask = []}
					= (Just x, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask nmSearch, oldMask = om})
				| otherwise
					= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
			Just x
				# (x,ust) = fx (UDSearch x) ust //all mask transformations are made here..
				= (Just x,ust)

gUpdate{|[]|} _ UDCreate ust = basicCreate [] ust
gUpdate{|[]|} fx (UDSearch l) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (l,childMasks,ust)
		= case isNew currentPath searchPath (length l) of
			True
				# (nv,ust) = fx UDCreate {ust & oldMask = [], newMask = []}
				= (l++[nv],childMasksN cm (length l) ++ [Untouched],ust)
			False
				= (l,childMasksN cm (length l), ust)
	# (lx,ust=:{newMask=cMasks})	= updateElements fx l {ust & currentPath = shiftDataPath currentPath, oldMask = childMasks, newMask = []}
	# ust							= {ust & newMask = cMasks}
	| currentPath == searchPath
		//Process the reordering commands 
		# split = split "_" (fromMaybe "" (fromJSON update))
		# index = toInt (last split)
		# (lx,cMasks,ust) = case hd split of	
			"mup" = (swap lx index,swap cMasks index,ust) 
			"mdn" = (swap lx (index+1),swap cMasks (index+1),ust)
			"rem" = (removeAt index lx,removeAt index cMasks,ust)	
			"add"
				# (nv,ust) = fx UDCreate {ust & oldMask = [], newMask = []}
				= (insertAt (index+1) nv lx, insertAt (index+1) Untouched cMasks, ust)
			_ 	
				= (lx,cMasks,ust)
		= (lx,{ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (PartiallyTouched cMasks), oldMask = om})
	| otherwise
		= (lx, {ust & currentPath  = stepDataPath currentPath, newMask = appendToMask newMask (PartiallyTouched cMasks), oldMask = om})
where
	//Check if search path is equal or below [datapath:(length list)]
	isNew cp sp l = sp <== dataPathFromList [l:dataPathList cp] 
	
	updateElements fx []     ust 
		= ([],ust)
	updateElements fx [l:ls] ust
		# (lx,ust)  = fx (UDSearch l) ust
		# (lxs,ust) = updateElements fx ls ust
		= ([lx:lxs],ust)
		
	swap []	  _		= []
	swap list index
		| index == 0 			= list //prevent move first element up
		| index >= length list 	= list //prevent move last element down
		| otherwise				
			# f = list !! (index-1)
			# l = list !! (index)
			= updateAt (index-1) l (updateAt index f list)

gUpdate{|Display|}				fx mode										ust = wrapperUpdate fx mode fromDisplay Display ust
gUpdate{|Editable|}				fx mode										ust = wrapperUpdate fx mode fromEditable Editable ust
gUpdate{|Hidden|}				fx mode										ust = wrapperUpdate fx mode fromHidden Hidden ust
gUpdate{|VisualizationHint|} 	fx UDCreate									ust = wrapperUpdate fx UDCreate undef VHEditable ust 
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHEditable s))			ust = wrapperUpdate fx mode fromVisualizationHint VHEditable ust
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHDisplay s))			ust = wrapperUpdate fx mode fromVisualizationHint VHDisplay ust
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHHidden s))			ust = wrapperUpdate fx mode fromVisualizationHint VHHidden ust

wrapperUpdate fx mode get cons ust=:{USt|currentPath} = case mode of
	UDCreate
		= appFst cons (fx UDCreate ust)
	UDSearch w
		# (w,ust) = fx (UDSearch (get w)) ust
		= (cons w,{USt|ust & currentPath = stepDataPath currentPath})
		
gUpdate{|Int|}					mode ust = basicUpdateSimple mode 0 ust
gUpdate{|Real|}					mode ust = basicUpdateSimple mode 0.0 ust
gUpdate{|Char|}					mode ust = basicUpdateSimple mode ' ' ust
gUpdate{|Bool|}					mode ust = basicUpdateSimple mode False ust
gUpdate{|String|}				mode ust = basicUpdateSimple mode "" ust
gUpdate{|Note|}					mode ust = basicUpdateSimple mode (Note "") ust
gUpdate{|Username|}				mode ust = basicUpdateSimple mode (Username "") ust
gUpdate{|Password|}				mode ust = basicUpdateSimple mode (Password "") ust
gUpdate{|EUR|}					mode ust = basicUpdateSimple mode (EUR 0) ust
gUpdate{|USD|}					mode ust = basicUpdateSimple mode (USD 0) ust
gUpdate{|User|}					mode ust = basicUpdateSimple mode (AnonymousUser "") ust
gUpdate{|BoundedInt|}			mode ust = basicUpdate mode (\json i -> maybe i (\cur -> {BoundedInt|i & cur = cur}) (fromJSON json)) {BoundedInt|min=1,cur=3,max=5} ust
gUpdate{|Progress|}				mode ust = noUpdate mode {Progress|progress=ProgressUndetermined, description = ""} ust
gUpdate{|HtmlInclude|}			mode ust = basicUpdateSimple mode (HtmlInclude "") ust
gUpdate{|FormButton|}			mode ust = basicUpdate mode (\st b								-> {FormButton|b & state = st})																						{FormButton | label = "Form Button", icon="", state = NotPressed}	ust
gUpdate{|URL|}					mode ust = basicUpdate mode (\json url -> maybe url (\s -> URL s) (fromJSON json))  (URL "") ust

gUpdate{|Table|}				mode ust = basicUpdate mode (\json (Table headers cells _)		-> case fromJSON json of Just i = Table headers cells (Just i); _ = Table headers cells Nothing)			(Table [] [] Nothing) 												ust

gUpdate{|TreeChoice|} _ _		UDCreate ust	= basicCreate (TreeChoice (Tree []) Nothing) ust
gUpdate{|TreeChoice|} _ _		(UDSearch (TreeChoice options sel)) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm, om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= case fromJSON update of
			Just ("sel",idx,val)
				= (TreeChoice options (if val (Just idx) Nothing), {ust & newMask = appendToMask newMask (touch cm)})
			Just ("exp",idx,val)
				= (TreeChoice options sel, {ust & newMask = appendToMask newMask (if val (expand idx cm) (collapse idx cm))})
			_
				= ((TreeChoice options sel), {ust & newMask = appendToMask newMask cm})
	| otherwise
		= ((TreeChoice options sel), {ust & newMask = appendToMask newMask cm})

touch (TouchedWithState s)	= TouchedWithState s
touch (PartiallyTouched c)	= PartiallyTouched c
touch _						= Touched

expand :: Int UpdateMask -> UpdateMask
expand idx (TouchedWithState s) = case fromJSON s of
	Just list	= TouchedWithState (toJSON (removeDup [idx:list]))
	_			= TouchedWithState (toJSON [idx])
expand idx _	= TouchedWithState (toJSON [idx])
 
collapse idx (TouchedWithState s) = case fromJSON s of
	Just list	= TouchedWithState (toJSON (removeMember idx list))
	_			= TouchedWithState s
collapse idx m = m

gUpdate{|TreeChoiceNoView|} _	mode ust = updateChoice mode update (TreeChoiceNoView (Tree []) Nothing) ust
where
	update ("sel",idx,val)		(TreeChoiceNoView options _) 		= TreeChoiceNoView options (if val (Just idx) Nothing)
	update ("exp",idx,val)		(TreeChoiceNoView options sel)		= TreeChoiceNoView options sel
	update _					treechoice							= treechoice
	

gUpdate{|GridChoice|} _ _		mode ust = updateChoice mode (\idx (GridChoice options _) -> GridChoice options (Just idx)) (GridChoice [] Nothing) ust
gUpdate{|GridChoiceNoView|} _	mode ust = updateChoice mode (\idx (GridChoiceNoView options _) -> GridChoiceNoView options (Just idx)) (GridChoiceNoView [] Nothing) ust
gUpdate{|RadioChoice|} _ _		mode ust = updateChoice mode (\idx (RadioChoice options _) -> RadioChoice options (Just idx)) (RadioChoice [] Nothing) ust
gUpdate{|RadioChoiceNoView|} _	mode ust = updateChoice mode (\idx (RadioChoiceNoView options _) -> RadioChoiceNoView options (Just idx)) (RadioChoiceNoView [] Nothing) ust
gUpdate{|ComboChoice|} _ _		mode ust = updateChoice mode (\idx (ComboChoice options _) -> ComboChoice options (Just idx)) (ComboChoice [] Nothing) ust
gUpdate{|ComboChoiceNoView|} _	mode ust = updateChoice mode (\idx (ComboChoiceNoView options _) -> ComboChoiceNoView options (Just idx)) (ComboChoiceNoView [] Nothing) ust

gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCCombo val))	ust = appFst DCCombo (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCRadio val))	ust = appFst DCRadio (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCTree val))		ust = appFst DCTree (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCGrid val))		ust = appFst DCGrid (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	UDCreate 					ust = appFst DCRadio (gUpdate{|*->*->*|} fx fy UDCreate ust)

gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCComboNoView val))	ust = appFst DCComboNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCRadioNoView val))	ust = appFst DCRadioNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCTreeNoView val)) 	ust = appFst DCTreeNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCGridNoView val)) 	ust = appFst DCGridNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx UDCreate	 						ust = appFst DCRadioNoView (gUpdate{|*->*|} fx UDCreate ust)

updateChoice mode select empty ust = basicUpdate mode (\json choice -> maybe choice (\i -> select i choice) (fromJSON json)) empty ust

gUpdate{|CheckMultiChoice|} _ _	mode ust = basicUpdate mode (\json (CheckMultiChoice opts sel)	-> case fromJSON json of Just (i,v) = CheckMultiChoice opts (updateSel i v sel); _ = CheckMultiChoice opts sel)	(CheckMultiChoice [] [])										ust
where
	updateSel i True sel	= removeDup [i:sel]
	updateSel i False sel 	= removeMember i sel
	
gUpdate{|Date|} UDCreate ust = basicCreate {day = 1, mon = 1, year = 1970} ust
gUpdate{|Date|} (UDSearch d) ust = basicSearch d (\json old -> fromMaybe old (fromJSON json)) ust
gUpdate{|Time|} UDCreate ust = basicCreate {hour = 0, min = 0, sec = 0} ust
gUpdate{|Time|} (UDSearch t) ust = basicSearch t (\json old -> fromMaybe old (fromJSON json)) ust

gUpdate{|Dynamic|}		mode ust = basicUpdate mode unchanged (dynamic 42) ust
gUpdate{|(->)|} _ fy	mode ust
	# (def,ust) = fy UDCreate ust
	= basicUpdate mode unchanged (const def) ust

gUpdate {|Document|} UDCreate ust = basicCreate {Document|documentId = "", contentUrl = "", name="", mime="", size = 0} ust
gUpdate {|Document|} (UDSearch s) ust=:{searchPath, currentPath, update, oldMask, newMask}
	# (cm,om)		= popMask oldMask
	# ust			= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= case fromJSON update of
			Nothing 	// Reset
				= ({Document|documentId = "", contentUrl = "", name="", mime="", size = 0},{ust & newMask = appendToMask newMask Blanked})
			Just doc 	//Update
				# ust					= {ust & newMask = appendToMask newMask (PartiallyTouched [])}
				= (doc,ust)
	| otherwise 
		= (s, {ust & newMask = appendToMask newMask cm})

gUpdate{|HtmlTag|} mode ust = noUpdate mode (Html "") ust

//Helper types for GoogleMap update
:: MVCUpdate = 
	{ center			:: !(Real,Real)
	, zoom				:: !Int
	, type				:: !GoogleMapType
	}	
	
:: ClickUpdate = 
	{ event				:: !ClickEvent
	, source			:: !ClickSource
	, point				:: !(Real,Real)
	}

:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK
:: ClickSource  = MAP | MARKER (Real,Real)

:: MarkerDragUpdate = 
	{ index				:: !Int
	, point				:: !(Real,Real)
	}
derive JSONDecode MVCUpdate,  ClickUpdate, ClickEvent, ClickSource, MarkerDragUpdate

gUpdate{|GoogleMap|} mode ust = basicUpdate mode parseUpdate defaultMap ust
where
	parseUpdate json orig
		# mbMVC		= fromJSON json
		| isJust mbMVC
			# {MVCUpdate|center=(lat,lng),zoom,type} = fromJust mbMVC
			= {GoogleMap | orig & perspective = {GoogleMapPerspective|orig.perspective & center = {lat=lat,lng=lng}, zoom = zoom, type = type}}
		# mbClick 	= fromJSON json
		| isJust mbClick
			# click = fromJust mbClick
			# marker = {GoogleMapMarker | position = {lat=fst click.ClickUpdate.point,lng=snd click.ClickUpdate.point}, title = Nothing, icon = Nothing, infoWindow = Nothing, draggable = True, selected = False} 
			= {GoogleMap | orig & markers = orig.GoogleMap.markers ++ [marker]}
		# mbMarkerDrag = fromJSON json
		| isJust mbMarkerDrag
			# {MarkerDragUpdate|index,point=(lat,lng)}	= fromJust mbMarkerDrag
			= {GoogleMap | orig & markers = [if (i == index) {GoogleMapMarker|m & position = {lat=lat,lng=lng}} m \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		
		| otherwise = orig

	defaultMap =
		{ GoogleMap
		| settings=settings
		, perspective=perspective
		, markers=[]
		}
	perspective =
		{ GoogleMapPerspective
		| type				= ROADMAP
		, center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
		, zoom				= 10
		}	
	settings =
		{ GoogleMapSettings
		| mapTypeControl	= True
		, panControl		= True
		, streetViewControl	= True
		, zoomControl		= True
		, scaleControl		= True
		, scrollwheel		= True
		, draggable			= True
		}

derive gUpdate Either, (,), (,,), (,,,), JSONNode, Void, DateTime, Timestamp, Map, EmailAddress, Action, TreeNode, UserConstraint, ManagementMeta, TaskPriority, Tree
derive gUpdate GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType

noUpdate :: !(UpdateMode a) a !*USt -> *(!a,!*USt)
noUpdate UDCreate def ust	= basicCreate def ust
noUpdate (UDSearch v) _ ust=:{currentPath,oldMask,newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}
	= (v,ust)

basicUpdate :: !(UpdateMode a) (upd a -> a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
basicUpdate mode toV def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v toV ust
	
basicUpdateSimple :: !(UpdateMode a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a
basicUpdateSimple mode def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v (\json old -> fromMaybe old (fromJSON json)) ust	

basicCreate :: !a !*USt -> *(!a,!*USt)
basicCreate def ust=:{newMask} = (def,{ust & newMask = appendToMask newMask Untouched})

basicSearch :: !a !(upd a -> a) !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
basicSearch v toV ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm, om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= (fromMaybe v (fmap (\u -> toV u v) (fromJSON update)), {ust & newMask = appendToMask newMask (toggleMask update)})
	| otherwise
		= (v, {ust & newMask = appendToMask newMask cm})


//Utility functions
dp2s :: !DataPath -> String
dp2s (DataPath path) = join "-" (map toString (reverse path))

s2dp :: !String -> DataPath
s2dp ""		= DataPath []
s2dp str	= DataPath (reverse (map toInt (split "-" str)))

isdps :: !String -> Bool
isdps path = and [c == '-' || isDigit c || c == '_' \\ c <-: path]

startDataPath :: DataPath
startDataPath = DataPath [0]

emptyDataPath :: DataPath
emptyDataPath = DataPath []

stepDataPath :: !DataPath -> DataPath
stepDataPath dp=:(DataPath [])	= dp
stepDataPath (DataPath [x:xs])	= DataPath [inc x:xs]

shiftDataPath :: !DataPath -> DataPath
shiftDataPath (DataPath path) = DataPath [0:path]

childDataPath :: !DataPath !Int -> DataPath
childDataPath (DataPath path) i = DataPath [i:path]

parentDataPath :: !DataPath -> (!DataPath,!Int)
parentDataPath (DataPath []) = (DataPath [], -1)
parentDataPath (DataPath [i:path]) = (DataPath path, i)

dataPathLevel :: !DataPath -> Int
dataPathLevel (DataPath l) = length l

instance == DataPath
where
	(==) (DataPath a) (DataPath b) = a == b

dataPathList :: !DataPath -> [Int]
dataPathList (DataPath list) = list

dataPathFromList :: ![Int] -> DataPath
dataPathFromList l = DataPath l

// detect whether two paths are equal or if path A is a sub-path of B, assuming reverse-notation. 
// e.g. [1,0] <== [0] 
(<==) infixr 1 :: !DataPath !DataPath -> Bool
(<==) (DataPath pathA) (DataPath pathB) = tlEq (reverse pathA) (reverse pathB)
where
	tlEq _  	 []		= True
	tlEq [] 	 _ 		= False
	tlEq [a:as] [b:bs] 	= (a == b) && (tlEq as bs)

//Masking and unmasking of fields
toggleMask :: !JSONNode -> UpdateMask
toggleMask update = case update of
	JSONNull	= Blanked
	_			= PartiallyTouched []
	
unchanged :: !Void !a -> a
unchanged _ v = v

instance GenMask UpdateMask
where
	popMask :: ![UpdateMask] -> (!UpdateMask, ![UpdateMask])
	popMask []			= (Untouched, [])
	popMask [c:cm]		= (c,cm)

	appendToMask :: ![UpdateMask] !UpdateMask -> [UpdateMask]
	appendToMask l m	= l ++ [m]

	childMasks :: !UpdateMask -> [UpdateMask]
	childMasks (PartiallyTouched  cm)	= cm
	childMasks _						= []

	childMasksN :: !UpdateMask !Int -> [UpdateMask]
	childMasksN (Untouched)	n			= repeatn n Untouched
	childMasksN (PartiallyTouched cm) n	= cm
	childMasksN (Touched) n				= repeatn n Touched
	childMasksN (TouchedWithState _) n	= repeatn n Touched
	childMasksN (Blanked) n				= repeatn n Untouched
	
	isTouched :: !UpdateMask -> Bool
	isTouched  Touched					= True
	isTouched (TouchedWithState _)		= True
	isTouched (PartiallyTouched _)		= True
	isTouched _							= False

allUntouched :: ![UpdateMask] -> Bool
allUntouched children = and [isUntouched c \\ c <- children]
where
	isUntouched Untouched	= True
	isUntouched _			= False

allBlanked :: ![UpdateMask] -> Bool
allBlanked children = and [isBlanked c \\ c <- children]
where
	isBlanked Blanked   = True
	isBlanked _			= False
