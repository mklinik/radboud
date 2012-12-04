implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdGeneric, StdEnum, StdFunc, List_NG, Generic_NG, JSON_NG
import GenUpdate, GenVerify, Util, Maybe, Functor, Text, HTML, Map, UIDefinition, SystemTypes, HtmlUtil, LayoutCombinators

visualizeAsText :: !StaticVisualizationMode !a -> String | gVisualizeText{|*|} a
visualizeAsText mode v = concat (gVisualizeText{|*|} mode v)

visualizeAsEditor :: !a !VerifyMask !TaskId !Layout !*IWorld -> (![(!UIControl,!UIAttributes)],!*IWorld) | gVisualizeEditor{|*|} a
visualizeAsEditor v vmask taskId layout iworld
	# vst		= {VSt|mkVSt taskId iworld & verifyMask = [vmask], currentPath = shiftDataPath emptyDataPath, layout = layout}
	# (res,vst)	= gVisualizeEditor{|*|} (Just v) vst
	= (controlsOf res,kmVSt vst)
	
//Generic text visualizer
generic gVisualizeText a :: !StaticVisualizationMode !a -> [String]

gVisualizeText{|UNIT|} _ _ = []

gVisualizeText{|RECORD|} fx mode (RECORD x)
	# viz = fx mode x
	= case mode of
		AsLabel			= take 1 viz
		AsDisplay		= viz

gVisualizeText{|FIELD of {gfd_name}|} fx mode (FIELD x)
	# viz = fx mode x
	= case mode of
		AsDisplay	= [camelCaseToWords gfd_name, ": ": viz] ++ [" "]
		AsLabel	= viz

gVisualizeText{|OBJECT|} fx mode (OBJECT x) = fx mode x

gVisualizeText{|CONS of {gcd_name,gcd_type_def}|} fx mode (CONS x)
	= normalADTStaticViz (fx mode x)
where
	normalADTStaticViz viz
		//If viz is empty, only show constructor name
		| isEmpty viz
			= [gcd_name]
		//If there are multiple constructors, also show the name of the constructor
		| gcd_type_def.gtd_num_conses > 1
			= intersperse " " [gcd_name:viz]
		//Otherwise show visualisation of fields separated by spaces
		| otherwise
			= intersperse " " viz

gVisualizeText{|PAIR|} fx fy mode (PAIR x y) = fx mode x ++ fy mode y

gVisualizeText{|EITHER|} fx fy mode either = case either of
	LEFT x	= fx mode x
	RIGHT y	= fy mode y

gVisualizeText{|Int|}			_ val				= [toString val]
gVisualizeText{|Real|}			_ val				= [toString val]
gVisualizeText{|Char|}			_ val				= [toString val]
gVisualizeText{|String|}		_ val				= [toString val]
gVisualizeText{|Bool|}			_ val				= [toString val]
gVisualizeText{|Password|}		_ val				= ["********"]
gVisualizeText{|Note|}			_ val				= [toString val]
gVisualizeText{|URL|}			_ val				= [toString val]
gVisualizeText{|Date|}			_ val				= [toString val]
gVisualizeText{|Time|}			_ val				= [toString val]
gVisualizeText{|User|}			_ val				= [toString val]
gVisualizeText{|EUR|}			_ val				= [toString val]
gVisualizeText{|USD|}			_ val						= [toString val]
gVisualizeText{|BoundedInt|}	_ {BoundedInt|cur}			= [toString cur]
gVisualizeText{|Progress|}		_ {Progress|description}	= [description]
gVisualizeText{|HtmlInclude|}	_ val						= ["Html include"]
gVisualizeText{|FormButton|}	_ val				= [val.FormButton.label]
gVisualizeText{|Document|}		_ val
	| val.Document.size == 0						= ["No Document"]
	| otherwise										= [val.Document.name]
gVisualizeText{|RadioChoice|}		fv _ mode val	= fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))
gVisualizeText{|RadioChoiceNoView|}	fo   mode val	= fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))
gVisualizeText{|ComboChoice|}		fv _ mode val	= fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))
gVisualizeText{|ComboChoiceNoView|} fo   mode val	= fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))
gVisualizeText{|GridChoice|}		fv _ mode val	= fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))	
gVisualizeText{|GridChoiceNoView|}	fo   mode val	= fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))	
gVisualizeText{|TreeChoice|}		fv _ mode val	= fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))
gVisualizeText{|TreeChoiceNoView|} 	fo     mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))

gVisualizeText{|DynamicChoice|}		fv fo mode (DCRadio val)	= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCCombo val)	= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCGrid val)		= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCTree val)		= gVisualizeText{|*->*->*|} fv fo mode val

gVisualizeText{|DynamicChoiceNoView|} fo mode (DCRadioNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCComboNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCTreeNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCGridNoView val)	= gVisualizeText{|*->*|} fo mode val

gVisualizeText{|CheckMultiChoice|}	fv _ _ val		= gVisualizeText{|* -> *|} fv  AsLabel (getSelectionViews val)
gVisualizeText{|Table|}	_ _							= ["Table"]
gVisualizeText {|[]|} fx  mode val					= ["[":  flatten (intersperse [", "] [fx mode x \\ x <- val])] ++ ["]"]
gVisualizeText{|Maybe|} fx mode val					= fromMaybe ["-"] (fmap (\v -> fx mode v) val)
gVisualizeText{|Hidden|} _ _ _						= []
gVisualizeText{|Display|} fx mode (Display val)		= fx mode val
gVisualizeText{|Editable|} fx mode(Editable val)	= fx mode val

gVisualizeText{|VisualizationHint|} fx mode val = case val of
	VHHidden x		= gVisualizeText{|* -> *|} fx mode (Hidden x)
	VHDisplay x		= gVisualizeText{|* -> *|} fx mode (Display x)
	VHEditable x	= gVisualizeText{|* -> *|} fx mode (Editable x)

gVisualizeText{|Void|} _ _					= []
gVisualizeText{|Dynamic|} _ _				= []
gVisualizeText{|(->)|} _ _ _ _				= []
gVisualizeText{|JSONNode|} _ val			= [toString val]
gVisualizeText{|HtmlTag|} _ html			= [toString html]

derive gVisualizeText DateTime, Either, (,), (,,), (,,,), Timestamp, Map, EmailAddress, Username, Action, TreeNode, UserConstraint, ManagementMeta, TaskPriority, Tree, ButtonState
derive gVisualizeText GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType

mkVSt :: !TaskId *IWorld -> *VSt
mkVSt taskId iworld
	= {VSt| currentPath = startDataPath, selectedConsIndex = -1, optional = False, disabled = False, verifyMask = []
	  , taskId = taskId, layout = autoLayout, iworld = iworld}

kmVSt :: !*VSt -> *IWorld //inverse of mkVSt
kmVSt {VSt|iworld} = iworld

//Generic visualizer
generic gVisualizeEditor a | gVisualizeText a, gHeaders a, gGridRows a :: !(Maybe a)!*VSt -> (!VisualizationResult,!*VSt)

gVisualizeEditor{|UNIT|} _ vst
	= (NormalEditor [],vst)

gVisualizeEditor{|RECORD|} fx _ _ _ val vst=:{VSt|currentPath,verifyMask,optional,disabled,taskId}
	# (cmv,vm)	= popMask verifyMask
	//When optional and no value yet, just show the checkbox
	| optional && isNothing val && not disabled
		= (OptionalEditor [checkbox False], {VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
	# (fieldViz,vst) = fx (fmap fromRECORD val) {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv, optional = False}
	//For optional records we add the checkbox to clear the entire record
	# viz = if (optional && not disabled) (OptionalEditor [checkbox True:controlsOf fieldViz]) fieldViz	
	= (viz,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
where
	checkbox checked = (UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId = toString taskId, editorId = dp2s currentPath, value = Just checked},newMap)
	
gVisualizeEditor{|FIELD of {gfd_name}|} fx _ _ _ val vst=:{VSt|disabled,layout}
	# (vizBody,vst)		= fx (fmap fromFIELD val) vst
	= case vizBody of
		HiddenEditor			= (HiddenEditor,vst)
		NormalEditor controls
			# controls = layout.Layout.editor (addLabel disabled gfd_name newMap,controls,Vertical)
			= (NormalEditor controls,vst)
		OptionalEditor controls	
			# controls = layout.Layout.editor (addLabel True gfd_name newMap,controls,Vertical)
			= (OptionalEditor controls, vst)


gVisualizeEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} fx _ _ _ val vst=:{currentPath,selectedConsIndex = oldSelectedConsIndex,disabled,verifyMask,taskId,layout}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	# (cmv,vm)	= popMask verifyMask
	# x			= fmap fromOBJECT val
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		# (items, vst=:{selectedConsIndex}) = fx x vst
		# content	= layout.editor (newMap, (if (isTouched cmv) (controlsOf items) []), Vertical)
		= (NormalEditor [(UIDropdown defaultSizeOpts
								{UIChoiceOpts
								| taskId = toString taskId
								, editorId = dp2s currentPath
								, value = if (isTouched cmv) (Just selectedConsIndex) Nothing
								, options = [gdc.gcd_name \\ gdc <- gtd_conses]}
							,addVerAttributes (verifyElementStr cmv) newMap)
						: content
						]
		  ,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})
	//ADT with one constructor or static render: put content into container, if empty show cons name
	| otherwise
		# (vis,vst) = fx x vst
		# vis = case vis of
			HiddenEditor 	= HiddenEditor
			NormalEditor [] = if (isTouched cmv || disabled) (NormalEditor [((stringDisplay ((gtd_conses !! vst.selectedConsIndex).gcd_name)),newMap)]) (NormalEditor [])			
			NormalEditor vis
				= NormalEditor [(setDirection Horizontal (defaultContainer (addSpacing (map fst vis))),newMap)]
			//TODO: Add case for OptionalEditor
		= (vis,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})
where
	addSpacing [] = []
	addSpacing [d:ds] = [d:map (setMargins 0 0 0 5) ds]

gVisualizeEditor{|CONS of {gcd_index}|} fx _ _ _ val vst = visualizeCustom mkControl vst
where
	mkControl name _ _ vst=:{VSt|taskId,currentPath,optional,disabled}
		# x = fmap fromCONS val
		# (viz,vst)	= fx x vst
		= (controlsOf viz, {VSt | vst & selectedConsIndex = gcd_index})

gVisualizeEditor{|PAIR|} fx _ _ _ fy _ _ _ val vst
	# (x,y)			= (fmap fromPAIRX val, fmap fromPAIRY val)
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	= case (vizx,vizy) of	//Define combination for all nine combinations of normal/optional/hidden editors
		(NormalEditor ex,	NormalEditor ey)		= (NormalEditor (ex ++ ey), vst)
		(NormalEditor ex,	OptionalEditor ey)		= (NormalEditor (ex ++ ey), vst)
		(NormalEditor ex,	HiddenEditor)			= (NormalEditor ex, vst)
		(OptionalEditor ex,	NormalEditor ey)		= (NormalEditor (ex ++ ey), vst)
		(OptionalEditor ex,	OptionalEditor ey)		= (OptionalEditor (ex ++ ey), vst)
		(OptionalEditor ex, HiddenEditor)			= (OptionalEditor ex, vst)
		(HiddenEditor,		NormalEditor ey)		= (NormalEditor ey, vst)
		(HiddenEditor,		OptionalEditor ey)		= (OptionalEditor ey, vst)
		(HiddenEditor,		HiddenEditor)			= (HiddenEditor, vst)
		
		
gVisualizeEditor{|EITHER|} fx _ _ _ fy _ _ _ val vst = case val of
		Nothing			= fx Nothing vst
		Just (LEFT x)	= fx (Just x) vst
		Just (RIGHT y)	= fy (Just y) vst


gVisualizeEditor{|(,)|} fx _ _ _ fy _ _ _ val vst=:{VSt|currentPath,verifyMask}
	# (x,y)			= (fmap fst val, fmap snd val)
	# (cmv,vm)		= popMask verifyMask
	# vst			= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	# viz = case (vizx,vizy) of
		(HiddenEditor,HiddenEditor) = HiddenEditor
		_	= NormalEditor (controlsOf vizx ++ controlsOf vizy)
				 
	= (viz, {VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gVisualizeEditor{|Int|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditInt defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)
	
gVisualizeEditor{|Real|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Char|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap toString val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|String|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Bool|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled		= ([(UIViewCheckbox defaultSizeOpts {UIViewOpts|value = val},addVerAttributes verRes newMap)],vst)
		| otherwise		= ([(UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Username|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(Username v) -> v) val},newMap)],vst)
		| otherwise	= ([(UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(Username v) -> v) val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Password|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = Just "********"},newMap)],vst)
		| otherwise	= ([(UIEditPassword defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value= fmap (\(Password v) -> v) val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Note|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
//		| disabled	= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap (\(Note v) -> Text v) val},newMap)],vst)
		| disabled	= ([(setMargins 5 5 5 5 (UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap noteToHtml val}),newMap)],vst)

		| otherwise	= ([(UIEditNote sizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(Note v) -> v) val},addVerAttributes verRes newMap)],vst)
	
	sizeOpts = {UISizeOpts|defaultSizeOpts & height = Just FlexSize, minHeight = Just WrapMin}
	
	// THIS IS A HACK!
	// The encoding of a Text constructor should escape newlines and convert them to <br> tags. Unfortunately it doesn't
	noteToHtml (Note s)	//TODO: Fix this in the toString of the Text constructor of HtmlTag type
		= case split "\n" s of
			[line]	= Text line
			lines	= SpanTag [] (intersperse (BrTag []) (map Text lines))
			

gVisualizeEditor{|Date|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditDate defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Time|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditTime defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|EUR|}	val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(EUR v) -> toString v) val},newMap)],vst)
		| otherwise	= ([(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(EUR v) -> toReal v / 100.0) val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|USD|}	val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(USD v) -> toReal v / 100.0) val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|BoundedInt|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		# sliderOpts	= {UISliderOpts|minValue=maybe 1 (\{BoundedInt|min} -> min) val,maxValue=maybe 5 (\{BoundedInt|max} -> max) val}
		| disabled									
			# viewOpts = {UIViewOpts|value = fmap curVal val}  
			= ([(UIViewSlider defaultSizeOpts viewOpts sliderOpts, newMap)],vst)
		| otherwise
			# editOpts = {UIEditOpts|taskId = toString taskId, editorId = name, value = fmap curVal val}
			= ([(UIEditSlider defaultSizeOpts editOpts sliderOpts, addVerAttributes verRes newMap)],vst)

	curVal {BoundedInt|cur} = cur

gVisualizeEditor{|Progress|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId}
		= ([(UIViewProgress defaultSizeOpts {UIViewOpts|value=fmap value val} {UIProgressOpts|text = text val},newMap)],vst)
	where
		text (Just {Progress|description}) 	= description
		text _								= ""
		
		value {Progress|progress=ProgressRatio ratio} 
			| ratio < 0.0	= ProgressRatio 0.0
			| ratio > 1.0	= ProgressRatio 1.0
							= ProgressRatio ratio
		value {Progress|progress} = progress
		
gVisualizeEditor{|HtmlInclude|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst
		= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value=fmap (\(HtmlInclude path) -> IframeTag [SrcAttr path] []) val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor {|Document|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewDocument defaultSizeOpts {UIViewOpts|value = val},newMap)],vst)
		| otherwise	= ([(UIEditDocument defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|URL|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap (\(URL url) -> ATag [HrefAttr url] [Text url]) val},newMap)], vst)
		| otherwise
			# val = checkMask touched val
			# ui = UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap toString val}
			= ([(ui,addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|FormButton|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# text = fmap (\b -> b.FormButton.label) val
		# iconCls = fmap (\b -> b.FormButton.icon) val
		= ([(UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\_ -> JSONString "pressed") val} {UIButtonOpts|text=text,iconCls=iconCls,disabled=False},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|GoogleMap|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId}
		# editOpts	= {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing}
		# opts		= mapOpts (fromMaybe defaultValue val)
		= ([(UIEditGoogleMap defaultSizeOpts editOpts opts,addVerAttributes verRes newMap)],vst)
	
	mapOpts map =
		{ UIGoogleMapOpts
		| center = (map.perspective.GoogleMapPerspective.center.lat,map.perspective.GoogleMapPerspective.center.lng)
		, mapType = mapType map.perspective.GoogleMapPerspective.type
		, markers = [{UIGoogleMapMarker|position=(lat,lng),title=title,icon=icon,infoWindow=fmap toString infoWindow,draggable=draggable,selected=selected}
					\\ {GoogleMapMarker|position={lat,lng},title,icon,infoWindow,draggable,selected} <- map.GoogleMap.markers]
		, options =
			{ UIGoogleMapOptions
			| mapTypeControl = map.settings.GoogleMapSettings.mapTypeControl
			, panControl = map.settings.GoogleMapSettings.panControl
			, streetViewControl = map.settings.GoogleMapSettings.streetViewControl
			, zoomControl = map.settings.GoogleMapSettings.zoomControl
			, scaleControl = map.settings.GoogleMapSettings.scaleControl
			, scrollwheel = map.settings.GoogleMapSettings.scrollwheel
			, draggable = map.settings.GoogleMapSettings.draggable
			, zoom = map.perspective.GoogleMapPerspective.zoom
			}
		}
	mapType ROADMAP 	= "ROADMAP"
	mapType SATELLITE 	= "SATELLITE"
	mapType HYBRID 		= "HYBRID"
	mapType TERRAIN 	= "TERRAIN"
		
gVisualizeEditor{|RadioChoice|} _ gx _ _ _ _ _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)

	vvalue (Just (RadioChoice options (Just sel)))	= Just (hd (gx AsLabel (fst(options !! sel ))))
	vvalue _										= Nothing
	evalue (Just (RadioChoice _ mbSel))				= mbSel
	evalue _										= Nothing
	options (Just (RadioChoice options _))			= [concat (gx AsLabel v) \\ (v,_) <- options]
	options	_										= []
	
		
gVisualizeEditor{|RadioChoiceNoView|} _ gx _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)
	
	vvalue (Just (RadioChoiceNoView options (Just sel)))	= Just (hd (gx AsLabel (options !! sel )))
	vvalue _												= Nothing
	evalue (Just (RadioChoiceNoView _ mbSel))				= mbSel
	evalue _												= Nothing
	options (Just (RadioChoiceNoView options _))			= [concat (gx AsLabel v) \\ v <- options]
	options	_												= []
	
gVisualizeEditor{|ComboChoice|} fx gx _ _ _ _ _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)

	vvalue (Just (ComboChoice options (Just sel)))	= Just (hd (gx AsLabel (fst(options !! sel ))))
	vvalue _										= Nothing
	evalue (Just (ComboChoice _ mbSel))				= mbSel
	evalue _										= Nothing
	options (Just (ComboChoice options _))			= [concat (gx AsLabel v) \\ (v,_) <- options]
	options	_										= []
	
gVisualizeEditor{|ComboChoiceNoView|} _ gx _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)
	
	vvalue (Just (ComboChoiceNoView options (Just sel)))	= Just (hd (gx AsLabel (options !! sel )))
	vvalue _												= Nothing
	evalue (Just (ComboChoiceNoView _ mbSel))				= mbSel
	evalue _												= Nothing
	options (Just (ComboChoiceNoView options _))			= [concat (gx AsLabel v) \\ v <- options]
	options	_												= []
	
gVisualizeEditor{|GridChoice|} _ gx hx ix _ _ _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		= ([(UIGrid defaultSizeOpts
			{UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options = options val}
			{UIGridOpts|columns = hx undef},addVerAttributes verRes newMap)],vst)
	
	value (Just (GridChoice options mbSel)) = mbSel
	value _									= Nothing
	options (Just (GridChoice options _))	= [fromMaybe [concat (gx AsLabel opt)] (ix opt []) \\ (opt,_) <- options]
	options _								= []


gVisualizeEditor{|GridChoiceNoView|} _ gx hx ix val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		= ([(UIGrid defaultSizeOpts
			{UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options =options val}
			{UIGridOpts|columns = hx undef},newMap)],vst)
	
	value (Just (GridChoiceNoView options mbSel))	= mbSel
	value _											= Nothing
	options (Just (GridChoiceNoView options _))		= [fromMaybe [concat (gx AsLabel opt)] (ix opt []) \\ opt <- options]
	options _										= []

gVisualizeEditor{|TreeChoice|} _ gx _ _ _ _ _ _ val vst=:{VSt|taskId,currentPath,disabled,verifyMask}
	# (cmv,vm)	= popMask verifyMask
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# ver		= verifyElementStr cmv
	# viz		= [(UITree sizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=dp2s currentPath,value=value val,options = options val cmv},addVerAttributes ver newMap)]
	= (NormalEditor viz,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
where

	sizeOpts = {UISizeOpts|defaultSizeOpts & height = Just FlexSize, minHeight = Just WrapMin}

	value  (Just (TreeChoice _ mbSel)) 	= mbSel
	value _								= Nothing
	
	options (Just (TreeChoice (Tree nodes) _)) msk = fst (mkTree nodes 0 )
		where
			expanded = case msk of
				VMValidWithState _ _ s 		= case fromJSON s of Just expanded = expanded; _ = []
				VMInvalidWithState _ _ s	= case fromJSON s of Just expanded = expanded; _ = []
				_							= []
				
			mkTree [] idx
				= ([],idx)
			mkTree [Leaf (v,_):r] idx
				# (rtree,idx`) 		= mkTree r (inc idx)
				= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = True, expanded = isMember idx expanded, children = Nothing}:rtree],idx`)
			mkTree [Node (v,_) nodes:r] idx
				# (children,idx`)	= mkTree nodes (inc idx)
				# (rtree,idx`)		= mkTree r idx`
				= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = False, expanded = isMember idx expanded, children = Just children}:rtree],idx`)
	options _ _ = []

gVisualizeEditor{|TreeChoiceNoView|} _ gx _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId}
		= ([(UITree defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options = options val},newMap)],vst)

	value (Just (TreeChoiceNoView _ mbSel)) = mbSel
	value _									= Nothing
	options (Just (TreeChoiceNoView (Tree nodes) _)) = fst (mkTree nodes 0)
	where
		mkTree [] idx
			= ([],idx)
		mkTree [Leaf v:r] idx
			# (rtree,idx`) 		= mkTree r (inc idx)
			= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = True, expanded = False, children = Nothing}:rtree],idx`)
		mkTree [Node v nodes:r] idx
			# (children,idx`)	= mkTree nodes (inc idx)
			# (rtree,idx`)		= mkTree r idx`
			= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = False, expanded = False, children = Just children}:rtree],idx`)
	options _ = []
				
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCCombo val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCRadio val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCTree val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCGrid val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 Nothing vst
	= (NormalEditor [],vst)

gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCComboNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCRadioNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCTreeNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCGridNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 Nothing vst
	= (NormalEditor [],vst)
	
getMbView f mbChoice = fmap f (maybe Nothing getMbSelectionView mbChoice)

gVisualizeEditor{|CheckMultiChoice|} fx _ _ _ _ _ _ _  val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# (options,sel)		= maybe ([],[]) (\(CheckMultiChoice options sel) -> (map fst options,sel)) val
		# (itemsVis,vst)	= childVisualizations fx options {VSt|vst & disabled = True}
		# itemDefs			= [defaultContainer [checkbox taskId i sel:map fst (controlsOf items)]  \\ items <- itemsVis & i <- [0..]]
		= ([(defaultContainer itemDefs,addVerAttributes verRes newMap)], vst)

	checkbox taskId i sel
		= UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId="sel-" +++ toString i,value= Just (isMember i sel)}

gVisualizeEditor{|Table|} val vst = visualizeCustom viz vst 
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		= ([(UIGrid defaultSizeOpts
			{UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options = options val}
			{UIGridOpts|columns = columns val},addVerAttributes verRes newMap)],vst)
	
	value (Just (Table _ _ mbSel))	= mbSel
	value _							= Nothing
	
	columns (Just (Table headers _ _))	= headers
	columns _							= []
	
	options (Just (Table _ cells _))	= map (map toString) cells
	options _							= []

gVisualizeEditor{|[]|} fx _ _ _ val vst=:{VSt|taskId,currentPath,disabled,verifyMask,layout}
	# (cmv,vm)		= popMask verifyMask
	# vst			= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# ver			= verifyElementStr cmv
	# val			= fromMaybe [] val
	# (items,vst)	= listControl val vst
	= (NormalEditor [(listContainer items,addVerAttributes ver newMap)],{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
where
	name = dp2s currentPath
	listControl items vst=:{VSt|optional,disabled}
		# (itemsVis,vst)	= childVisualizations fx items vst
		# numItems = length items
		| disabled
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
		| otherwise
			//# (newItem,vst)		= newChildVisualization fx True vst
			//= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [newItemControl newItem],vst)
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
						
	listItemControl disabled numItems idx item 
		# controls	= map fst (layout.editor (newMap,controlsOf item,Vertical))
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString ("mup_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=idx == 0}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString ("mdn_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= idx == numItems - 1}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString ("rem_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=False}
					  ]
		= setHeight WrapSize (setDirection Horizontal (defaultContainer (if disabled controls (controls ++ buttons))))
/*
	newItemControl item
		# controls	= map fst (layout.editor (newMap,controlsOf item))
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=True}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= True}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=True}
					  ]
		= setDirection Horizontal (defaultContainer (controls ++ buttons))
*/	
	addItemControl numItems
		# controls	= [UIViewString {defaultSizeOpts & width=Just FlexSize} {UIViewOpts|value= Just (numItemsText numItems)}]
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString "add")} {UIButtonOpts|text=Nothing,iconCls=Just "icon-add",disabled=False}]
		= setHeight WrapSize (setDirection Horizontal (defaultContainer (controls ++ buttons)))
	
	listContainer items
		= setHeight WrapSize (defaultContainer items)
	
	numItemsText 1 = "1 item"
	numItemsText n = toString n +++ " items"
	
				
gVisualizeEditor{|Dynamic|}					_ vst	= noVisualization vst
gVisualizeEditor{|(->)|} _ _ _ _ _ _ _ _	_ vst	= noVisualization vst

gVisualizeEditor{|Maybe|} fx _ _ _ val vst=:{VSt|currentPath,optional}
	# (viz,vst) = case val of
		Just (Just x)	= fx (Just x) {VSt|vst & optional = True}
		_				= fx Nothing {VSt|vst & optional = True}
	= (toOptional viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
where
	toOptional	(NormalEditor ex)	= OptionalEditor ex
	toOptional	viz					= viz
		
// wrapper types changing visualization behaviour
gVisualizeEditor{|Hidden|} fx _ _ _ val vst=:{VSt | currentPath, verifyMask}
	# (_,vm) = popMask verifyMask	
	= (HiddenEditor,{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gVisualizeEditor{|Display|} fx _ _ _ val vst=:{VSt|currentPath,disabled}
	# (def,vst) = fx (fmap fromDisplay val) {VSt | vst &  disabled = True}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, disabled = disabled})

gVisualizeEditor{|Editable|} fx _ _ _ val vst=:{VSt|currentPath, disabled}
	# (def,vst) = fx (fmap fromEditable val) {VSt | vst & disabled = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, disabled = disabled})

gVisualizeEditor{|VisualizationHint|} fx gx hx ix val vst=:{VSt|currentPath}
	= case val of
		Just (VHHidden x)	= gVisualizeEditor{|* -> *|} fx gx hx ix (Just (Hidden x)) vst
		Just (VHDisplay x)	= gVisualizeEditor{|* -> *|} fx gx hx ix (Just (Display x)) vst
		Just (VHEditable x)	= gVisualizeEditor{|* -> *|} fx gx hx ix (Just (Editable x)) vst
		Nothing				= fx Nothing vst

gVisualizeEditor{|Void|} _ vst = noVisualization vst
gVisualizeEditor{|HtmlTag|}	val vst = visualizeCustom toControl vst
where
	toControl name touched _ vst
		= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = val},newMap)], vst)

derive gVisualizeEditor DateTime, User
derive gVisualizeEditor JSONNode, Either, (,,), (,,,), Timestamp, Map, EmailAddress, Action, TreeNode, UserConstraint, ManagementMeta, TaskPriority, Tree
derive gVisualizeEditor GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType

generic gHeaders a :: a -> [String]

gHeaders{|UNIT|} _			= []
gHeaders{|PAIR|} _ _ _		= []
gHeaders{|EITHER|} _ _ _	= []
gHeaders{|OBJECT|} _ _		= []
gHeaders{|CONS|} _ _		= []
gHeaders{|RECORD of {grd_fields}|} _ _	= [camelCaseToWords fieldname \\ fieldname <- grd_fields]
gHeaders{|FIELD|} _ _		= []
gHeaders{|Int|}	_			= []
gHeaders{|Char|} _			= []
gHeaders{|String|} _		= []
gHeaders{|Real|} _			= []
gHeaders{|Bool|} _ 			= []
gHeaders{|Dynamic|}	_		= []
gHeaders{|BoundedInt|} _	= []
gHeaders{|Progress|} _		= []
gHeaders{|HtmlTag|}	_		= []
gHeaders{|(->)|} _ _ _		= []

derive gHeaders [], Maybe, Either, (,), (,,), (,,,), JSONNode, Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gHeaders URL, Note, Username, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, User, CheckMultiChoice, Map, Tree, TreeNode, Table
derive gHeaders EmailAddress, Action, HtmlInclude, UserConstraint, ManagementMeta, TaskPriority
derive gHeaders	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gHeaders DynamicChoice, RadioChoice, ComboChoice, GridChoice, TreeChoice
derive gHeaders DynamicChoiceNoView, RadioChoiceNoView, ComboChoiceNoView, GridChoiceNoView, TreeChoiceNoView

generic gGridRows a | gVisualizeText a :: !a ![String] -> Maybe [String]

gGridRows{|OBJECT|} _ _ _ _					= Nothing
gGridRows{|CONS|} _ _ _ acc					= Nothing
gGridRows{|PAIR|} fx _ fy _ (PAIR x y) acc	= fy y (fromMaybe [] (fx x acc))
gGridRows{|RECORD|} fx _ (RECORD r) acc		= fmap reverse (fx r acc) 
gGridRows{|FIELD|} _ gx (FIELD f) acc		= Just [concat (gx AsLabel f):acc]
gGridRows{|EITHER|} _ _ _ _	_ _				= abort "gGridRows: EITHER should not occur"
gGridRows{|UNIT|} _ _						= abort "gGridRows: UNIT should not occur"

gGridRows{|Int|} i _						= Nothing
gGridRows{|Char|} c _						= Nothing
gGridRows{|String|} s _						= Nothing
gGridRows{|Real|} r _						= Nothing
gGridRows{|Bool|} b _						= Nothing
gGridRows{|Dynamic|} d _					= Nothing
gGridRows{|BoundedInt|} _ _					= Nothing
gGridRows{|Progress|} _ _					= Nothing
gGridRows{|HtmlTag|} h _					= Nothing
gGridRows{|(->)|} _ gx _ gy f _				= Nothing


derive gGridRows [], Maybe, Either, (,), (,,), (,,,), JSONNode, Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gGridRows URL, Note, Username, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, User, UserConstraint, CheckMultiChoice, Map, Tree, TreeNode, Table
derive gGridRows EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority, ButtonState
derive gGridRows GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gGridRows DynamicChoice, RadioChoice, ComboChoice, TreeChoice, GridChoice
derive gGridRows DynamicChoiceNoView, RadioChoiceNoView, ComboChoiceNoView, GridChoiceNoView, TreeChoiceNoView

//***** UTILITY FUNCTIONS *************************************************************************************************	
		
visualizeCustom :: !UIVizFunction !*VSt -> *(!VisualizationResult,!*VSt)
visualizeCustom tuiF vst=:{VSt|currentPath,disabled,verifyMask}
	# (cmv,vm)	= popMask verifyMask
	// only check mask if generating editor definition & not for labels
	# touched	= isTouched cmv
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# ver		= verifyElementStr cmv
	# (vis,vst) = tuiF (dp2s currentPath) touched ver vst
	= (NormalEditor vis,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
noVisualization :: !*VSt -> *(!VisualizationResult,!*VSt)
noVisualization vst=:{VSt|currentPath,verifyMask}
	# (_,vm) = popMask verifyMask
	= (NormalEditor [], {VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
childVisualizations :: !((Maybe a) -> .(*VSt -> *(!VisualizationResult,*VSt))) ![a] !*VSt -> *(![VisualizationResult],!*VSt)
childVisualizations fx children vst = childVisualizations` children [] vst
where
	childVisualizations` [] acc vst
		= (reverse acc,vst)
	childVisualizations` [child:children] acc vst
		# (childV,vst) = fx (Just child) vst
		= childVisualizations` children [childV:acc] vst

newChildVisualization :: !((Maybe a) -> .(*VSt -> *(VisualizationResult,*VSt))) !Bool !*VSt -> *(!VisualizationResult,!*VSt)
newChildVisualization fx newOptional vst=:{VSt|optional}
	# (childV,vst) = fx Nothing {VSt|vst & optional = newOptional}
	= (childV,{VSt|vst & optional = optional})

eventValue :: !DataPath !(Maybe (!String,!JSONNode)) -> Maybe JSONNode
eventValue currentPath mbEvent = case mbEvent of
	Just (dp,val) | dp == dp2s currentPath	= Just val
	_										= Nothing

verifyElementStr :: !VerifyMask -> VerifyResult
verifyElementStr cmv = case cmv of
	VMValid mbHnt _				= maybe NoMsg ValidMsg mbHnt
	VMValidWithState mbHnt _ _	= maybe NoMsg ValidMsg mbHnt
	VMUntouched mbHnt _ _		= maybe NoMsg HintMsg mbHnt
	VMInvalid err _				= ErrorMsg (toString err)
	VMInvalidWithState err _ _	= ErrorMsg (toString err)
	
addVerAttributes :: !VerifyResult !UIAttributes -> UIAttributes
addVerAttributes (HintMsg msg)	attr = put HINT_ATTRIBUTE msg attr
addVerAttributes (ValidMsg msg)	attr = put VALID_ATTRIBUTE msg attr
addVerAttributes (ErrorMsg msg) attr = put ERROR_ATTRIBUTE msg attr
addVerAttributes _				attr = attr

addLabel :: !Bool !String !UIAttributes -> UIAttributes
addLabel optional label attr = put LABEL_ATTRIBUTE (format optional label) attr
where
	format optional label = camelCaseToWords label +++ if optional "" "*" +++ ":" //TODO: Move to layout


checkMask :: !Bool !(Maybe a) -> (Maybe a)
checkMask False _	= Nothing
checkMask _ val 	= val

controlsOf :: !VisualizationResult -> [(UIControl,UIAttributes)]
controlsOf (NormalEditor controls)		= controls
controlsOf (OptionalEditor controls)	= controls
controlsOf HiddenEditor					= []

//*********************************************************************************************************************
	
(+++>) infixr 5	:: !a !String -> String | gVisualizeText{|*|} a
(+++>) a s = visualizeAsText AsLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualizeText{|*|} a
(<+++) s a = s +++ visualizeAsText AsLabel a
