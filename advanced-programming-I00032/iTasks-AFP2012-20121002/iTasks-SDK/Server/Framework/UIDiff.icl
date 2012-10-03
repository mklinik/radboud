implementation module UIDiff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple, sapldebug
import Text, Util, UIDefinition
from Task import :: Event(..)

:: DiffPath :== [DiffStep] 
:: DiffStep
	= ItemStep !Int		//Navigate to item i
	| MenuStep			//Navigate to the menu bar
	| WindowStep !Int	//Navigate to window i

:: DiffResult
	= DiffImpossible
	| DiffPossible [UIUpdate]

instance toString DiffPath
where
	toString path = join "-" (map step (reverse path))
	where
		step (ItemStep i)	= toString i
		step (MenuStep)		= "m"
		step (WindowStep i)	= "w" +++ toString i

derive gEq UISizeOpts, UISide, UISize, UIMinSize, UISideSizes, UIViewOpts, UISliderOpts, UIProgressOpts, UIButtonOpts
derive gEq UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UIGridOpts, UITreeNode, UIMenuButtonOpts, UIMenuItem, UIActionOpts
derive gEq UILabelOpts, UITabOpts, UIIconOpts, UITaskletOpts
derive gEq UIControl, UILayoutOpts, UIWindowOpts, UIFieldSetOpts, UIPanelOpts, UIContainerOpts, UIChoiceOpts, UIEditOpts, UIVAlign, UIHAlign, UIDirection

derive JSONEncode UITreeNode

//BAD IDEA: FIXME!
//There should be a complete iTasks instance for ProgressAmount type
derive gEq ProgressAmount
JSONEncode{|ProgressAmount|} ProgressUndetermined		= [JSONString "undetermined"]
JSONEncode{|ProgressAmount|} (ProgressRatio ratio)		= [JSONReal ratio]

diffUIDefinitions :: !UIDef !UIDef !Event -> [UIUpdate]	
diffUIDefinitions d1 d2 event = diffItems [] event (uiDefControls d1) (uiDefControls d2) 

//Compare controls
diffControls :: !DiffPath !Event !UIControl !UIControl -> DiffResult
diffControls path event c1 c2
	# parts = case (c1,c2) of
		(UIViewString sOpts1 vOpts1, UIViewString sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewHtml sOpts1 vOpts1, UIViewHtml sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewDocument sOpts1 vOpts1, UIViewDocument sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewCheckbox sOpts1 vOpts1, UIViewCheckbox sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewSlider sOpts1 vOpts1 opts1, UIViewSlider sOpts2 vOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2,diffOpts opts1 opts2]
		(UIViewProgress sOpts1 vOpts1 opts1, UIViewProgress sOpts2 vOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2,diffOpts opts1 opts2]
		(UIEditString sOpts1 eOpts1, UIEditString sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditNote sOpts1 eOpts1, UIEditNote sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditPassword sOpts1 eOpts1, UIEditPassword sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditInt sOpts1 eOpts1, UIEditInt sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditDecimal sOpts1 eOpts1, UIEditDecimal sOpts2 eOpts2)	
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditCheckbox sOpts1 eOpts1, UIEditCheckbox sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditSlider sOpts1 eOpts1 opts1, UIEditSlider sOpts2 eOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2,diffOpts opts1 opts2]
		(UIEditDate sOpts1 eOpts1, UIEditDate sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditTime sOpts1 eOpts1, UIEditTime sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditDocument sOpts1 eOpts1, UIEditDocument sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditButton sOpts1 eOpts1 opts1, UIEditButton sOpts2 eOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2,diffOpts opts1 opts2]
		(UIEditGoogleMap sOpts1 eOpts1 opts1, UIEditGoogleMap sOpts2 eOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2,diffOpts opts1 opts2]
		(UIDropdown sOpts1 cOpts1, UIDropdown sOpts2 cOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2]
		(UIGrid sOpts1 cOpts1 opts1, UIGrid sOpts2 cOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2,diffOpts opts1 opts2]
		(UITree sOpts1 cOpts1, UITree sOpts2 cOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2]
		(UIActionButton sOpts1 aOpts1 opts1, UIActionButton sOpts2 aOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffActionOpts path aOpts1 aOpts2,diffOpts opts1 opts2]
		(UIMenuButton sOpts1 opts1, UIMenuButton sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UILabel sOpts1 opts1, UILabel sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UIIcon sOpts1 opts1, UIIcon sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UITab sOpts1 opts1, UITab sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UITasklet sOpts1 opts1, UITasklet sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UIContainer sOpts1 lOpts1 items1 opts1, UIContainer sOpts2 lOpts2 items2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffLayoutOpts path lOpts1 lOpts2, DiffPossible (diffItems path event items1 items2), diffOpts opts1 opts2]
		(UIPanel sOpts1 lOpts1 items1 opts1, UIPanel sOpts2 lOpts2 items2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffLayoutOpts path lOpts1 lOpts2, DiffPossible (diffItems path event items1 items2), diffOpts opts1 opts2]
		(UIFieldSet sOpts1 lOpts1 items1 opts1, UIFieldSet sOpts2 lOpts2 items2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffLayoutOpts path lOpts1 lOpts2, DiffPossible (diffItems path event items1 items2), diffOpts opts1 opts2]
		(UIWindow sOpts1 lOpts1 items1 opts1, UIWindow sOpts2 lOpts2 items2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffLayoutOpts path lOpts1 lOpts2, DiffPossible (diffItems path event items1 items2), diffOpts opts1 opts2]
		(UICustom opts1, UICustom opts2)
			= [diffOpts opts1 opts2]
		(_,_)
			= [DiffImpossible]		
	= DiffPossible (replaceIfImpossible path c2 parts)

//As a first step, only do diffs for value changes, all other diffs trigger replacements...

diffSizeOpts :: DiffPath UISizeOpts UISizeOpts -> DiffResult
diffSizeOpts path opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffImpossible //DiffPossible [UIResize (toString path) opts2]

diffViewOpts :: DiffPath (UIViewOpts a) (UIViewOpts a) -> DiffResult | gEq{|*|} a & encodeUIValue a
diffViewOpts path opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffPossible [UISetValue (toString path) (encodeUIValue opts2.UIViewOpts.value)]

diffEditOpts :: DiffPath Event (UIEditOpts a) (UIEditOpts a) -> DiffResult | gEq{|*|} a & encodeUIValue a
diffEditOpts path event opts1 opts2
	| isEmpty taskIdUpd && isEmpty editorIdUpd
		= DiffPossible (foldr (++) [] [taskIdUpd,editorIdUpd,valueUpd])
	| otherwise
		= DiffImpossible
where
	taskIdUpd	= if (opts1.UIEditOpts.taskId == opts2.UIEditOpts.taskId) [] [UISetTaskId (toString path) opts2.UIEditOpts.taskId]
	editorIdUpd = if (opts1.UIEditOpts.editorId == opts2.UIEditOpts.editorId) [] [UISetEditorId (toString path) opts2.UIEditOpts.editorId]
	valueUpd
		| eventMatch opts2 event
			# value2 = encodeUIValue opts2.UIEditOpts.value
			= if (eventValue event === value2) [] [UISetValue (toString path) value2]
		| otherwise 
			= if (opts1.UIEditOpts.value === opts2.UIEditOpts.value) [] [UISetValue (toString path) (encodeUIValue opts2.UIEditOpts.value)]

	eventMatch {UIEditOpts|taskId,editorId} (EditEvent matchTask matchEditor _) = (taskId == toString matchTask) && (editorId == matchEditor)
	eventMatch _ _ = False
	
	eventValue (EditEvent _ _ value) = value
	
diffChoiceOpts :: DiffPath (UIChoiceOpts a) (UIChoiceOpts a) -> DiffResult | gEq{|*|} a & JSONEncode{|*|} a
diffChoiceOpts path opts1 opts2 = DiffImpossible

diffActionOpts :: DiffPath UIActionOpts UIActionOpts -> DiffResult
diffActionOpts path opts1 opts2 = diffOpts opts1 opts2

diffLayoutOpts :: DiffPath UILayoutOpts UILayoutOpts -> DiffResult
diffLayoutOpts path opts1 opts2 = diffOpts opts1 opts2

diffOpts :: a a -> DiffResult | gEq{|*|} a	//Very crude, but always working fallback diff
diffOpts opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffImpossible

diffItems :: DiffPath Event [UIControl] [UIControl] -> [UIUpdate]
diffItems path event items1 items2 = diff path event 0 items1 items2
where
	diff path event i [] []
		= []
	diff path event i items1 [] //Less items in new than old (remove starting with the last item)
		= [UIRemove (toString path) n \\ n <- reverse [i.. i + length items1 - 1 ]] 
	diff path event i [] items2 //More items in new than old
		= [UIAdd (toString path) n def \\ n <- [i..] & def <- items2]	
	diff path event i [c1:c1s] [c2:c2s] //Compare side by side
		=	replaceIfImpossible [ItemStep i:path] c2 [diffControls [ItemStep i:path] event c1 c2]
		++  diff path event (i + 1) c1s c2s
		
//Try to diff a control in parts. If one of the parts is impossible, then return a full replace instruction
replaceIfImpossible :: DiffPath UIControl [DiffResult] -> [UIUpdate]
replaceIfImpossible path fallback parts
	| allPossible parts		= foldr (++) [] [d \\DiffPossible d <- parts]
							= [UIReplace (toString parentPath) parentIndex fallback]
where
	[ItemStep parentIndex:parentPath] = path
	
	allPossible [] 						= True
	allPossible [DiffImpossible:_]		= False
	allPossible [(DiffPossible _):ps]	= allPossible ps

/*
where
	diffEditorDefinitions`` :: !(Maybe EditEvent) !UIControlContent !UIControlContent -> Maybe [UIUpdate]
	diffEditorDefinitions`` event old new = case (old,new) of
		// Documents are replaced if their value has changed
		(UIEditControl (UIDocumentControl odoc) oc, UIEditControl (UIDocumentControl ndoc) nc)
			| odoc == ndoc && oc.UIEditControl.taskId == nc.UIEditControl.taskId && oc.UIEditControl.name == nc.UIEditControl.name
				= Just []
			| otherwise
				= Nothing
		(UIEditControl (UIGridControl ogrid) _, UIEditControl (UIGridControl ngrid) _)
			| ogrid =!= ngrid	= Nothing
		(UIEditControl otype oc, UIEditControl ntype nc)
			| otype === ntype
				= Just (valueUpdate path event oc nc ++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(UIShowControl otype oc, UIShowControl ntype nc)
			| otype === ntype && oc.UIShowControl.value === nc.UIShowControl.value
				= Just []
			| otherwise
				= Nothing
		(UIButton o,UIButton n)
			| o.UIButton.text == n.UIButton.text && o.UIButton.iconCls == n.UIButton.iconCls
				= Just (update (\o n -> o.UIButton.disabled == n.UIButton.disabled) (\b -> Just (not b.UIButton.disabled)) UISetEnabled path o n
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(UIContainer o, UIContainer n)
			|  (o.UIContainer.direction === n.UIContainer.direction
				&& o.UIContainer.halign === n.UIContainer.halign
				&& o.UIContainer.valign === n.UIContainer.valign)
				= Just (diffChildEditorDefinitions path event o.UIContainer.items n.UIContainer.items)
		(UIPanel o, UIPanel n)
			| ( o.UIPanel.direction === n.UIPanel.direction
				&& o.UIPanel.halign === n.UIPanel.halign
				&& o.UIPanel.valign === n.UIPanel.valign
				&& o.UIPanel.frame === n.UIPanel.frame
				&& o.UIPanel.menus === n.UIPanel.menus
				&& (isJust o.UIPanel.iconCls == isJust n.UIPanel.iconCls))
					# titleUpdate	= update (\o n -> o.UIPanel.title === n.UIPanel.title && o.UIPanel.iconCls == n.UIPanel.iconCls) (\{UIPanel|title,iconCls} -> Just (fromMaybe "" title,iconCls)) UISetTitle path o n
					# itemUpdates	= diffChildEditorDefinitions path event o.UIPanel.items n.UIPanel.items
					# menuUpdates	= []
					//# menuUpdates	= diffUIMenus path o.UIPanel.menus n.UIPanel.menus
					= Just (titleUpdate ++ itemUpdates ++ menuUpdates)
		(UIWindow o, UIWindow n)
			|  (o.UIWindow.direction === n.UIWindow.direction
				&& o.UIWindow.halign === n.UIWindow.halign
				&& o.UIWindow.valign === n.UIWindow.valign)
				= Just (diffChildEditorDefinitions path event o.UIWindow.items n.UIWindow.items)
		(UIListContainer lcOld, UIListContainer lcNew)	
			= Just (diffChildEditorDefinitions path event (items lcOld) (items lcNew)
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
			where
				items lc = [{content = UIListItem item, width = Nothing, height = Nothing, margins = Nothing} \\ item <- lc.UIListContainer.items]
		(UIListItem liOld, UIListItem liNew)
			= Just (diffChildEditorDefinitions path event [liOld.UIListItem.items] [liNew.UIListItem.items])
		(UITabContainer tcOld, UITabContainer tcNew)
			# activeTabUpdate	= update (\o n -> o.UITabContainer.active == n.UITabContainer.active) (\{UITabContainer|active} -> Just active) UISetActiveTab path tcOld tcNew
			# itemUpdates 		= diffChildEditorDefinitions path event (items tcOld) (items tcNew)
			= Just (itemUpdates ++ activeTabUpdate)
			where
				items tc = [{content = UITabItem item, width = Nothing, height = Nothing, margins = Nothing} \\ item <- tc.UITabContainer.items]
		(UITabItem o, UITabItem n)
			| (o.UITabItem.closeAction === n.UITabItem.closeAction //Can't diff the close action for now
				&& o.UITabItem.menus === n.UITabItem.menus)		//Diff of menus is also still impossible
					# titleUpdate	= update (\o n -> o.UITabItem.title == n.UITabItem.title && o.UITabItem.iconCls == n.UITabItem.iconCls) (\{UITabItem|title,iconCls} -> Just (title,iconCls)) UISetTitle path o n
					# itemUpdates	= diffChildEditorDefinitions path event o.UITabItem.items n.UITabItem.items
					# menuUpdates 	= []
					= Just (titleUpdate ++ itemUpdates ++ menuUpdates)
			| otherwise
				= Nothing
		(UIIcon o, UIIcon n)
			| o.UIIcon.type == n.UIIcon.type
				&& o.UIIcon.tooltip === n.UIIcon.tooltip
					= Just []	
		// Custom components need to figure out their own update on the client side
		(UICustom oc, UICustom nc)
			| oc === nc	= Just []
			| otherwise	= Just [UIUpdate (toString path) newTui]
		// Fallback: always replace
		_	= Nothing
	
	//Determine the updates for child items in containers, lists etc	
	diffChildEditorDefinitions :: DiffPath (Maybe EditEvent) [UIControl] [UIControl] -> [UIUpdate]
	diffChildEditorDefinitions path event old new = diffChildEditorDefinitions` path event 0 old new
	where
		diffChildEditorDefinitions` path event i [] []
			= []
		diffChildEditorDefinitions` path event i old []
			//Less items in new than old (remove starting with the last item)
			= [UIRemove (toString path) n \\ n <- reverse [i.. i + length old - 1 ]] 
		diffChildEditorDefinitions` path event i [] new
			//More items in new than old
			= [UIAdd (toString path) n def \\ n <- [i..] & def <- new] 
		diffChildEditorDefinitions` path event i [o:os] [n:ns] 
			=	(diffEditorDefinitions` [ItemStep i:path] event o n)
			++  (diffChildEditorDefinitions` path event (i + 1) os ns)

//Update the value of a control
valueUpdate path mbEvent old new = update (sameValue mbEvent) (\{UIEditControl|value} -> Just value) UISetValue path old new
where
	sameValue Nothing old new
			= old.UIEditControl.value == new.UIEditControl.value
	sameValue (Just (TaskEvent eTask (eName,eValue))) old new
		| old.UIEditControl.taskId == Just (toString eTask) && old.UIEditControl.name == eName
			= eValue  == new.UIEditControl.value
			= old.UIEditControl.value == new.UIEditControl.value

//Update the task id of a control
taskIdUpdate path old new	= update sameTaskId taskIdOf UISetTaskId path old new

//Update the name of a control
nameUpdate path old new		= update sameName nameOf UISetName path old new

update eqfun accfun consfun path old new
	| not (eqfun old new)	= maybe [] (\prop -> [consfun (toString path) prop]) (accfun new)
	| otherwise				= []

//If the menus are not exactly the same simply replace all of them 
diffUIMenus :: DiffPath [UIMenuButton] [UIMenuButton] -> [UIUpdate]
diffUIMenus path old new
	| old === new	= []
	| otherwise		=  reverse [UIRemove menupath i \\ i <- [0.. (length old - 1)]]
					++ [UIAdd menupath i (tuidef b) \\ i <- [0..] & b <- new]
where
	menupath = toString [MenuStep:path]	
	tuidef b	= {UIControl| content = UIMenuButton b, width = Nothing, height = Nothing, margins = Nothing}
	
sameTaskId :: !UIControlContent !UIControlContent -> Bool
sameTaskId a b = (taskIdOf a) == (taskIdOf b)

sameName :: !UIControlContent !UIControlContent -> Bool
sameName a b = (nameOf a) == (nameOf b)

taskIdOf :: !UIControlContent -> Maybe String
taskIdOf (UIEditControl _ {UIEditControl|taskId})		= taskId
taskIdOf (UIButton {UIButton|taskId})					= taskId
taskIdOf (UIListContainer {UIListContainer|taskId})		= taskId
taskIdOf _												= Nothing

nameOf :: !UIControlContent -> Maybe String
nameOf (UIEditControl _ {UIEditControl|name})			= Just name
nameOf (UIButton {UIButton|name})						= Just name
nameOf (UIListContainer {UIListContainer|name})			= name
nameOf _												= Nothing
*/
encodeUIUpdates :: ![UIUpdate] -> JSONNode
encodeUIUpdates updates = JSONArray (flatten (map encodeUIUpdate updates))

encodeUIUpdate :: UIUpdate -> [JSONNode]
encodeUIUpdate (UISetValue path value)			= [node path "setValue"			[value]]
encodeUIUpdate (UISetTaskId path taskId)		= [node path "setTaskId"	 	[JSONString taskId]]
encodeUIUpdate (UISetName path name)			= [node path "setName"			[JSONString name]]
encodeUIUpdate (UISetEnabled path enabled)		= [node path "setDisabled"		[JSONBool (not enabled)]]
encodeUIUpdate (UISetActive path active)		= [node path "setActive"		[JSONBool active]]
encodeUIUpdate (UISetTitle path title)			= [node path "setTitle"			[JSONString title]]
encodeUIUpdate (UIReplace path index def)		= [node path "replace" 			[JSONInt index, encodeUIControl def]]
encodeUIUpdate (UIUpdate path def)				= [node path "update"			[encodeUIControl def]]
encodeUIUpdate (UIAdd path index def)			= [node path "insert"			[JSONInt index, encodeUIControl def]]
encodeUIUpdate (UIRemove path index)			= [node path "remove"			[JSONInt index]]
encodeUIUpdate _								= []

node path method arguments
	= JSONObject [("path",JSONString path),("method",JSONString method),("arguments",JSONArray arguments)]

