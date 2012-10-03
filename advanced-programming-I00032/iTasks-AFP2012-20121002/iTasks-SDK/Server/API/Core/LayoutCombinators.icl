implementation module LayoutCombinators

import StdTuple, StdList, StdBool, StdOrdList
import Maybe, Text, Tuple, Map, Util, HtmlUtil
import SystemTypes, UIDefinition

from StdFunc import o

from Task import :: TaskCompositionType, :: TaskCompositionType(..)
from TaskState import :: TIMeta(..)
derive gEq TaskCompositionType

autoLayout :: Layout
autoLayout = {editor = autoEditorLayout, interact = autoInteractionLayout ,step = autoStepLayout
			 ,parallel = autoParallelLayout, workOn = autoWorkOnLayout, final = autoFinalLayout}

/**
* The basic data layout groups the controls of a part of a compound datastructure in a fieldset
*/
autoEditorLayout :: UIControlSequence -> UIAnnotatedControls
autoEditorLayout (attributes,[],_)		= []
autoEditorLayout (attributes,controls,direction)
	= [(defToContainer (layoutControls (UIControlSequence (attributes,controls,direction))),attributes)]
/**
* The basic interaction layout simply decorates the prompt and merges it with the editor.
*/
autoInteractionLayout :: UIControlSequence UIControlSequence -> UIControlSequence
autoInteractionLayout prompt=:(pattr,pcontrols,_) editor=:(eattr,econtrols,_)
	= (mergeAttributes pattr eattr, [(c,newMap)\\ c <- decoratePrompt pcontrols] ++ econtrols, Vertical)
/**
* Adds actions to a partial user interface definition
*/
autoStepLayout :: UIDef [UIAction]-> UIDef
autoStepLayout (UIControlSequence (attributes,controls,direction)) actions
	//Recognize special case of a complete empty interaction wrapped in a step as an actionset
	| isEmpty controls && isEmpty (toList attributes)
		= UIActionSet actions
	//Promote the control sequence to a control group because they are grouped by the step combinator
		= UIControlGroup (attributes,controls,direction,actions)
autoStepLayout (UIAbstractContainer (attributes,controls,direction,actions)) stepActions
	//If an abstract container is placed under a step container, we add the actions to the remaining actions
	= UIAbstractContainer (attributes,controls,direction,actions ++ stepActions)
autoStepLayout def _
	//In other cases, we ignore the actions because they do not make sense in this context
	= def

/**
* The default parallel composition only merges the prompt of the parallel with
* the definitions of the constituents.
*/
autoParallelLayout :: UIControlSequence [UIDef] -> UIDef
autoParallelLayout prompt defs	
	| allPartial defs			= partialMerge prompt defs
	| additionalActions defs	= additionalActionMerge defs
								= sequenceMerge prompt defs
where
	allPartial [] = True
	allPartial [UIControlSequence _:ds]	= allPartial ds
	allPartial _						= False

	additionalActions defs = scan False False defs
	where
		scan anyActionSet oneOther []					= anyActionSet && oneOther		//If we found exactly one def other than UIActionSet we can apply the special rule
		scan anyActionSet oneOther [UIActionSet _:ds]	= scan True oneOther ds			//Keep going...
		scan anyActionSet False [d:ds]					= scan anyActionSet True ds		//We found the first def other than UIActionSet
		scan anyActionSet True [d:ds]					= False							//Oops, more than one def other than UIActionSet 

/**
* Overrule the title attribute with the title in the task meta data
*/
autoWorkOnLayout :: UIDef TIMeta -> UIDef
autoWorkOnLayout def meta=:{TIMeta|management}
	= maybe def (\title -> uiDefSetAttribute TITLE_ATTRIBUTE title def) management.ManagementMeta.title
	
/**
* Add actions and frame the content
*/
autoFinalLayout :: UIDef -> UIFinal	//TODO: Size should be minWidth, but that doesn't seem to work yet...
autoFinalLayout (UIControlSequence (attributes,controls,direction))
	= (decorateControls controls,get TITLE_ATTRIBUTE attributes)
autoFinalLayout def=:(UIControlGroup (attributes,controls,direction,actions))
	# (actions,panel) = placeActions actions (defToPanel (layoutControls def))
	= ([(setSize WrapSize WrapSize o setFramed True) panel], get TITLE_ATTRIBUTE attributes)
autoFinalLayout (UIActionSet actions)							= ([],Nothing)
autoFinalLayout def=:(UIAbstractContainer (attributes,controls,actions,direction))
	= ([defToPanel def],get TITLE_ATTRIBUTE attributes)
autoFinalLayout (UIFinal final)									= final

/**
* Adds hints, labels etc to a set of controls
* This transforms UIControlGroups or UIControlSequences to UI
*/
layoutControls :: UIDef -> UIDef
layoutControls (UIControlSequence (attributes,controls,direction))
	= layoutControls (UIControlGroup (attributes,controls,direction,[]))
layoutControls (UIControlGroup (attributes,controls,direction,actions))
	//Decorate & size the controls
	= UIAbstractContainer (attributes, decorateControls controls, direction, actions)
layoutControls def = def

//Add labels and icons to a set of controls if they have any of those attributes set
decorateControls :: UIAnnotatedControls -> UIControls
decorateControls  controls = mapLst decorateControl controls
where
	mapLst f [] = []
	mapLst f [x] = [f True x]
	mapLst f [x:xs] = [f False x: mapLst f xs]
	
decorateControl :: Bool (!UIControl,!UIAttributes) -> UIControl
decorateControl last (control,attributes)
	# mbLabel 	= get LABEL_ATTRIBUTE attributes
	# mbHint 	= get HINT_ATTRIBUTE attributes
	# mbValid	= get VALID_ATTRIBUTE attributes
	# mbError	= get ERROR_ATTRIBUTE attributes
	# hasMargin	= hasMargin control
	# noMargins	= noMarginControl control
	= case (mbLabel,mbHint,mbValid,mbError) of
		(Nothing,Nothing,Nothing,Nothing)	//Just set margins
			| hasMargin	= control
						= if noMargins
							(setMargins 0 0 0 0 control)
							(if last (setMargins 0 5 5 5 control) (setMargins 0 5 0 5 control))

		_									//Add decoration													
			# control = row (labelCtrl mbLabel ++ [control] ++ iconCtrl mbHint mbValid mbError) 
			= if noMargins
				(setMargins 0 0 0 0 control)
				(if last (setMargins 0 5 5 5 control) (setMargins 0 5 0 5 control))		
where
	row ctrls				= (setSize FlexSize WrapSize o setDirection Horizontal) (defaultContainer ctrls)
	
	labelCtrl (Just label)	= [setWidth (ExactSize 100) (stringDisplay label)]
	labelCtrl Nothing		= []
	
	iconCtrl (Just msg) _ _	= icon "icon-hint" msg
	iconCtrl _ (Just msg) _	= icon "icon-valid" msg
	iconCtrl _ _ (Just msg)	= icon "icon-invalid" msg
	iconCtrl _ _ _			= []
	
	icon cls tooltip		= [setLeftMargin 5 (UIIcon defaultSizeOpts {UIIconOpts|iconCls = cls, tooltip = Just tooltip})]

	hasMargin control = isJust (getSizeOpts control).UISizeOpts.margins

	noMarginControl	(UIPanel _ _ _ _)	= True
	noMarginControl	(UIGrid _ _ _)		= True
	noMarginControl	(UITree _ _)		= True
	noMarginControl _					= False

//Wrap the controls of the prompt in a container with a nice css class and add some bottom margin
decoratePrompt :: [(UIControl,UIAttributes)] -> [UIControl]
decoratePrompt []		= []
decoratePrompt controls	= [UIContainer sizeOpts defaultLayoutOpts (map fst controls) containerOpts]
where
	sizeOpts = {defaultSizeOpts & margins = Just {top= 5, right = 5, bottom = 10, left = 5}
			   , width = Just FlexSize, minWidth = Just WrapMin, height = Just WrapSize}
	containerOpts = {UIContainerOpts|baseCls=Just "itwc-prompt", bodyCls=Nothing}

//Create a single container control
defToContainer :: UIDef -> UIControl
defToContainer def = UIContainer sizeOpts layoutOpts (uiDefControls def) containerOpts 
where
	containerOpts	= {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}
	layoutOpts		= {UILayoutOpts|defaultLayoutOpts & direction = (uiDefDirection def)}
	sizeOpts		= {UISizeOpts|defaultSizeOpts & width = Just FlexSize}
//Create a single panel control
defToPanel :: UIDef -> UIControl
defToPanel def = UIPanel sizeOpts layoutOpts (uiDefControls def) panelOpts
where
	panelOpts = {UIPanelOpts|title = title,frame = False, tbar = Nothing
					,iconCls = iconCls, baseCls = Nothing, bodyCls = Nothing}
	layoutOpts = {UILayoutOpts|defaultLayoutOpts & direction = (uiDefDirection def)}
	sizeOpts		= {UISizeOpts|defaultSizeOpts & width = Just FlexSize}
	
	title		= get TITLE_ATTRIBUTE attributes	
	iconCls		= fmap (\icon -> "icon-" +++ icon) (get ICON_ATTRIBUTE attributes)
	attributes	= uiDefAttributes def

//Create a single control that represents the ui definition
//This can be a container, a panel or just a single control such as a textarea, a grid or a tree
defToControl :: UIDef -> UIControl
defToControl def
	| isJust (get TITLE_ATTRIBUTE (uiDefAttributes def)) //If a title attribute is set, always make a panel
		= defToPanel def
	| otherwise
		= case uiDefControls def of
			[c=:(UIContainer _ _ _ _)]	= c //Already a container, no need to double wrap
			[c=:(UIPanel _ _ _ _)]		= c	//Idem...
			_							= defToContainer def
		

placeActions :: [UIAction] UIControl -> ([UIAction],UIControl)
placeActions actions (UIPanel sOpts lOpts items opts)
	//Place button actions
	# (buttons,actions)	= actionsToButtons actions	
	# items				= if (isEmpty buttons) items (items ++ [buttonPanel buttons])
	//Place menu actions
	# (menus,actions)	= actionsToMenus actions
	# opts				= case menus of
		[]	= opts
		_	= {UIPanelOpts|opts & tbar = Just menus}
	= (actions, UIPanel sOpts lOpts items opts)
placeActions actions (UIContainer sOpts lOpts items opts)
	//Place button actions
	# (buttons,actions)	= actionsToButtons actions	
	# items				= if (isEmpty buttons) items (items ++ [buttonPanel buttons])
	= (actions, UIContainer sOpts lOpts items opts)
placeActions actions control = (actions,control)

//Merge the fragments of a composed interactive task into a single definition
partialMerge :: UIControlSequence [UIDef] -> UIDef
partialMerge prompt=:(attributes,pcontrols,_) defs
	# controls		= foldr (++) [(c,newMap) \\ c <- decoratePrompt pcontrols] (map uiDefAnnotatedControls defs)
	//Determine title: if prompt has title use it, else combine titles 
	# attributes = case (get TITLE_ATTRIBUTE attributes, collectTitles defs) of
		(Just _,_) 	= attributes //Title already set, do nothing
		(_,[])		= attributes //None of the parts have a title, do nothing
		(_,titles)	= put TITLE_ATTRIBUTE (join ", " titles) attributes	//Set the joined titles
	= UIControlSequence (attributes,controls,Vertical)
where
	collectTitles defs = [title \\ Just title <- [get TITLE_ATTRIBUTE (uiDefAttributes d) \\d <- defs]]

//Adds the actions of ActionSet defs to an existing other definition
additionalActionMerge :: [UIDef] -> UIDef
additionalActionMerge defs
	# (def,additional)	= collect Nothing [] defs
	= case def of
		UIControlSequence (attributes,controls,direction)			= UIControlGroup (attributes,controls,direction,additional) 
		UIControlGroup (attributes,controls,direction,actions)		= UIControlGroup (attributes,controls,direction,actions ++ additional)
		UIAbstractContainer (attributes,controls,direction,actions)	= UIAbstractContainer (attributes,controls,direction,actions ++ additional)
		_															= def
where
	collect mbd actions []					= (fromMaybe (UIActionSet actions) mbd,actions)
	collect mbd	actions [UIActionSet a:ds]	= collect mbd (actions ++ a) ds
	collect mbd actions [d:ds]				= collect (Just d) actions ds
	
//Create groups for all definitions in the list
sequenceMerge :: ParallelLayout
sequenceMerge = merge
where
	merge prompt=:(attributes,pcontrols,direction) defs
		# (actions,controls)	= unzip [placeActions (uiDefActions d) (defToPanel (layoutControls d)) \\ d <- defs]
		# controls				= decoratePrompt pcontrols ++ controls
		# actions				= foldr (++) [] actions
		= UIAbstractContainer (attributes, controls, direction, actions)

sideMerge :: UISide Int ParallelLayout -> ParallelLayout
sideMerge side size restMerge = merge
where
	merge prompt []		= UIControlSequence prompt
	
	merge prompt=:(attributes,_,_) parts
		# (direction,sidePart,restParts) = case side of
			TopSide		= (Vertical, hd parts,tl parts)
			RightSide	= (Horizontal, last parts,init parts)
			BottomSide	= (Vertical, last parts,init parts)
			LeftSide	= (Horizontal, hd parts, tl parts)
		# restPart		= (restMerge noPrompt restParts)
		
		# (sideA,sideUI)	= placeActions (uiDefActions sidePart) (defToControl (layoutControls sidePart))
		# (restA,restUI)	= placeActions (uiDefActions restPart) (defToControl (layoutControls restPart))
		# sideUI			= (ifH direction (setWidth (ExactSize size)) (setHeight (ExactSize size))) (fill sideUI)
		# restUI			= fill restUI
		# controls			= ifTL side [sideUI,restUI] [restUI,sideUI]
		= UIAbstractContainer (attributes, controls, direction, sideA ++ restA)
		
	noPrompt = (newMap,[],Vertical)
	
	ifTL TopSide a b = a
	ifTL LeftSide a b = a
	ifTL _ a b = b
	
	ifH Horizontal a b = a
	ifH _ a b = b
	
tabbedMerge :: ParallelLayout
tabbedMerge = merge
where
	merge prompt=:(attributes,pcontrols,_) defs
		# pcontrols					= decoratePrompt pcontrols
		# (activeIndex,activeDef)	= findActive defs	
		# (tabBar,actions)			= mkTabs activeIndex defs	
		# (actions,tabContent)		= maybe ([],defaultPanel []) toTabContent activeDef
		# controls					= pcontrols ++ [tabBar,tabContent]
		= UIAbstractContainer (attributes, controls, Vertical, []) 

	toTabContent def
		# def = tweakAttr (del TITLE_ATTRIBUTE) def	//No double titles
		# def = removeCloseAction def				//Close actions are managed via the tabs
		= placeActions (uiDefActions def) (defToPanel (layoutControls def))
		
	findActive defs = find 0 (0,Nothing) defs
	where
		find i bestSoFar [] = bestSoFar
		find i (_,Nothing) [d:ds]	= find (i+1) (i,Just d) ds
		find i bestSoFar=:(_,Just best) [d:ds]	= if (later d best) (find (i+1) (i,Just d) ds) (find (i+1) bestSoFar ds)
		
		later defA defB
			# a = uiDefAttributes defA
			# b = uiDefAttributes defB
			= case (get LAST_EVENT_ATTRIBUTE a,get LAST_EVENT_ATTRIBUTE b) of
			(Just ta,Just tb)
				| ta == tb	//If the last event time is the same, then we compare creation times to which tab is newest
					= case (get CREATED_AT_ATTRIBUTE a, get CREATED_AT_ATTRIBUTE b) of
						(Just ca,Just cb)	= toInt ca > toInt cb
						_					= False
				| otherwise	
					= toInt ta > toInt tb
			(Just _,Nothing)	= True
			_					= False
			
		
	mkTabs active defs
		# (tabs,actions) = unzip [mkTab (i == active) d \\ d <- defs & i <- [0..]]
		= ((setDirection Horizontal o setHeight WrapSize o setBaseCls "x-tab-bar") (defaultContainer tabs),foldr (++) [] actions)

	mkTab active def
		# attributes			= uiDefAttributes def
		# actions				= uiDefActions def
		# taskId				= get TASK_ATTRIBUTE attributes
		# iconCls				= fmap (\i -> "icon-" +++ i) (get ICON_ATTRIBUTE attributes)
		# text					= fromMaybe "Untitled" (get TITLE_ATTRIBUTE attributes)
		# (close,actions)		= searchCloseAction actions
		# tabOpts				= {text = text ,focusTaskId = taskId,active = active,closeTaskId = close,iconCls=iconCls}
		= (UITab defaultSizeOpts tabOpts, if active actions [])
	
	searchCloseAction [] = (Nothing,[])
	searchCloseAction [{taskId,action=ActionClose,enabled}:as] = (if enabled (Just taskId) Nothing,as)
	searchCloseAction [a:as] = let (mbtask,as`) = searchCloseAction as in (mbtask,[a:as`])

	removeCloseAction (UIControlGroup (attributes,controls,direction,actions)) 
		= UIControlGroup (attributes,controls,direction,filter notClose actions)
	removeCloseAction (UIAbstractContainer (attributes,controls,direction,actions))
		= UIAbstractContainer (attributes,controls,direction,filter notClose actions)
	removeCloseAction def
		= def
	
	notClose {UIAction|action=ActionClose}	= False
	notClose _								= True
		
hideLayout :: Layout
hideLayout =
	{ editor	= \(a,c,_)			-> []
	, interact	= \prompt editor	-> (mergeAttributes (fst3 prompt) (fst3 editor), [], Vertical)
	, step		= \def actions		-> noControls (addActions actions def)
	, parallel	= \(a,c,d) defs		-> noControls (foldr mergeDefs (UIControlGroup (a,c,d,[])) defs)
	, workOn	= \def meta			-> noControls def
	, final		= \def				-> (uiDefControls (noControls def),Nothing)
	}
where
	noControls (UIControlGroup (attributes,_,direction,actions)) = UIControlGroup (attributes,[],direction,actions)
	noControls def = def
	
	addActions extra (UIControlGroup (attributes,controls,direction,actions)) = UIControlGroup (attributes,controls,direction,actions ++ extra)
	addActions extra def = def
	
	
	mergeDefs :: UIDef UIDef -> UIDef
	mergeDefs (UIControlGroup (at1,ct1,d1,ac1)) (UIControlGroup (at2,ct2,d2,ac2))
		= UIControlGroup (mergeAttributes at1 at2, ct1 ++ ct2, d1, ac1 ++ ac2)
	mergeDefs d1 d2 = d1	//TODO: Define other merge options

customMergeLayout :: ParallelLayout -> Layout
customMergeLayout merger = {autoLayout & parallel = merger}

partLayout :: Int -> Layout
partLayout idx = {autoLayout & parallel = layout}
where	
	layout prompt parts
		| idx < length parts	= (parts !! idx)
		| otherwise				= autoParallelLayout prompt parts

updSizeOpts :: (UISizeOpts -> UISizeOpts) UIControl -> UIControl
updSizeOpts f (UIViewString	sOpts vOpts)			= (UIViewString	(f sOpts) vOpts)
updSizeOpts f (UIViewHtml sOpts vOpts)				= (UIViewHtml (f sOpts) vOpts)
updSizeOpts f (UIViewDocument sOpts vOpts)			= (UIViewDocument (f sOpts) vOpts)
updSizeOpts f (UIViewCheckbox sOpts vOpts)			= (UIViewCheckbox (f sOpts) vOpts)
updSizeOpts f (UIViewSlider sOpts vOpts opts)		= (UIViewSlider (f sOpts) vOpts opts)
updSizeOpts f (UIViewProgress sOpts vOpts opts)		= (UIViewProgress (f sOpts) vOpts opts)
updSizeOpts f (UIEditString	sOpts eOpts)			= (UIEditString	(f sOpts) eOpts)
updSizeOpts f (UIEditNote sOpts eOpts)				= (UIEditNote (f sOpts) eOpts)
updSizeOpts f (UIEditPassword sOpts eOpts)			= (UIEditPassword (f sOpts) eOpts)
updSizeOpts f (UIEditInt sOpts eOpts)				= (UIEditInt (f sOpts) eOpts)
updSizeOpts f (UIEditDecimal sOpts eOpts)			= (UIEditDecimal (f sOpts) eOpts)
updSizeOpts f (UIEditCheckbox sOpts eOpts)			= (UIEditCheckbox (f sOpts) eOpts)
updSizeOpts f (UIEditSlider sOpts eOpts opts)		= (UIEditSlider (f sOpts) eOpts opts)
updSizeOpts f (UIEditDate sOpts eOpts)				= (UIEditDate (f sOpts) eOpts)
updSizeOpts f (UIEditTime sOpts eOpts)				= (UIEditTime (f sOpts) eOpts)
updSizeOpts f (UIEditDocument sOpts eOpts)			= (UIEditDocument (f sOpts) eOpts)
updSizeOpts f (UIEditButton	sOpts eOpts opts)		= (UIEditButton	(f sOpts) eOpts opts)
updSizeOpts f (UIEditGoogleMap sOpts eOpts opts)	= (UIEditGoogleMap (f sOpts) eOpts opts)
updSizeOpts f (UIDropdown sOpts cOpts)				= (UIDropdown (f sOpts) cOpts)
updSizeOpts f (UIGrid sOpts cOpts opts)				= (UIGrid (f sOpts) cOpts opts)
updSizeOpts f (UITree sOpts cOpts)					= (UITree (f sOpts) cOpts)
updSizeOpts f (UIActionButton sOpts aOpts opts)		= (UIActionButton (f sOpts) aOpts opts)	
updSizeOpts f (UIMenuButton	sOpts opts)				= (UIMenuButton	(f sOpts) opts)	
updSizeOpts f (UILabel sOpts opts)					= (UILabel (f sOpts) opts)
updSizeOpts f (UIIcon sOpts opts)					= (UIIcon (f sOpts) opts)
updSizeOpts f (UITab sOpts opts)					= (UITab (f sOpts) opts)
updSizeOpts f (UITasklet sOpts opts)				= (UITasklet (f sOpts) opts)
updSizeOpts f (UIContainer sOpts lOpts items opts)	= (UIContainer (f sOpts) lOpts items opts)
updSizeOpts f (UIPanel sOpts lOpts items opts)		= (UIPanel (f sOpts) lOpts items opts)
updSizeOpts f (UIFieldSet sOpts lOpts items opts)	= (UIFieldSet (f sOpts) lOpts items opts)
updSizeOpts f (UIWindow	sOpts lOpts items opts)		= (UIWindow	(f sOpts) lOpts items opts)

getSizeOpts :: UIControl -> UISizeOpts
getSizeOpts (UIViewString	sOpts vOpts)			= sOpts
getSizeOpts (UIViewHtml sOpts vOpts)				= sOpts
getSizeOpts (UIViewDocument sOpts vOpts)			= sOpts
getSizeOpts (UIViewCheckbox sOpts vOpts)			= sOpts
getSizeOpts (UIViewSlider sOpts vOpts opts)			= sOpts
getSizeOpts (UIViewProgress sOpts vOpts opts)		= sOpts
getSizeOpts (UIEditString	sOpts eOpts)			= sOpts
getSizeOpts (UIEditNote sOpts eOpts)				= sOpts
getSizeOpts (UIEditPassword sOpts eOpts)			= sOpts
getSizeOpts (UIEditInt sOpts eOpts)					= sOpts
getSizeOpts (UIEditDecimal sOpts eOpts)				= sOpts
getSizeOpts (UIEditCheckbox sOpts eOpts)			= sOpts
getSizeOpts (UIEditSlider sOpts eOpts opts)			= sOpts
getSizeOpts (UIEditDate sOpts eOpts)				= sOpts
getSizeOpts (UIEditTime sOpts eOpts)				= sOpts
getSizeOpts (UIEditDocument sOpts eOpts)			= sOpts
getSizeOpts (UIEditButton sOpts eOpts opts)			= sOpts
getSizeOpts (UIEditGoogleMap sOpts eOpts opts)		= sOpts
getSizeOpts (UIDropdown sOpts cOpts)				= sOpts
getSizeOpts (UIGrid sOpts cOpts opts)				= sOpts
getSizeOpts (UITree sOpts cOpts)					= sOpts
getSizeOpts (UIActionButton sOpts aOpts opts)		= sOpts	
getSizeOpts (UIMenuButton	sOpts opts)				= sOpts	
getSizeOpts (UILabel sOpts opts)					= sOpts
getSizeOpts (UIIcon sOpts opts)						= sOpts
getSizeOpts (UITab sOpts opts)						= sOpts
getSizeOpts (UITasklet sOpts opts)					= sOpts
getSizeOpts (UIContainer sOpts lOpts items opts)	= sOpts
getSizeOpts (UIPanel sOpts lOpts items opts)		= sOpts
getSizeOpts (UIFieldSet sOpts lOpts items opts)		= sOpts
getSizeOpts (UIWindow	sOpts lOpts items opts)		= sOpts


setSize :: !UISize !UISize !UIControl -> UIControl
setSize width height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width, height = Just height}) ctrl

setWidth :: !UISize !UIControl -> UIControl
setWidth width ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width}) ctrl

setHeight :: !UISize !UIControl -> UIControl
setHeight height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & height = Just height}) ctrl

setMinSize :: !UIMinSize !UIMinSize !UIControl -> UIControl
setMinSize width height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just width, minHeight = Just height}) ctrl

setMinWidth :: !UIMinSize !UIControl -> UIControl
setMinWidth width ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just width}) ctrl

setMinHeight :: !UIMinSize !UIControl -> UIControl
setMinHeight height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minHeight = Just height}) ctrl

fill :: !UIControl -> UIControl
fill ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just FlexSize, height = Just FlexSize}) ctrl

fillHeight :: !UIControl -> UIControl
fillHeight ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just FlexSize}) ctrl

fillWidth :: !UIControl -> UIControl
fillWidth ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just FlexSize}) ctrl

fixedHeight	:: !Int !UIControl -> UIControl
fixedHeight size ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just (ExactSize size)}) ctrl

fixedWidth :: !Int !UIControl -> UIControl
fixedWidth size ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just (ExactSize size)}) ctrl

wrapHeight :: !UIControl -> UIControl
wrapHeight ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just WrapSize}) ctrl
 
wrapWidth :: !UIControl -> UIControl
wrapWidth ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just WrapSize}) ctrl

setMargins :: !Int !Int !Int !Int !UIControl -> UIControl
setMargins top right bottom left ctrl
	= updSizeOpts (\opts -> {UISizeOpts|opts & margins = Just {top = top, right = right, bottom = bottom, left = left}}) ctrl

setTopMargin :: !Int !UIControl -> UIControl
setTopMargin top ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = top, right = 0, bottom = 0, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & top = top}}

setRightMargin	:: !Int !UIControl -> UIControl
setRightMargin right ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = right, bottom = 0, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & right = right}}
	
setBottomMargin	:: !Int !UIControl -> UIControl
setBottomMargin bottom ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = 0, bottom = bottom, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & bottom = bottom}}
	
setLeftMargin :: !Int !UIControl -> UIControl
setLeftMargin left ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = 0, bottom = 0, left = left}}
		Just m	= {UISizeOpts|opts & margins = Just {m & left = left}}

setPadding :: !Int !Int !Int !Int !UIControl -> UIControl
setPadding top right bottom left (UIContainer sOpts lOpts items opts)
	= UIContainer sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left (UIPanel sOpts lOpts items opts)
	= UIPanel sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left (UIWindow sOpts lOpts items opts)
	= UIWindow sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left ctrl = ctrl

setTitle :: !String !UIControl -> UIControl
setTitle title (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & title = Just title}
setTitle title (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & title = Just title}
setTitle title (UIFieldSet sOpts lOpts items opts) = UIFieldSet sOpts lOpts items {UIFieldSetOpts|opts & title = title}
setTitle title ctrl = ctrl

setFramed :: !Bool !UIControl -> UIControl
setFramed frame (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & frame = frame}
setFramed frame (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & frame = frame}
setFramed frame ctrl = ctrl

setIconCls :: !String !UIControl -> UIControl
setIconCls iconCls (UIActionButton sOpts aOpts opts) = UIActionButton sOpts aOpts {UIButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIMenuButton sOpts opts) = UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIIcon sOpts opts) = UIIcon sOpts  {UIIconOpts|opts & iconCls = iconCls}
setIconCls iconCls (UITab sOpts opts) = UITab sOpts  {UITabOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & baseCls = Just iconCls}
setIconCls iconCls ctrl = ctrl

setBaseCls :: !String !UIControl -> UIControl
setBaseCls baseCls (UIContainer sOpts lOpts items opts) = UIContainer sOpts lOpts items {UIContainerOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls ctrl = ctrl

setDirection :: !UIDirection !UIControl -> UIControl
setDirection dir (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & direction = dir} items opts
setDirection dir (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & direction = dir} items opts
setDirection dir (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & direction = dir} items opts
setDirection dir ctrl									= ctrl

setHalign :: !UIHAlign !UIControl -> UIControl
setHalign align (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & halign = align} items opts
setHalign align (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & halign = align} items opts
setHalign align (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & halign = align} items opts
setHalign align ctrl									= ctrl

setValign :: !UIVAlign !UIControl -> UIControl
setValign align (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & valign = align} items opts
setValign align (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & valign = align} items opts
setValign align (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & valign = align} items opts
setValign align ctrl									= ctrl

//Container coercion
toPanel	:: !UIControl -> UIControl
//Panels are left untouched
toPanel ctrl=:(UIPanel _ _ _ _)		= ctrl
//Containers are coerced to panels
toPanel ctrl=:(UIContainer sOpts lOpts items {UIContainerOpts|baseCls,bodyCls})
	= UIPanel sOpts lOpts items {UIPanelOpts|title=Nothing,frame=False,tbar=Nothing,iconCls=Nothing,baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a panel instead
toPanel ctrl = defaultPanel [ctrl]

toContainer :: !UIControl -> UIControl
//Containers are left untouched
toContainer ctrl=:(UIContainer _ _ _ _) = ctrl
//Panels can be coerced to containers
toContainer ctrl=:(UIPanel sOpts lOpts items {UIPanelOpts|baseCls,bodyCls})
	= UIContainer sOpts lOpts items {UIContainerOpts|baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a container instead
toContainer ctrl = defaultContainer [ctrl]
	
//GUI combinators						
hjoin :: ![UIControl] -> UIControl
hjoin items = UIContainer defaultSizeOpts {defaultLayoutOpts & direction = Horizontal, halign = AlignLeft, valign = AlignMiddle} items {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}

vjoin :: ![UIControl] -> UIControl
vjoin items = UIContainer defaultSizeOpts {defaultLayoutOpts & direction = Vertical, halign = AlignLeft, valign = AlignTop} items {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}
						
//Container operations
addItemToUI :: (Maybe Int) UIControl UIControl -> UIControl
addItemToUI mbIndex item ctrl = case ctrl of
	UIContainer sOpts lOpts items opts	= UIContainer sOpts lOpts (add mbIndex item items) opts
	UIPanel sOpts lOpts items opts		= UIPanel sOpts lOpts (add mbIndex item items) opts
	UIWindow sOpts lOpts items opts		= UIWindow sOpts lOpts (add mbIndex item items) opts
	_									= ctrl
where
	add Nothing item items		= items ++ [item]
	add (Just pos) item items	= take pos items ++ [item] ++ drop pos items
	
getItemsOfUI :: UIControl -> [UIControl]
getItemsOfUI (UIContainer _ _ items _)	= items
getItemsOfUI (UIPanel _ _ items _)		= items
getItemsOfUI (UIWindow _ _ items _)	= items
getItemsOfUI ctrl						= [ctrl]
	
setItemsOfUI :: [UIControl] UIControl -> UIControl
setItemsOfUI items (UIContainer sOpts lOpts _ opts)	= UIContainer sOpts lOpts items opts
setItemsOfUI items (UIPanel sOpts lOpts _ opts)		= UIPanel sOpts lOpts items opts
setItemsOfUI items (UIWindow sOpts lOpts _ opts)		= UIWindow sOpts lOpts items opts
setItemsOfUI items ctrl								= ctrl

//Container for a set of horizontally layed out buttons
buttonPanel	:: ![UIControl] -> UIControl	
buttonPanel buttons
	= (/*setBaseCls "x-toolbar" o*/ wrapHeight o fillWidth o setPadding 2 2 2 0 o setDirection Horizontal o setHalign AlignRight) (defaultContainer buttons)

actionsToButtons :: ![UIAction] -> (![UIControl],![UIAction])
actionsToButtons [] = ([],[])
actionsToButtons [a=:{taskId,action,enabled}:as]
	# (buttons,actions)	= actionsToButtons as 
	= case split "/" (actionName action) of
		//Action name consist of only one part -> make a button
		[name]	= ([mkButton taskId action enabled : buttons],actions)
		//Action name is "/" -> also make a button or we get a weird menu
		["",""]	= ([mkButton taskId action enabled : buttons],actions)
		//Action name consists of multiple parts -> pass through
		_		= (buttons,[a:actions])
where
	mkButton taskId action enabled
		= UIActionButton defaultSizeOpts {UIActionOpts|taskId = toString taskId,actionId= actionName action}
			{UIButtonOpts|text = Just (actionName action), iconCls = Just (actionIcon action), disabled = not enabled}
			
actionsToMenus :: ![UIAction] -> (![UIControl],![UIAction])
actionsToMenus actions
	# (menus,actions) = makeMenus [] actions
	= (sortBy menuOrder menus,actions)
where
	makeMenus :: [UIControl] [UIAction] -> ([UIControl],[UIAction])
	makeMenus menus []	= (menus,[])	
	makeMenus menus [a=:{taskId,action,enabled}:as] = makeMenus (addToMenus (split "/" (actionName action)) taskId action enabled menus) as
		
	addToMenus [main:item] taskId action enabled [] //Create menu
		= [createButton main item taskId action enabled]
	addToMenus [main:item] taskId action enabled [m=:(UIMenuButton sOpts opts):ms] //Add to existing menu if it exists
		| opts.UIMenuButtonOpts.text == Just main //Found!
			= [UIMenuButton sOpts {UIMenuButtonOpts|opts & menu = addToItems item taskId action enabled opts.UIMenuButtonOpts.menu}:ms]
		| otherwise
			= [m:addToMenus [main:item] taskId action enabled ms]
			
	addToItems [item:sub] taskId action enabled [] //Create item
		= [createItem item sub taskId action enabled]
	addToItems [item:sub] taskId action enabled [i:is]
		| itemText i == item
			| isEmpty sub	//Duplicate item (just add it)
				= [i,createItem item sub taskId action enabled:is]
			| otherwise		//Add to the found item
				= [addToItem sub taskId action enabled i:is]
		| otherwise
			= [i:addToItems [item:sub] taskId action enabled is]
	addToItems [] _ _ _ _
		= []

	itemText (UIActionMenuItem _ {UIButtonOpts|text})	= fromMaybe "" text
	itemText (UISubMenuItem {UIMenuButtonOpts|text})	= fromMaybe "" text
	itemText _					= ""
	
	createButton item sub taskId action enabled
		= UIMenuButton defaultSizeOpts
			{UIMenuButtonOpts
			|text = Just item
			,iconCls = Just (icon item)
			,disabled	= if (isEmpty sub) (not enabled) False
			,menu = addToItems sub taskId action enabled []
			}
	createItem item [] taskId action enabled //Action item
		= UIActionMenuItem
			{UIActionOpts|taskId=taskId,actionId=actionName action}
			{UIButtonOpts|text=Just item,iconCls = Just (icon item), disabled = not enabled}
	createItem item sub taskId action enabled //Sub item
		= UISubMenuItem
				{ text = Just item
				, iconCls = Just (icon item)
				, disabled = False
				, menu = addToItems sub taskId action enabled []
				}
		
	addToItem sub taskId action enabled item=:(UISubMenuItem opts=:{UIMenuButtonOpts|menu})
		= UISubMenuItem {UIMenuButtonOpts|opts & menu = addToItems sub taskId action enabled menu}
	
	icon name = "icon-" +++ (replaceSubString " " "-" (toLowerCase name))

	menuOrder (UIMenuButton _ {UIMenuButtonOpts|text=Just m1}) (UIMenuButton _ {UIMenuButtonOpts|text=Just m2}) = m1 < m2
	menuOrder m1 m2 = False

uiOf :: UIDef -> UIControl
uiOf def = case uiDefControls def of
	[c:_]	= c
	[]		= stringDisplay "-"

singleControl :: UIDef -> Bool
singleControl  def = case uiDefControls def of
	[_]	= True
	_	= False

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes
mergeAttributes attr1 attr2 = foldr (\(k,v) attr -> put k v attr) attr1 (toList attr2)

appDeep	:: [Int] (UIControl -> UIControl) UIControl -> UIControl
appDeep [] f ctrl = f ctrl
appDeep [s:ss] f ctrl = case ctrl of
	(UIContainer sOpts lOpts items cOpts) 	= UIContainer sOpts lOpts (update items) cOpts
	(UIPanel sOpts lOpts items pOpts)		= UIPanel sOpts lOpts (update items) pOpts
	(UIWindow sOpts lOpts items wOpts)		= UIWindow sOpts lOpts (update items) wOpts
	_										= ctrl
where
	update items = [if (i == s) (appDeep ss f item) item \\ item <- items & i <- [0..]]

tweakUI :: (UIControl -> UIControl) UIDef -> UIDef
tweakUI f (UIControlSequence (attributes,controls,direction))
	= UIControlSequence (attributes,[(f c,a) \\ (c,a) <- controls],direction)
tweakUI f (UIControlGroup (attributes,controls,direction,actions))
	= UIControlGroup (attributes,[(f c,a) \\ (c,a) <- controls],direction,actions)
tweakUI f (UIAbstractContainer (attributes,controls,actions,direction))	= UIAbstractContainer (attributes,map f controls,actions,direction)
tweakUI f (UIFinal (controls,title))							= UIFinal (map f controls,title)
tweakUI f def													= def

tweakAttr :: (UIAttributes -> UIAttributes) UIDef -> UIDef
tweakAttr f (UIControlSequence (attributes,controls,direction))
	= UIControlSequence (f attributes, controls,direction)
tweakAttr f (UIControlGroup (attributes,controls,direction,actions))
	= UIControlGroup (f attributes, controls, direction, actions)
tweakAttr f (UIAbstractContainer (attributes,controls,actions,direction))
	= UIAbstractContainer (f attributes, controls, actions,direction)
tweakAttr f def													= def

tweakControls :: ([(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]) UIDef -> UIDef
tweakControls f (UIControlSequence (attributes,controls,direction))
	= UIControlSequence (attributes,f controls,direction)
tweakControls f (UIControlGroup (attributes,controls,direction,actions))
	= UIControlGroup (attributes,f controls,direction,actions)
tweakControls f (UIAbstractContainer (attributes,controls,actions,direction))
	= UIAbstractContainer (attributes,map fst (f [(c,newMap) \\ c <- controls]),actions,direction)
tweakControls f (UIFinal (controls,title))							= UIFinal (map fst (f [(c,newMap) \\ c <- controls]),title)
tweakControls f def													= def
