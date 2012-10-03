implementation module UIDefinition

import JSON_NG, StdList, StdBool, StdTuple, GenEq_NG, StdFunc, HTML, Text, Map, List_NG
from SystemTypes import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount(..), :: Action
	
defaultSizeOpts	:: UISizeOpts
defaultSizeOpts = {width = Nothing, minWidth = Nothing, height = Nothing, minHeight = Nothing, margins = Nothing}

defaultLayoutOpts :: UILayoutOpts
defaultLayoutOpts = {direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing}

defaultContainer :: ![UIControl] -> UIControl
defaultContainer items = UIContainer defaultSizeOpts defaultLayoutOpts items {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}

defaultPanel :: ![UIControl] -> UIControl
defaultPanel items = UIPanel defaultSizeOpts defaultLayoutOpts items {UIPanelOpts|title=Nothing,frame=False,tbar=Nothing,iconCls=Nothing,baseCls=Nothing,bodyCls=Nothing}

defaultWindow :: ![UIControl] -> UIControl
defaultWindow items = UIWindow defaultSizeOpts defaultLayoutOpts items {UIWindowOpts|title=Nothing,frame=False,tbar=Nothing,iconCls=Nothing,baseCls=Nothing,bodyCls=Nothing}

stringDisplay :: !String -> UIControl
stringDisplay value = UIViewString defaultSizeOpts {UIViewOpts|value = Just value}

uiDefAttributes	:: UIDef -> UIAttributes
uiDefAttributes (UIControlSequence (attributes,_,_))		= attributes
uiDefAttributes (UIControlGroup (attributes,_,_,_)) 		= attributes
uiDefAttributes (UIAbstractContainer (attributes,_,_,_))	= attributes
uiDefAttributes _											= newMap

uiDefControls :: UIDef -> [UIControl]
uiDefControls (UIControlSequence (_,controls,_))		= map fst controls
uiDefControls (UIControlGroup (_,controls,_,_))			= map fst controls
uiDefControls (UIAbstractContainer (_,controls,_,_))	= controls
uiDefControls (UIFinal (controls,_))					= controls
uiDefControls _											= []

uiDefAnnotatedControls :: UIDef -> [(UIControl,UIAttributes)]
uiDefAnnotatedControls (UIControlSequence (_,controls,_))		= controls
uiDefAnnotatedControls (UIControlGroup (_,controls,_,_))		= controls
uiDefAnnotatedControls (UIAbstractContainer (_,controls,_,_))	= [(c,newMap)\\c <- controls]
uiDefAnnotatedControls (UIFinal (controls,_))					= [(c,newMap)\\c <- controls]
uiDefAnnotatedControls _										= []

uiDefActions :: UIDef -> [UIAction]
uiDefActions (UIActionSet actions)					= actions
uiDefActions (UIControlGroup (_,_,_,actions)) 		= actions
uiDefActions (UIAbstractContainer (_,_,_,actions))	= actions
uiDefActions _										= []

uiDefDirection :: UIDef -> UIDirection
uiDefDirection (UIAbstractContainer (_,_,direction,_))	= direction
uiDefDirection _										= Vertical

uiDefSetAttribute :: String String UIDef -> UIDef
uiDefSetAttribute key value (UIControlSequence (attributes,controls,direction))
	= UIControlSequence (put key value attributes,controls,direction)
uiDefSetAttribute key value (UIControlGroup (attributes,controls,direction,actions))
	= UIControlGroup (put key value attributes,controls,direction,actions)
uiDefSetAttribute key value (UIAbstractContainer (attributes,controls,direction,actions))
	= UIAbstractContainer (put key value attributes,controls,direction,actions)
uiDefSetAttribute key value def = def

uiDefSetDirection :: UIDirection UIDef -> UIDef
uiDefSetDirection direction (UIControlSequence (attributes,controls,_))
	= (UIControlSequence (attributes,controls,direction))
uiDefSetDirection direction (UIControlGroup (attributes,controls,_,actions))
	= (UIControlGroup (attributes,controls,direction,actions))
uiDefSetDirection direction (UIAbstractContainer (attributes,controls,_,actions))
	= (UIAbstractContainer (attributes,controls,direction,actions))
uiDefSetDirection direction def = def

encodeUIDefinition :: !UIDef -> JSONNode
encodeUIDefinition def = JSONArray (map encodeUIControl (uiDefControls def))

encodeUIControl :: !UIControl -> JSONNode
encodeUIControl (UIViewString sopts vopts)				= enc "itwc_view_string" [toJSON sopts,toJSON vopts] []
encodeUIControl (UIViewHtml sopts vopts)				= enc "itwc_view_html" [toJSON sopts, encHtml vopts] []
encodeUIControl (UIViewDocument sopts vopts)			= enc "itwc_view_document" [toJSON sopts, toJSON vopts] []
encodeUIControl (UIViewCheckbox sopts vopts)			= enc "itwc_view_checkbox" [toJSON sopts, toJSON vopts] []
encodeUIControl (UIViewSlider sopts vopts opts)			= enc "itwc_view_slider" [toJSON sopts, toJSON vopts, toJSON opts] []
encodeUIControl (UIViewProgress sopts vopts opts)		= enc "itwc_view_progress" [toJSON sopts, toJSON vopts, toJSON opts] []
encodeUIControl (UIEditString sopts eopts)				= enc "itwc_edit_string" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditNote sopts eopts)				= enc "itwc_edit_note" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditPassword sopts eopts)			= enc "itwc_edit_password" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditInt sopts eopts)					= enc "itwc_edit_int" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditDecimal sopts eopts)				= enc "itwc_edit_decimal" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditCheckbox sopts eopts)			= enc "itwc_edit_checkbox" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditSlider sopts eopts opts)			= enc "itwc_edit_slider" [toJSON sopts, toJSON eopts, toJSON opts] []
encodeUIControl (UIEditDate sopts eopts)				= enc "itwc_edit_date" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditTime sopts eopts)				= enc "itwc_edit_time" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditDocument sopts eopts)			= enc "itwc_edit_document" [toJSON sopts, toJSON eopts] []
encodeUIControl (UIEditGoogleMap sopts eopts opts)		= enc "itwc_edit_googlemap" [toJSON sopts, toJSON eopts, toJSON opts] []
encodeUIControl (UIEditButton sopts eopts opts)			= enc "itwc_editbutton" [toJSON sopts, toJSON eopts, toJSON opts] []
encodeUIControl (UIDropdown sopts copts)				= enc "itwc_choice_dropdown" [toJSON sopts, toJSON copts] []
encodeUIControl (UIGrid sopts copts opts)				= enc "itwc_choice_grid" [toJSON sopts, toJSON copts, toJSON opts] []
encodeUIControl (UITree sopts copts)					= enc "itwc_choice_tree" [toJSON sopts, toJSON copts] []
encodeUIControl (UIActionButton sopts aopts opts)		= enc "itwc_actionbutton" [toJSON sopts, toJSON aopts, toJSON opts] []
encodeUIControl (UIMenuButton sopts opts)				= enc "itwc_menubutton" [toJSON sopts, toJSON opts] []
encodeUIControl (UILabel sopts opts)					= enc "itwc_label" [toJSON sopts, toJSON opts] []
encodeUIControl (UIIcon sopts opts)						= enc "itwc_icon" [toJSON sopts, toJSON opts] []
encodeUIControl (UITab sopts opts)						= enc "itwc_tab" [toJSON sopts, toJSON opts] []
encodeUIControl (UITasklet sopts opts)					= enc "itwc_tasklet" [toJSON sopts, toJSON opts] []
encodeUIControl (UIContainer sopts lopts items opts)	= enc "itwc_container" [toJSON sopts, toJSON lopts, toJSON opts] items
encodeUIControl (UIPanel sopts lopts items opts)		= enc "itwc_panel" [toJSON sopts, toJSON lopts, toJSON opts] items
encodeUIControl (UIFieldSet sopts lopts items opts)		= enc "itwc_fieldset" [toJSON sopts, toJSON lopts, toJSON opts] items
encodeUIControl (UIWindow sopts lopts items opts)		= enc "itwc_window" [toJSON sopts, toJSON lopts, toJSON opts] items
encodeUIControl (UICustom json)							= json

derive JSONEncode UISizeOpts, UIViewOpts, UIEditOpts, UIChoiceOpts, UIActionOpts, UILayoutOpts
derive JSONEncode UISliderOpts, UIProgressOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UIGridOpts, UIButtonOpts, UITreeNode, UILabelOpts
derive JSONEncode UIIconOpts, UITabOpts, UITaskletOpts
derive JSONEncode UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts

JSONEncode{|UISideSizes|} {top,right,bottom,left}
	= [JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)]

JSONEncode{|UISize|} (ExactSize s)		= [JSONInt s]
JSONEncode{|UISize|} WrapSize			= [JSONString "wrap"] 
JSONEncode{|UISize|} FlexSize			= [JSONString "flex"] 

JSONEncode{|UIMinSize|} (ExactMin s)	= [JSONInt s]
JSONEncode{|UIMinSize|} WrapMin			= [JSONString "wrap"]

JSONEncode{|UIVAlign|} AlignTop			= [JSONString "top"]
JSONEncode{|UIVAlign|} AlignMiddle		= [JSONString "middle"]
JSONEncode{|UIVAlign|} AlignBottom		= [JSONString "bottom"]

JSONEncode{|UIHAlign|} AlignLeft		= [JSONString "left"]
JSONEncode{|UIHAlign|} AlignCenter		= [JSONString "center"]
JSONEncode{|UIHAlign|} AlignRight		= [JSONString "right"]

JSONEncode{|UIDirection|} Vertical		= [JSONString "vertical"]
JSONEncode{|UIDirection|} Horizontal	= [JSONString "horizontal"]

JSONEncode{|UIMenuButtonOpts|} {UIMenuButtonOpts|text,iconCls,disabled,menu}
	= [JSONObject (text` ++ [("disabled",JSONBool disabled),("menu",menu`)] ++ iconCls`)]
where
	text`		= maybe [] (\s -> [("text",JSONString s)]) text
	iconCls`	= maybe [] (\s -> [("iconCls",JSONString s)]) iconCls
	menu`= JSONObject [("xtype",JSONString "itwc_menu"),("items",JSONArray (map toJSON menu))]

JSONEncode{|UIMenuItem|} (UIActionMenuItem aopts opts)	= [enc "itwc_actionmenuitem" [toJSON aopts,toJSON opts] []]
JSONEncode{|UIMenuItem|} (UISubMenuItem opts) 			= [enc "itwc_submenuitem" [toJSON opts] []]

JSONEncode{|UIControl|} control = [encodeUIControl control]
JSONEncode{|ProgressAmount|} ProgressUndetermined		= [JSONString "undetermined"]
JSONEncode{|ProgressAmount|} (ProgressRatio ratio)		= [JSONReal ratio]

enc :: String [JSONNode] [UIControl] -> JSONNode
enc xtype opts items = JSONObject [("xtype",JSONString xtype):optsfields ++ itemsfield]
where
	optsfields = flatten [fields \\ JSONObject fields <- opts]
	itemsfield = case items of
		[]	= []
		_	= [("items",JSONArray (map encodeUIControl items))]

//Special cases
encHtml :: (UIViewOpts HtmlTag) -> JSONNode
encHtml {UIViewOpts|value=Just html} = JSONObject [("value",JSONString (toString html))]
encHtml {UIViewOpts|value=Nothing} = JSONObject []

class encodeUIValue a :: a -> JSONNode
instance encodeUIValue String			where encodeUIValue v = JSONString v
instance encodeUIValue Int				where encodeUIValue v = JSONInt v
instance encodeUIValue Real				where encodeUIValue v = JSONReal v
instance encodeUIValue Bool				where encodeUIValue v = JSONBool v
instance encodeUIValue Document			where encodeUIValue v = toJSON v
instance encodeUIValue Date				where encodeUIValue v = toJSON v
instance encodeUIValue Time				where encodeUIValue v = toJSON v
instance encodeUIValue HtmlTag			where encodeUIValue v = JSONString (toString v)
instance encodeUIValue ProgressAmount	where encodeUIValue v = toJSON v
instance encodeUIValue JSONNode			where encodeUIValue v = toJSON v
instance encodeUIValue (Maybe a) | encodeUIValue a
where
	encodeUIValue Nothing = JSONNull
	encodeUIValue (Just a) = encodeUIValue a
