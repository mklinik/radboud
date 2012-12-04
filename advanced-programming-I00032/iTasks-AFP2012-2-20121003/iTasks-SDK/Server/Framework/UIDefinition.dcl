definition module UIDefinition
/**
* This module provides an abstract representation of user interfaces.
* This representation seeks a middle ground between being fine grained enough
* to describe rich user interfaces and being leaving rendering details to the client framework.
*/
import JSON_NG, GenEq_NG
from SystemTypes	import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount, :: Action
from Task			import :: TaskId
from HTML			import :: HtmlTag
from Map			import :: Map(..)

/**
* Rendering a user interface for a composition of is a staged process in which
* the raw UI material provided by basic tasks is grouped by layout policies to reach
* a final UI definition consisting of a set of controls and a window title for the top-level application window.
*
* The UIDef type has contstructors for the various types of partial UI definitions.
*/

:: UIDef
	= UIControlSequence 	!UIControlSequence										//Components from an interact task
	| UIActionSet			!UIActions												//Actions from a chooseAction task
	| UIControlGroup		!UIControlGroup											//Components from a single or multiple interacts grouped by a shared step combinator
	| UIAbstractContainer	!UIAbstractContainer									//A decorated, layed out set of controls that can be put in a container 
	| UIFinal				!UIFinal												//The final set of controls to put in the main viewport

:: UIControlSequence 	:== (!UIAttributes, !UIAnnotatedControls, !UIDirection)
:: UIControlGroup		:== (!UIAttributes, !UIAnnotatedControls, !UIDirection, !UIActions)
:: UIAbstractContainer	:== (!UIAttributes, !UIControls, !UIDirection, !UIActions)
:: UIFinal				:== (!UIControls, !Maybe UITitle)

:: UIAttributes 		:== Map String String
:: UIControls			:== [UIControl]
:: UIAnnotatedControls	:== [(!UIControl,!UIAttributes)]
:: UIActions			:== [UIAction]
:: UITitle				:== String

:: UIAction	=
	{ taskId	:: !String
	, action	:: !Action
	, enabled	:: !Bool
	}

:: UIControl
	// Components for viewing data:
	= UIViewString		!UISizeOpts	!(UIViewOpts String)							// - String (non-wrapping single line text with automatic escaping)
	| UIViewHtml		!UISizeOpts	!(UIViewOpts HtmlTag)							// - Html (formatted multi line text)
	| UIViewDocument	!UISizeOpts	!(UIViewOpts Document)							// - Document (info + download link)
	| UIViewCheckbox	!UISizeOpts	!(UIViewOpts Bool)								// - Checkbox (non-editable tick-mark)
	| UIViewSlider		!UISizeOpts	!(UIViewOpts Int)	!UISliderOpts				// - Slider (non-editable slider)
	| UIViewProgress	!UISizeOpts	!(UIViewOpts ProgressAmount) !UIProgressOpts	// - Progress (non editable progress bar)
	// Components for editing data:
	| UIEditString		!UISizeOpts	!(UIEditOpts String)						// - String (single line text field)
	| UIEditNote		!UISizeOpts	!(UIEditOpts String)						// - Note (multi-line text field)
	| UIEditPassword	!UISizeOpts	!(UIEditOpts String)						// - Password (single line text field that hides the text)
	| UIEditInt			!UISizeOpts	!(UIEditOpts Int)							// - Int (integer number field)
	| UIEditDecimal		!UISizeOpts	!(UIEditOpts Real)							// - Decimal (decimal number field)
	| UIEditCheckbox	!UISizeOpts	!(UIEditOpts Bool)							// - Checkbox (editable checkbox)
	| UIEditSlider		!UISizeOpts	!(UIEditOpts Int) !UISliderOpts				// - Slider (editable slider)
	| UIEditDate		!UISizeOpts	!(UIEditOpts Date)							// - Date (date picker)
	| UIEditTime		!UISizeOpts	!(UIEditOpts Time)							// - Time (time picker)
	| UIEditDocument	!UISizeOpts	!(UIEditOpts Document)						// - Document (info + upload possibility)
	| UIEditButton		!UISizeOpts !(UIEditOpts JSONNode) !UIButtonOpts		// - Button that sends edit events on click
	| UIEditGoogleMap	!UISizeOpts !(UIEditOpts JSONNode) !UIGoogleMapOpts		// - Google Map panel
	// Components for indicating choices:
	| UIDropdown		!UISizeOpts	!(UIChoiceOpts String)						// - Dropdown (choice from a list of alternatives)
	| UIGrid			!UISizeOpts	!(UIChoiceOpts [String]) !UIGridOpts		// - Grid (selecting an item in a table)
	| UITree			!UISizeOpts	!(UIChoiceOpts UITreeNode) 					// - Tree (selecting a node in a tree structure)
	// Components for triggering actions:
	| UIActionButton	!UISizeOpts	!UIActionOpts !UIButtonOpts					// - Action Button (clicks trigger action events)
	| UIMenuButton		!UISizeOpts	!UIMenuButtonOpts							// - Menu Button (clicks open a menu)
	// Misc auxiliary components:
	| UILabel			!UISizeOpts	!UILabelOpts								// - Label (non-wrapping text label, clicks focus next component)
	| UIIcon			!UISizeOpts	!UIIconOpts									// - Icon (information icon with tooltip text)
	| UITab				!UISizeOpts	!UITabOpts									// - Tab (clicks trigger focus events)
	| UITasklet			!UISizeOpts !UITaskletOpts								// - Tasklet (custom clientside interaction)
	// Container components for composition:
	| UIContainer		!UISizeOpts !UILayoutOpts ![UIControl] !UIContainerOpts	// - Container (lightweight wrapper to compose components)
	| UIPanel			!UISizeOpts !UILayoutOpts ![UIControl] !UIPanelOpts		// - Panel (container with decoration like a title header, icon and frame)
	| UIFieldSet		!UISizeOpts !UILayoutOpts ![UIControl] !UIFieldSetOpts	// - Fieldset (wrapper with a simple border and title)
	| UIWindow			!UISizeOpts !UILayoutOpts ![UIControl] !UIWindowOpts	// - Window (floating window TODO)
	// DEPRECATED: custom xtjs definition:
	| UICustom			!JSONNode
	
:: UISizeOpts =
	{ width		:: !Maybe UISize
	, minWidth	:: !Maybe UIMinSize
	, height	:: !Maybe UISize
	, minHeight	:: !Maybe UIMinSize
	, margins	:: !Maybe UISideSizes
	}

:: UISize
	= ExactSize !Int
	| WrapSize
	| FlexSize

:: UIMinSize
	= ExactMin !Int
	| WrapMin
	
:: UILayoutOpts =
	{ direction	:: !UIDirection
	, halign	:: !UIHAlign
	, valign	:: !UIVAlign
	, padding	:: !Maybe UISideSizes
	}

:: UIHAlign
	= AlignLeft
	| AlignCenter
	| AlignRight

:: UIVAlign
	= AlignTop
	| AlignMiddle
	| AlignBottom
	
:: UIDirection
	= Horizontal
	| Vertical
	
:: UISide
	= TopSide
	| RightSide
	| BottomSide
	| LeftSide

:: UISideSizes =
	{ top		:: !Int
	, right		:: !Int
	, bottom	:: !Int
	, left		:: !Int
	}
	
:: UIViewOpts a =
	{ value			:: !Maybe a
	}
	
:: UIEditOpts a =
	{ taskId		:: !String
	, editorId		:: !String
	, value			:: !Maybe a
	}

:: UIActionOpts =
	{ taskId		:: !String
	, actionId		:: !String
	}

:: UIChoiceOpts a =
	{ taskId		:: !String
	, editorId		:: !String
	, value			:: !Maybe Int
	, options		:: ![a]
	}
		
:: UISliderOpts =
	{ minValue		:: !Int
	, maxValue		:: !Int
	}
	
:: UIProgressOpts = 
	{ text			:: !String
	}
	
:: UIGoogleMapOpts = 
	{ center 			:: !(!Real,!Real)
	, mapType			:: !String
	, markers			:: ![UIGoogleMapMarker]	
	, options			:: !UIGoogleMapOptions
	}

:: UIGoogleMapMarker =
	{ position				:: !(!Real,!Real)
	, title					:: !Maybe String
	, icon					:: !Maybe String
	, infoWindow			:: !Maybe String
	, draggable				:: !Bool
	, selected				:: !Bool
	}
		
:: UIGoogleMapOptions =
	{ mapTypeControl 	:: !Bool
	, panControl		:: !Bool
	, streetViewControl	:: !Bool
	, zoomControl		:: !Bool
	, scaleControl		:: !Bool
	, scrollwheel		:: !Bool
	, draggable			:: !Bool
	, zoom				:: !Int
	}

:: UIGridOpts =
	{ columns		:: ![String]
	}

:: UITreeNode =
	{ text		:: !String
	, children	:: !Maybe [UITreeNode]
	, leaf		:: !Bool
	, expanded	:: !Bool
	, value		:: !Int
	}

:: UIButtonOpts =
	{ text		:: !Maybe String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	}

:: UIMenuButtonOpts =
	{ text		:: !Maybe String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	, menu		:: ![UIMenuItem]
	}

:: UIMenuItem
	= UIActionMenuItem	!UIActionOpts	!UIButtonOpts		// - Action Menu Item (clicks trigger action events)
	| UISubMenuItem						!UIMenuButtonOpts	// - Sub Menu Item (clicks open a submenu)
		
:: UILabelOpts =
	{ text			:: !String
	}
	
:: UIIconOpts = 
	{ iconCls		:: !String
	, tooltip		:: !Maybe String
	}

:: UITabOpts =
	{ text			:: !String
	, active		:: !Bool
	, focusTaskId	:: !Maybe String
	, closeTaskId	:: !Maybe String
	, iconCls		:: !Maybe String
	}

:: UITaskletOpts = 
	{ taskId		 :: !String
	// It contains html _or_ tui
	, html 			 :: !Maybe String
	, ui			 :: !Maybe UIControl
	, st			 :: !Maybe String
	, script		 :: !Maybe String
	, events		 :: !Maybe [(!String,!String,!String)]	// HTML id, event name, handler function
	, resultFunc     :: !Maybe String
	// They are a pair: the controller hijacks all the events sent to the given instance
	, instanceNo	 :: !Maybe String
	, controllerFunc :: !Maybe String
	}

:: UIContainerOpts =
	{ baseCls		:: !Maybe String
	, bodyCls		:: !Maybe String
	}

:: UIPanelOpts =
	{ title			:: !Maybe String
	, frame			:: !Bool
	, tbar			:: !Maybe [UIControl]
	, iconCls		:: !Maybe String
	, baseCls		:: !Maybe String
	, bodyCls		:: !Maybe String
	}

:: UIFieldSetOpts =
	{ title			:: !String
	}
	
:: UIWindowOpts =
	{ title			:: !Maybe String
	, frame			:: !Bool
	, tbar			:: !Maybe [UIControl]
	, iconCls		:: !Maybe String
	, baseCls		:: !Maybe String
	, bodyCls		:: !Maybe String
	}

//Utility functions
defaultSizeOpts			:: UISizeOpts
defaultLayoutOpts		:: UILayoutOpts

defaultContainer		:: ![UIControl]	-> UIControl
defaultPanel			:: ![UIControl]	-> UIControl
defaultWindow			:: ![UIControl]	-> UIControl
stringDisplay			:: !String		-> UIControl

//Success guaranteed access to the possible parts of a ui definition
uiDefAttributes			:: UIDef -> UIAttributes
uiDefControls			:: UIDef -> [UIControl]
uiDefAnnotatedControls	:: UIDef -> [(UIControl,UIAttributes)]
uiDefActions			:: UIDef -> [UIAction]
uiDefDirection			:: UIDef -> UIDirection

uiDefSetAttribute		:: String String UIDef -> UIDef
uiDefSetDirection		:: UIDirection UIDef -> UIDef
//Encode a user interface definition to a format that
//can be interpreted by the client framework
encodeUIDefinition		:: !UIDef -> JSONNode
encodeUIControl			:: !UIControl -> JSONNode

//Encoding of values for use in UI diffs
class encodeUIValue a :: a -> JSONNode
instance encodeUIValue String
instance encodeUIValue Int
instance encodeUIValue Real
instance encodeUIValue Bool
instance encodeUIValue Document
instance encodeUIValue Date
instance encodeUIValue Time
instance encodeUIValue HtmlTag
instance encodeUIValue ProgressAmount
instance encodeUIValue JSONNode
instance encodeUIValue (Maybe a) | encodeUIValue a
