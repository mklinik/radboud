definition module UIDiff

import UIDefinition
from Task import :: Event

:: UIUpdate
	//Leaf updates
	= UISetValue		!UIPath !JSONNode		// Set the value of a component
	| UISetTaskId		!UIPath !String			// Set taskId a component belongs to
	| UISetEditorId		!UIPath !String			// Set taskId a component belongs to
	| UISetName			!UIPath !String			// Set name of a component
	| UISetEnabled		!UIPath !Bool			// Enable/disable form elements
	| UISetActive		!UIPath !Bool			// Make a tab active/inactive
	| UISetTitle		!UIPath !String			// Set title of a container
	| UIUpdate			!UIPath !UIControl		// Let a component update itself with a new UI definition (for custom components)
	//Structure edits
	| UIAdd				!UIPath !Int !UIControl	//Add child element at index
	| UIRemove			!UIPath !Int			//Remove child element at index
	| UIReplace			!UIPath !Int !UIControl	//Replace child element at index
	//Changing size
	| UIResize			!UIPath !UISizeOpts
	
	
:: UIPath	:== String

diffUIDefinitions :: !UIDef !UIDef !Event -> [UIUpdate]	

encodeUIUpdates :: ![UIUpdate] -> JSONNode