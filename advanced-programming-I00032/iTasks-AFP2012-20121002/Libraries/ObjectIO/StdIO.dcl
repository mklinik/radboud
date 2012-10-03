definition module StdIO

//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdIO contains all definition modules of the Object I/O library.
//	********************************************************************************

import
	StdId,					// The operations that generate identification values
	StdIOBasic,				// Function and type definitions used in the library
	StdIOCommon,			// Function and type definitions used in the library
	StdKey,					// Function and type definitions on keyboard
	StdMaybe,				// The Maybe data type
	StdPSt,					// Operations on PSt that are not device related
	StdPStClass,			// PSt/IOSt instances of common classes
	StdSystem,				// System dependent operations
	
	StdFileSelect,			// File selector dialogues
	
	StdPictureDef,			// Type definitions for picture handling
	StdPicture,				// Picture handling operations
	StdBitmap,				// Defines an instance for drawing bitmaps
	
	StdProcessDef,			// Type definitions for process handling
	StdProcessAttribute,	// ProcessAttribute access operations
	StdProcess,				// Process handling operations
	
	StdClipboard,			// Clipboard handling operations
	
	StdPrint,				// General printing functions
	StdPrintText,			// Specialised text printing functions
	
	StdControlDef,			// Type definitions for controls
	StdControlAttribute,	// ControlAttribute access operations
	StdControlClass,		// Standard controls class instances
	StdControlReceiver,		// Receiver controls class instances
	StdControl,				// Control handling operations
	
	StdMenuDef,				// Type definitions for menus
	StdMenuAttribute,		// MenuAttribute access operations
	StdMenuElementClass,	// Standard menus class instances
	StdMenuReceiver,		// Receiver menus class instances
	StdMenuElement,			// Menu element handling operations
	StdMenu,				// Menu handling operations
	
	StdReceiverDef,			// Type definitions for receivers
	StdReceiverAttribute,	// ReceiverAttribute access operations
	StdReceiver,			// Receiver handling operations
	
	StdTimerDef,			// Type definitions for timers
	StdTimerAttribute,		// TimerAttribute access operations
	StdTimerElementClass,	// Standard timer class instances
	StdTimerReceiver,		// Receiver timer class instances
	StdTimer,				// Timer handling operations
	StdTime,				// Time related operations
	
	StdWindowDef,			// Type definitions for windows
	StdWindowAttribute,		// WindowAttribute access operations
	StdWindow				// Window handling operations
