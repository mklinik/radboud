definition module AppState

import iTasks, ExperimentalDomain
from Configuration import :: IDEConfig

:: AppState =	{ srcEditorContent	:: !String
				, ideConfig			:: !IDEConfig
				, syntaxHighlColors	:: !SyntaxHighlighterColors
				}
				
:: SyntaxHighlighterColors =	{ keywords				:: !Color
								, typeDefinitions		:: !Color
								, singleLineComments	:: !Color
								, multiLineComments		:: !Color
								, strings				:: !Color
								, characters			:: !Color
								, numbers				:: !Color
								}
				
derive class iTask	AppState, SyntaxHighlighterColors
derive gMerge		AppState

initAppState :: !IDEConfig -> AppState