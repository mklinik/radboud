implementation module AppState

import ExperimentalDomain
from Configuration import :: IDEConfig

derive class iTask	AppState, SyntaxHighlighterColors
derive gMerge		AppState, SyntaxHighlighterColors
derive bimap		Maybe, (,)

initAppState :: !IDEConfig -> AppState
initAppState config =	{ srcEditorContent	= ""
						, ideConfig			= config
						, syntaxHighlColors =	{ keywords				= colorPurple
												, typeDefinitions		= colorRed
												, singleLineComments	= colorAqua
												, multiLineComments		= colorBlue
												, strings				= colorGreen
												, characters			= colorFuchsia
												, numbers				= colorOrange
												}
						}