definition module Configuration

import iTasks, AppState

:: IDEConfig =	{ oldIDEPath	:: !Path
				, projectsPath	:: !Path
				}
				
derive class iTask			IDEConfig
derive class SharedVariable	IDEConfig
				
loadConfig :: Task (Maybe IDEConfig)
getConfig :: !(DBId AppState) -> Task IDEConfig