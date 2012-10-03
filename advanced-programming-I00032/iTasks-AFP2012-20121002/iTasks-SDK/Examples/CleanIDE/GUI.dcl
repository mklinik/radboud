definition module GUI

import iTasks

:: WizardStep state = ViewOnState !String ![View state] | CustomTask (state WizardAction -> Task (state,WizardAction))
:: WizardAction = GotoNext | GotoPrevious

derive class iTask WizardAction

wizard :: !String !description ![WizardStep state] !state -> Task (Maybe state) | html description & iTask state & SharedVariable state

editOptions :: !description !state !(state -> opts) !(opts state -> state) -> Task state | html description & iTask state & iTask opts