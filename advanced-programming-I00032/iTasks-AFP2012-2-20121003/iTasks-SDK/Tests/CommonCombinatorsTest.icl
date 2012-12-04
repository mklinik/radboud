module CommonCombinatorsTest
/**
* This module tests the combinators in CommonCombinators.dcl
*/
import StdEnv, iTasks, iData
import CommonCombinators, PromptingCombinators

Start :: *World -> *World
Start world = startEngine tests world
where
	tests = [ 	{ name		= "chooseTask_btn_test"
		  		, label		= "chooseTask_btn"
		  		, roles		= []
		  		, mainTask	= chooseTask_btn_test
		  		}
		  	,	{ name		= "chooseTask_pdm_test"
		  		, label		= "chooseTask_pdm"
		  		, roles		= []
		  		, mainTask	= chooseTask_pdm_test
		  		}
		  	,	{ name		= "chooseTask_cbox_test"
		  		, label		= "chooseTask_cbox"
		  		, roles		= []
		  		, mainTask	= chooseTask_cbox_test
		  		}
		  	]

chooseTask_btn_test :: Task Void
chooseTask_btn_test
	= chooseTask_btn [Text "Please choose a task"] taskChoices

chooseTask_pdm_test :: Task Void
chooseTask_pdm_test
	= chooseTask_pdm  [Text "Please select a task"] 0 taskChoices

chooseTask_cbox_test :: Task Void
chooseTask_cbox_test
	= chooseTask_cbox seqTasks [Text "Please choose some tasks"] richTaskChoices
	#>> return_V Void


taskChoices = [ ("A", [Text "you chose A"] ?>> editTask "Ok" Void)
			  , ("B", [Text "you chose B"] ?>> editTask "Ok" Void)
			  , ("C", [Text "you chose C"] ?>> editTask "Ok" Void)
			  ]

richTaskChoices = [((False,(\_ cur -> cur),[Text label]),(label,task)) \\ (label,task) <- taskChoices]
