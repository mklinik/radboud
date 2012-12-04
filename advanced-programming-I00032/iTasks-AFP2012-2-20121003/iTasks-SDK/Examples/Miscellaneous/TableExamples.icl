implementation module TableExamples

import iTasks, Text
from StdFunc import seq

tableExamples :: [Workflow]
tableExamples = []

/*tableExamples :: [Workflow]
tableExamples =	[ workflow "Examples/Miscellaneous/Plant dataset table" "Uses the Table type to represent a simple plant dataset." plantExample
				, workflow "Examples/Miscellaneous/Symmetric lens" "This example shows the usage of the symmetric lens combinator." symmetricLensExample
				]

plantExample
	# plantExample` = try plantExample` showFileError
	# plantExample` = try plantExample` showParseError 
	= catchAll plantExample` showUnknownError 

plantExample` =
		readDataset
	>>= transform Table
	>>=	\table. updateInformation ("Plant Dataset",description) [] table <<@ fullWidthInteractionLayout
	>>= showInformation "Updated dataset" []
	>>| stop
where
	description =
		[ Text "This example demonstrates how the table type is used to edit a dataset. "
		, Text "The dataset is taken from an ", ATag [HrefAttr "http://dev.sencha.com/deploy/dev/examples/grid/edit-grid.html"] [Text "Ext JS Grid Example"], Text "."
		]

showFileError (FileException path _)	= showInformation ("Error","Error reading dataset.") [] path >>| stop
showParseError (CannotParse err)		= showInformation ("Error","Error reading dataset.") [] err >>| stop
showUnknownError err					= showInformation "Error" [] err >>| stop

readDataset :: Task [Plant]
readDataset =
					importCSVFile ".\\Miscellaneous\\plants.csv"
	>>= \csvData.	toPlants csvData []
where
	toPlants [] acc = return (reverse acc)
	toPlants [plant:rest] acc = case plant of
		[common,botanical,zone,light,price,availability,indoor]
			= toPlants rest
				[{ name =
					{ common		= common
					, botanical		= botanical
					}
				, light 			= case light of
					"Sunny"			= Sunny
					"Sun or Shade"	= SunOrShade
					"Mostly Shady"	= MostlyShady
					_				= Shade
				, price				= USD (toInt (toReal price) * 100)
				, availability		= s2Date availability
				, indoor			= indoor == "true"
				, notes				= Nothing
				}:acc]
		_
			= throw (CannotParse "invalid CSV row!")

	s2Date str = case split "/" str of
		[m,d,y]	= {day = toInt d, mon = toInt m, year = toInt y}
		_		= {day = 0, mon = 0, year = 0}

:: Plant =		{ name			:: PlantName
				, light			:: PlantLight
				, price			:: Currency
				, availability	:: Date
				, indoor		:: Bool
				, notes			:: Maybe [Note]
				}
:: PlantName =	{ common		:: String
				, botanical		:: String
				}
:: PlantLight	= Sunny
				| SunOrShade
				| MostlyShady
				| Shade

derive class iTask Plant, PlantName, PlantLight
derive bimap Maybe, (,)

symmetricLensExample =
	updateSharedInformation ("Symmetric lens example",description) [UpdateView (GetShared toView,PutbackShared fromView)] (dbx >&< dby) Void >>+ \_ -> UserActions [(ActionQuit,Just Void)]
where
	description =
		[ PTag []
			[ Text "This example demonstrates the usage of a symmetric lens. "
			, Text "It also shows that views on both databases can be combined in one form."
			]
		, PTag []
			[ Text "The example is taken from:"
			, BrTag []
			, Text "Martin Hofmann, Benjamin C. Pierce, and Daniel Wagner. "
			, ATag [HrefAttr "http://www.cis.upenn.edu/~bcpierce/papers/symmetric-full.pdf"] [Text "Symmetric lenses"], Text ". "
			, Text "In ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL), Austin, Texas, January 2011."
			]
		]
		
	(dbx,dby) = symmetricLens putr putl (sharedStore "tableExampleX" initX) (sharedStore "tableExampleY" initY)

	toView				= app2 (Table,Table)
	fromView view _	_	= app2 (fromTable,fromTable) view

initX :: [DBX]
initX =	[	{ DBX
			| composer		= "Schubert"
			, dateOfBirth	= 1797
			, dateOfDeath	= Just 1828
			}
		,	{ DBX
			| composer		= "Shumann"
			, dateOfBirth	= 1810
			, dateOfDeath	= Just 1856
			}
		]
		
initY :: [DBY]
initY =	[	{ DBY
			| composer		= "Schubert"
			, country		= "Austria"
			}
		,	{ DBY
			| composer		= "Shumann"
			, country		= "Germany"
			}
		]

putr :: ![DBX] ![DBY] -> [DBY]
putr dbx dby = map (\({DBX|composer},{DBY|country}) -> {DBY|composer = composer, country = country}) (zip2 dbx dby)
	
putl :: ![DBY] ![DBX] -> [DBX]
putl dby dbx = map (\({DBY|composer},{DBX|dateOfBirth,dateOfDeath}) -> {DBX|composer = composer, dateOfBirth = dateOfBirth, dateOfDeath = dateOfDeath}) (zip2 dby dbx)
	
:: DBX =		{ composer		:: !String
				, dateOfBirth	:: !Year
				, dateOfDeath	:: !Maybe Year
				}
:: DBY =		{ composer		:: !String
				, country		:: !String
				}
				
:: Year :== Int

derive class iTask DBX, DBY

instance < (Maybe a) | < a
where
	(<) (Just x) (Just y)	= x < y
	(<) Nothing (Just _) 	= True
	(<) _ _					= False*/
