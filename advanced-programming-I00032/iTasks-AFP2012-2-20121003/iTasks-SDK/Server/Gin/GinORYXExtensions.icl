implementation module GinORYXExtensions

import Maybe
import JSON

derive JSONEncode ORYXExtensionsFile, ORYXPerspective
derive JSONDecode ORYXExtensionsFile, ORYXPerspective

derive bimap Maybe, (,)

JSONEncode{|ORYXExtension|} {title, namespace, description, definition_, extends} = 
	[JSONObject [ ("title", toJSON title)
				, ("namespace", toJSON namespace)
				, ("description", toJSON description)
				, ("definition", toJSON definition_)
				, ("extends", toJSON extends)
				]
	]

JSONDecode{|ORYXExtension|} [node:nodes]
	# mTitle		= jsonQuery "title"			node
	# mNamespace	= jsonQuery "namespace"		node
	# mDescription	= jsonQuery "description"	node
	# mDefinition	= jsonQuery "definition"	node
	# mExtends		= jsonQuery "extends"		node
	| isNothing mTitle			= (Nothing, nodes)
	| isNothing mNamespace		= (Nothing, nodes)
	| isNothing mDescription	= (Nothing, nodes)
	| isNothing mDefinition		= (Nothing, nodes)
	| isNothing mExtends		= (Nothing, nodes)
	= (Just { ORYXExtension 
			| title 		= fromJust mTitle
			, namespace		= fromJust mNamespace
			, description	= fromJust mDescription
			, definition_	= fromJust mDefinition
			, extends		= fromJust mExtends
			}
	  , nodes)

makeORYXExtensionsFile :: ![String] -> ORYXExtensionsFile
makeORYXExtensionsFile names = 
	{ ORYXExtensionsFile 
	| extensions = 
		[	{ ORYXExtension
			| title = "Graphical iTask Notation - " +++ name +++ " extension"
			, namespace = "http://mbsd.icis.ru.nl/itasks/gin/" +++ name +++ "#"
			, description = "Graphical iTask Notation - " +++ name +++ " extension"
			, definition_ = "/services/json/stencils/gin/" +++ name
			, extends = "http://mbsd.icis.ru.nl/itasks/gin#"
			}
			\\ name <- names
		]
	, perspectives = []
	}
