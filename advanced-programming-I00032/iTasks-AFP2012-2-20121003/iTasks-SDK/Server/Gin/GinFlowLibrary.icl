implementation module GinFlowLibrary

import StdEnum
from StdFunc import o
import StdTuple

import GinAbstractSyntax
import GinSyntax
import GinParser
import GinSVG
import GinTypes

predefinedModule :: GModule
predefinedModule = 
	{ GModule
	| name = "(Clean language)"
	, imports = 
		[ "StdEnv"
		]
	, types =	
		[ { name = "Bool"   , rhs = GAbstractTypeRhs }
		, { name = "Char"   , rhs = GAbstractTypeRhs }
		, { name = "Int"    , rhs = GAbstractTypeRhs }
		, { name = "Real"   , rhs = GAbstractTypeRhs }
		, { name = "String" , rhs = GAbstractTypeRhs }
		, { name = "Task"   , rhs = GAbstractTypeRhs }
		, { name = "Void"   , rhs = GAbstractTypeRhs }				  
		]
	, moduleKind = GCleanModule
		[ bStartStop, bCase, bMerge, bLet, bListComprehension]
	}
	
bStartStop :: Binding
bStartStop = ParallelBinding
	{ split = { GDeclaration 
	          | name				= "start"
	          , title				= Just "Start"
	          , description			= Just "Unique starting point of a workflow diagram"
	          , returnType			= GUndefinedTypeExpression
	          , returnDescription	= Nothing
	          , formalParams 		= []
	          , icon 				= Just "start"
	          , shape 				= GBuiltInShape startShape
	          }
	, merge = { GDeclaration 
	          | name				= "stop"
	          , title				= Just "Stop"
	          , description 		= Just "Unique endpoint of a workflow diagram"
	          , returnType 			= GUndefinedTypeExpression
	          , returnDescription	= Nothing
	          , formalParams		= []
	          , icon				= Just "stop"
	          , shape				= GBuiltInShape stopShape
	          }
	, type = GTypeVariable "a"
	, fixedNrBranches = Just 1
	, parameterMap = Extension (PBBranch 0)
	}
where
	startShape :: SVGShape
	startShape = 
	  	{ SVGShape
		| width = 20
		, height = 20
		, defs = []
		, magnets = True
		, elements = 
			[ SVGPolygon Nothing [ (XLeft, YTop), (XPct 66, YPct 50), (XLeft, YBottom)] []
			]
		}
		
	stopShape :: SVGShape
	stopShape = 
	  	{ SVGShape
		| width			= 20
		, height		= 20
		, defs			= []
		, magnets		= True
		, elements		= 
			[ SVGRect Nothing ((XLeft, YTop), (XRight, YBottom)) 0 0 []
			]
		}

bCase :: Binding
bCase = NodeBinding
	{ NodeBinding
	| declaration = 
		{ GDeclaration 
		| name				= "case split"
		, title				= Just "Choice"
		, description		= Just "Makes an exclusive choice between different branches"
		, returnType		= GUndefinedTypeExpression
		, returnDescription	= Nothing
		, formalParams		= 
			[	{ GFormalParameter 
		     	| name			= "a"
				, title			= Nothing
				, description	= Nothing
				, type			= GTypeVariable "a"
				, defaultValue	= Nothing
				, visible		= True
				}
			]
		, icon 				= Just "case-split"
		, shape 			= GBuiltInShape caseSplitShape
		}
	, parameterMap = NBBuiltIn
	}
where
	caseSplitShape :: SVGShape
	caseSplitShape = 
	  	{ SVGShape
		| width		= 120
		, height	= 60
		, defs		= []
		, magnets	= True
		, elements	= 
			[ SVGPolygon Nothing 
				[ (XPct 50, YTop), (XRight, YPct 50), (XPct 50, YBottom), (XLeft, YPct 50)] [SVGAnchors "top left right bottom", SVGFill "white", SVGResize "horizontal vertical"]
			, SVGText (Just "a") (XPct 50, YPct 50) "" [SVGAlign "middle center"]
			]
		}

bMerge :: Binding
bMerge = NodeBinding
	{ NodeBinding
	| declaration = 
		{ GDeclaration 
		| name				= "case merge"
		, title				= Just "Merge" 
		, description		= Just "Merges multiple exclusive branches into a single branch"
		, returnType		= GUndefinedTypeExpression
        , returnDescription	= Nothing
		, formalParams		= []
		, icon				= Just "case-merge"
		, shape				= GBuiltInShape mergeShape
		}
	, parameterMap = NBBuiltIn
	}
where
	mergeShape :: SVGShape
	mergeShape = 
	  	{ SVGShape
		| width		= 20
		, height	= 20
		, defs		= []
		, magnets	= True
		, elements	= 
			[ SVGPolygon Nothing [ (XPct 50, YTop), (XRight, YPct 50), (XPct 50, YBottom), (XLeft, YPct 50)] [SVGAnchors "top left right bottom"] ]
		}

bLet :: Binding
bLet = NodeBinding
	{NodeBinding
	| declaration = 
		{ GDeclaration 
		| name = "let"
		, title = Just "Let"
		, description = Just "Defines a new variable scope"
		, returnType = GUndefinedTypeExpression
        , returnDescription = Nothing
		, formalParams = [ { GFormalParameter 
		                 | name			= "pattern"
		                 , title		= Nothing
		                 , description	= Nothing
		                 , type			= GUndefinedTypeExpression
		                 , defaultValue	= Just "<variable>"
						 , visible		= True
		                 }
		                 , { GFormalParameter 
		                 | name			= "expression"
		                 , title		= Nothing
		                 , description	= Nothing
		                 , type			= GTypeVariable "a" 
		                 , defaultValue	= Just "<expression>"
						 , visible		= True
		                 }
		               ]
		, icon = Just "let"
		, shape = GBuiltInShape letShape
		}
	, parameterMap = NBBuiltIn
	}
where
	letShape :: SVGShape
	letShape = 
	  	{ SVGShape
		| width		= 100
		, height	= 42
		, defs		= []
		, magnets	= True
		, elements	= 
			[ SVGRect Nothing ((XLeft, YTop), (XRight, YBottom)) 0 0 [SVGAnchors "top left right bottom", SVGFill "white", SVGResize "horizontal vertical"]
			, SVGText (Just "pattern")    (XAbs 5, YAbs 13) ""  [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText Nothing             (XAbs 5, YAbs 33) "=" [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText (Just "expression") (XAbs 18, YAbs 33) "" [SVGAnchors "top left", SVGAlign "middle left"]
			]
		}

bListComprehension :: Binding
bListComprehension = NodeBinding
	{NodeBinding
	| declaration = 
		{ GDeclaration 
		| name				= "list comprehension"
		, title				= Just "Task comprehension"
		, description		= Just "Construct a list of tasks based on runtime data"
		, returnType		= gTask (GTypeVariable "a")
        , returnDescription	= Nothing
		, formalParams		= 
			[ { GFormalParameter 
              | name			= "generatorpattern"
              , title			= Nothing
              , description		= Nothing
              , type			= GUndefinedTypeExpression
              , defaultValue	= Nothing
			  , visible			= True
              }
            , { GFormalParameter 
              | name			= "generatorexpression"
              , title			= Nothing
              , description		= Nothing
              , type			= GConstructor "a"
              , defaultValue	= Nothing
			  , visible			= True
              }
            , { GFormalParameter 
              | name			= "guard"
              , title			= Nothing
              , description	= Nothing
              , type			= GConstructor "Bool"
              , defaultValue	= Nothing
			  , visible			= True
              }
            , { GFormalParameter 
			  | name			= "output"
			  , title			= Nothing
			  , description		= Nothing
			  , type			= gTask (GTypeVariable "a")
			  , defaultValue	= Nothing
			  , visible			= True
	          }
          ]
		, icon				= Just "list-comprehension"
		, shape				= GBuiltInShape listComprehensionShape
		}
	, parameterMap = NBBuiltIn
	}
where
	listComprehensionShape :: SVGShape
	listComprehensionShape = 
	  	{ SVGShape
		| width = 150
		, height = 150
		, defs = []
		, magnets = True
		, elements = 
			[ SVGRect Nothing ((XLeft, YTop), (XRight, YBottom)) 0 0 [SVGAnchors "top left right bottom", SVGFill "white", SVGResize "horizontal vertical"]
			, SVGRect Nothing ((XAbs 3, YAbs 3), (XAbs 148, YAbs 148)) 0 0 [SVGAnchors "top left right bottom", SVGFill "white", SVGResize "horizontal vertical"]
			, SVGText Nothing             (XAbs 7, YAbs 13) "foreach" [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText (Just "generatorpattern")    (XAbs 60, YAbs 13) ""  [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText Nothing             (XAbs 7, YAbs 33) "in" [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText (Just "generatorexpression") (XAbs 60, YAbs 33) "" [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText Nothing             (XAbs 7, YAbs 53) "given" [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText (Just "guard") (XAbs 60, YAbs 53) "" [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGLine Nothing ((XAbs 3, YAbs 63),(XAbs 148, YAbs 63)) [SVGAnchors "top left right"]
			, SVGText (Just "output") (XAbs 7, YAbs 73) "" [SVGAnchors "top left", SVGAlign "middle left"]
			]
		}
