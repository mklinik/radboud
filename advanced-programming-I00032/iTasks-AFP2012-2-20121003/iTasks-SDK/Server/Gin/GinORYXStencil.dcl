definition module GinORYXStencil

import JSON

from GinSyntax import :: GModule
import GinORYX

::ORYXStencilSet =
	{ title			:: !String
	, namespace		:: !String
	, description	:: !String
	, extends		:: !Maybe String
	, stencils		:: ![ORYXStencil]
	, rules			:: !ORYXRules
	, baseUrl		:: !Maybe String
	}
	
::ORYXStencil = 
	{ type			:: !String
	, id			:: !String
	, title			:: !String
	, groups		:: ![String]
	, description	:: !String
	, view			:: !String
	, icon			:: !String
	, mayBeRoot		:: !Bool
	, roles			:: ![String]
	, properties	:: ![ORYXProperties]
	}

::ORYXRules = 
	{ connectionRules	:: ![ORYXConnectionRule]
	, containmentRules	:: ![ORYXContainmentRule]
	, morphingRules		:: ![ORYXMorphingRule]
	}

::ORYXConnectionRule = 
	{ role	:: !String
	, connects :: ![ORYXConnect]
	}

::ORYXConnect = 
	{ from_	:: !String
	, to	:: ![String]
	}

::ORYXContainmentRule = 
	{ role		:: !String
	, contains	:: ![String]
	}

::ORYXMorphingRule = 
	{ role			:: !String
	, baseMorphs	:: ![String]
	}

derive gEq		 	ORYXStencilSet, ORYXStencil, ORYXRules, ORYXConnectionRule, ORYXConnect, ORYXContainmentRule, ORYXMorphingRule
derive JSONEncode	ORYXStencilSet, ORYXStencil, ORYXRules, ORYXConnectionRule, ORYXConnect, ORYXContainmentRule, ORYXMorphingRule
derive JSONDecode 	ORYXStencilSet, ORYXStencil, ORYXRules, ORYXConnectionRule, ORYXConnect, ORYXContainmentRule, ORYXMorphingRule

makeStencilSet :: GModule -> ORYXStencilSet
predefinedStencilSet :: ORYXStencilSet

from GinSVG import :: SVGShape

diagramView :: SVGShape
