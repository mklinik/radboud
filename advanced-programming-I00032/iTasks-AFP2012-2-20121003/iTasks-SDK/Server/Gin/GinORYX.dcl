definition module GinORYX

import GenEq
import JSON, HTML, TUIDefinition

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, :: StaticVisualizationMode
from iTasks import class iTask, generic gVisualizeText, generic gVisualizeHtml, generic gVisualizeEditor, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

from GinSyntax import ::GDeclaration, ::GModule, ::GGraph, ::GImport, ::Binding, ::Bindings
from GinParser import ::GPath, ::GPathNode

:: ORYXEditor =	{ diagram		:: ORYXDiagram
				, stencilset	:: ORYXStencilSetReference
				, errors		:: [ORYXError]
				}

:: ORYXBound =	
	{ x :: !Real
	, y :: !Real
	}

:: ORYXBounds =	
	{ lowerRight	:: !ORYXBound
	, upperLeft		:: !ORYXBound
	}

:: ORYXChildShape =	
	{ resourceId	:: !ORYXResourceId
	, properties	:: !ORYXProperties
	, stencil		:: !ORYXStencilReference
	, childShapes	:: ![ORYXChildShape]
	, outgoing		:: ![ORYXOutgoing]
	, bounds		:: !ORYXBounds
	, dockers		:: ![ORYXDocker]
	, target		:: Maybe ORYXTarget
	}
	
:: ORYXProperties = ORYXProperties [ORYXProperty]

:: ORYXProperty =
	{ key	:: !String
	, value	:: !JSONNode
	}
	
:: ORYXDiagram = 
	{ resourceId	:: !ORYXResourceId
	, properties	:: !ORYXProperties
	, stencil	 	:: !ORYXStencilReference
	, childShapes	:: ![ORYXChildShape]
	, bounds		:: !ORYXBounds
	, stencilset	:: !ORYXStencilSetReference
	, ssextensions	:: ![ORYXStencilSetExtension]
	}
	
:: ORYXDocker = 
	{ x	:: !Real
	, y :: !Real
	}
	
:: ORYXOutgoing =
	{ resourceId	:: !ORYXResourceId
	}
 
:: ORYXResourceId :== String

:: ORYXStencilReference = 
	{ id			:: !ORYXResourceId
	}

:: ORYXStencilSetReference =
	{ url		:: !String
	, namespace	:: !String
	}

:: ORYXStencilSetExtension :== String

:: ORYXTarget =
	{ resourceId	:: !ORYXResourceId
	}
	
:: ORYXError = 
	{ resourceId	:: !ORYXResourceId
	, message		:: !String
	, paramName		:: !Maybe String
	}

// Stencil definition types

derive gEq		 		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive JSONEncode		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive JSONDecode 		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gVisualizeText	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gVisualizeHtml	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gVisualizeEditor	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gUpdate	    	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gDefaultMask		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gVerify  		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError

oryxDiagramToGraph :: !Bindings !ORYXDiagram -> GGraph

emptyORYXEditor :: ORYXEditor

petriNetORYXEditor :: ORYXEditor

bpmnORYXEditor :: ORYXEditor

xmasORYXEditor :: ORYXEditor

ginORYXDiagram :: ORYXDiagram

ginORYXEditor :: !ORYXDiagram /*!(ORYXEditor *IWorld -> *(WorldPredicateResult,*IWorld))*/-> ORYXEditor

updateDiagramExtensions :: !GModule -> GModule

makeORYXError :: !ORYXDiagram !(GPath,String) -> ORYXError
