definition module GinSVG

import Maybe
import XML, HTML, TUIDefinition

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, :: StaticVisualizationMode
from iTasks import class iTask, generic gVisualizeText, generic gVisualizeHtml, generic gVisualizeEditor, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

:: SVGPosX	= XLeft
			| XRight
			| XAbs !Int
			| XPct !Int

:: SVGPosY	= YTop
			| YBottom
			| YAbs !Int
			| YPct !Int

:: SVGCoordinate :== (!SVGPosX, !SVGPosY)

:: SVGBounds :== (!SVGCoordinate, !SVGCoordinate)

:: SVGElement	= SVGRect		SVGId !SVGBounds !Int !Int ![SVGStyle]
				| SVGEllipse	SVGId !SVGBounds ![SVGStyle]
				| SVGLine		SVGId !SVGBounds ![SVGStyle]
				| SVGPolygon	SVGId [SVGCoordinate] ![SVGStyle]
				| SVGPath		SVGId !String ![SVGStyle]
				| SVGText 		SVGId !SVGCoordinate !String ![SVGStyle]
				| SVGImage		SVGId !SVGBounds !String ![SVGStyle]
				| SVGGroup		SVGId [SVGElement]
				
:: SVGStyle = 
			//Basic SVG
			  SVGStroke !String
			| SVGFill !String
			| SVGStrokeWidth Int
			| SVGStrokeDashArray !String
			| SVGStrokeLineCap !String
			| SVGStrokeLineJoin !String
			| SVGStrokeMiterLimit !Int
			| SVGMarkerEnd !String
			| SVGFontWeight !String
			// ORYX extensions
			| SVGAlign !String
			| SVGAnchors !String
			| SVGEdgePosition !String
			| SVGResize !String

:: SVGShape = 
	{ width		:: !Int
	, height	:: !Int
	, defs		:: ![XMLNode]
	, magnets	:: !Bool
	, elements	:: ![SVGElement]
	}
	
:: SVGId :== Maybe String

derive class iTask SVGShape

instance toString SVGShape
