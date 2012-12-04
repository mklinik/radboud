implementation module GinSVG

import StdInt
import StdGeneric
import StdList
import StdString

import Maybe
import XML, HTML, TUIDefinition

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, :: StaticVisualizationMode
from iTasks import class iTask, generic gVisualizeText, generic gVisualizeHtml, generic gVisualizeEditor, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

derive class iTask SVGPosX, SVGPosY, SVGElement, SVGStyle, SVGShape
derive class iTask XMLDoc, XMLNode, XMLAttr, XMLQName

derive bimap Maybe, (,)

instance toString SVGShape
where
	toString {width, height, defs, magnets, elements} = toString doc
	where
		doc :: XMLDoc
		doc = XMLDoc 
			(Just "http://www.w3.org/2000/svg")
			[ ("svg","http://www.w3.org/2000/svg")
			, ("oryx","http://www.b3mn.org/oryx")
			, ("xlink", "http://www.w3.org/1999/xlink")
			]
			(XMLElem (uname "svg") 
				[ XMLAttr (uname "width")  (toString width)
				, XMLAttr (uname "height") (toString height)
				] 
				([defs`] ++ magnets` ++ [group]))
	
		defs` :: XMLNode
		defs` = XMLElem (uname "defs") [] defs
			
		magnets` :: [XMLNode]
		magnets` = if (not magnets) []
			[ XMLElem (qname "oryx" "magnets") [] 
				[ magnet 1 (height / 2) "anchor" "left"
				, magnet (width / 2) height "anchor" "bottom"
				, magnet width (height / 2) "anchor" "right"
				, magnet (width / 2) 1 "anchor" "top"
				, magnet (width / 2) (height / 2) "default" "yes"
				]
			]

		magnet :: Int Int String String -> XMLNode
		magnet cx cy type value = XMLElem (qname "oryx" "magnet")
			[ XMLAttr (qname "oryx" "cx") (toString cx)
			, XMLAttr (qname "oryx" "cy" ) (toString cy)
			, XMLAttr (qname "oryx" type) value
			]
			[]
		
		group :: XMLNode
		group = XMLElem (uname "g") [XMLAttr (uname "pointer-events") "fill"]
			(map (elementToXMLNode width height) elements)

elementToXMLNode :: Int Int SVGElement -> XMLNode
elementToXMLNode width height node = case node of
	(SVGRect sid bounds rx ry styles) = 
		XMLElem (uname "rect")
			( getID sid 
			  ++ getBoundsWidth bounds
			  ++ [ XMLAttr (uname "rx") (toString ry)
				 , XMLAttr (uname "ry") (toString ry) 
				 ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGEllipse sid ((x1,y1),(x2,y2)) styles) =
		XMLElem (uname "ellipse")
			( getID sid ++ 
			  [ XMLAttr (uname "cx") (toString ((getX x1 + getX x2) / 2))
			  , XMLAttr (uname "cy") (toString ((getY y1 + getY y2) / 2))
			  , XMLAttr (uname "rx") (toString ((getX x2 - getX x1) / 2))
			  , XMLAttr (uname "ry") (toString ((getY y2 - getY y1) / 2))
			  ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGLine sid ((x1,y1),(x2,y2)) styles) =
		XMLElem (uname "line")
			( getID sid ++ 
				[ XMLAttr (uname "x1") (toString (getX x1))
				, XMLAttr (uname "y1") (toString (getY y1))
				, XMLAttr (uname "x2") (toString (getX x2))
				, XMLAttr (uname "y2") (toString (getY y2))
				]
			    ++ map styleToXMLAttr styles
			) []
	(SVGPolygon sid points styles) =
		XMLElem (uname "polygon")
			( getID sid ++
			  [ XMLAttr (uname "points") 
			            (foldr (+++) "" (map (\(x,y) -> toString (getX x) +++ "," +++ toString (getY y) +++ " ") points))
			  ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGPath sid d styles) =
		XMLElem (uname "path")
			( getID sid
			  ++ [ XMLAttr (uname "d") d ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGText sid (x,y) text styles) = 
		XMLElem (uname "text")
			( getID sid ++ 
				[ XMLAttr (uname "x") (toString (getX x))
				, XMLAttr (uname "y") (toString (getY y))
			 	]
 			    ++ map styleToXMLAttr styles
			) [XMLText text]
	(SVGImage sid bounds image styles) = 
		XMLElem (uname "image")
			( getID sid 
			  ++ getBoundsWidth bounds
			  ++ [ XMLAttr (qname "xlink" "href") image ]
		      ++ map styleToXMLAttr styles
			) []
	(SVGGroup sid elements) = 
		XMLElem (uname "g") (getID sid) (map (elementToXMLNode width height) elements)
where
	getID :: SVGId -> [XMLAttr]
	getID Nothing    = []
	getID (Just sid) = [XMLAttr (uname "id") sid]
	
	getBoundsWidth :: SVGBounds -> [XMLAttr]
	getBoundsWidth  ((x1,y1),(x2,y2)) =
		[ XMLAttr (uname "x") (toString (getX x1))
		, XMLAttr (uname "y") (toString (getY y1))
		, XMLAttr (uname "width") (toString (getX x2 - getX x1))
		, XMLAttr (uname "height") (toString (getY y2 - getY y1))
		]
	
	getX :: SVGPosX -> Int
	getX XLeft = 0
	getX XRight = width
	getX (XAbs x) = x
	getX (XPct p) = (p * width / 100)
	
	getY :: SVGPosY -> Int
	getY YTop = 0
	getY YBottom = height
	getY (YAbs y) = y
	getY (YPct p) = (p * height / 100)
	
styleToXMLAttr :: SVGStyle -> XMLAttr
styleToXMLAttr (SVGStroke s)			= XMLAttr (uname "stroke") s
styleToXMLAttr (SVGFill s)				= XMLAttr (uname "fill") s
styleToXMLAttr (SVGStrokeWidth w)		= XMLAttr (uname "stroke-width") (toString w)
styleToXMLAttr (SVGStrokeDashArray a)	= XMLAttr (uname "stroke-dasharray") (toString a)
styleToXMLAttr (SVGStrokeLineCap s)		= XMLAttr (uname "stroke-linecap") s
styleToXMLAttr (SVGStrokeLineJoin s)	= XMLAttr (uname "stroke-linejoin") s
styleToXMLAttr (SVGStrokeMiterLimit l)	= XMLAttr (uname "stroke-miterlimit") (toString l)
styleToXMLAttr (SVGMarkerEnd m)			= XMLAttr (uname "marker-end") m
styleToXMLAttr (SVGFontWeight w)        = XMLAttr (uname "font-weight") w
styleToXMLAttr (SVGAlign s)				= XMLAttr (qname "oryx" "align") s
styleToXMLAttr (SVGAnchors s)			= XMLAttr (qname "oryx" "anchors") s
styleToXMLAttr (SVGEdgePosition s)		= XMLAttr (qname "oryx" "edgePosition") s
styleToXMLAttr (SVGResize s)			= XMLAttr (qname "oryx" "resize") s 
