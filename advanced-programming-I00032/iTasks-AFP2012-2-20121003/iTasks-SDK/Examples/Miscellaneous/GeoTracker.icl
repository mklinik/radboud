implementation module GeoTracker

import iTasks
import GoogleMaps

geoTrackerExamples :: [Workflow]
geoTrackerExamples = 
	[workflow "Examples/Geo tracker/View map" "Look at the locations of users on the map." viewMap
	,workflow "Examples/Geo tracker/Report position" "Tell us where you are..." reportPosition
	]

locationStore :: Shared [(User,GoogleMapPosition)]
locationStore = sharedStore "Locations" []
	
reportPosition :: Task [(User,GoogleMapPosition)]
reportPosition
	=	enterInformation "Where are you now?" []
	>>= \position ->
		get currentUser 
	>>= \user ->
		update (updatePos (user,position)) locationStore
where
	updatePos (user,position) [] = [(user,position)]
	updatePos (user,position) [(u,p):ps]
		| u == user	= [(user,position):ps]
					= [(u,p):updatePos (user,position) ps]
	
viewMap :: Task Void
viewMap = 	viewInformation "Look where everyone is" [] nlMap
		>>* [AnyTime ActionQuit (const (return Void))]
/*
interact
			"Look where everyone is"
			(\gmap locations _ -> [FormPart (FormValue {GoogleMap|gmap & markers = map mkMarker locations}) (\mbMap -> ({GoogleMap|fromMaybe gmap mbMap & markers = []},Nothing))])
			nlMap
			locationStore
			>>* [AnyTime ActionQuit (const (return Void))]
*/
where
	nlMap :: GoogleMap		
	nlMap = {GoogleMap| defaultMap & perspective = {type = ROADMAP, zoom = 7, center = {lat = 52.396, lng = 5.21}}}
	
	toView :: (GoogleMap, [(User,GoogleMapPosition)]) -> GoogleMap
	toView (gmap,locations) = {GoogleMap|gmap & markers = map mkMarker locations}
	
	fromView :: GoogleMap (GoogleMap,[(User,GoogleMapPosition)]) -> (GoogleMap,[(User,GoogleMapPosition)])
	fromView view (gmap,positions)	= ({GoogleMap|view & markers = []},positions)
		
	mkMarker :: (User,GoogleMapPosition) -> GoogleMapMarker
	mkMarker (user,position)
		=	{ GoogleMapMarker
			| position		= position
			, title			= Just (toString user)
			, icon			= Nothing
			, infoWindow	= Just {GoogleMapInfoWindow|content = toString info}
			, draggable		= False
			, selected		= False
			}
	where
		info = SpanTag []
			[H1Tag [] [Text (toString user)],BrTag []
			,Text "Lat:", Text (toString position.lat), BrTag []
			,Text "Lng: ", Text (toString position.lng), BrTag []
			]
