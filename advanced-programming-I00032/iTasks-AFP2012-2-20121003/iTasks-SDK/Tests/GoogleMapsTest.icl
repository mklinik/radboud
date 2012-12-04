module GoogleMapsTest

import iTasks
import GoogleMaps

myMap = {defaultMap & markers = myMarkers}

myMarkers = [{GoogleMapMarker|position = pos,title = Just name, icon = Just "house", infoWindow = Just {GoogleMapInfoWindow|content = name}, draggable = True, selected = False} \\ (name,pos) <- [nijmegen]]



nijmegen = ("Nijmegen",{GoogleMapPosition|lat = 51.82, lng = 5.86})

updateMap :: Task GoogleMap
updateMap = updateInformation "Edit markers" [] myMarkers
		>>= \markers -> 
			updateInformation "Do something with the map" [] {myMap & markers = markers}
	
mapsTask :: Task GoogleMap
mapsTask = updateMap >>= viewInformation "Review Your Result" []
	
Start :: *World -> *World
Start world = startEngine mapsTask world