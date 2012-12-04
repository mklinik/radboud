implementation module google_maps_services

import IntegrationTasks

reverse_geocoding :: !String !String !Bool !String !(String -> a) -> Task a | iTask a
reverse_geocoding q output sensor api_key parsefun = callRPCHTTP GET url args parsefun
where
	url = "http://maps.google.com/maps/geo"
	args =	[ ("q",q)
			, ("output",output)
			, ("sensor",toString sensor)
			, ("api_key",api_key)
			]
