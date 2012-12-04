implementation module RPCExamples

import iTasks, XML, Error, Text, GoogleMaps, google_maps_services

rpcExamples :: [Workflow]
rpcExamples = 	[ workflow	"Examples/Miscellaneous/Weather forecast" "Fetches the weather forecast from a remote server" weatherExample
				]

GOOGLE_API = "http://www.google.com/ig/api"

weatherExample :: Task HtmlDisplay
weatherExample = 					
					eitherTask enterLocation markLocation
	>>=				getLocation
	>>= \location.	callRPCHTTP GET GOOGLE_API [("weather", location),("hl","en-GB")] formatResponse
	>>= 			showInformation ("Weather", "Weather forecast is:") []

enterLocation = enterInformation ("Enter location", "Enter a location you want to retrieve the weather forecast for.") []
markLocation =
		updateInformation ("Mark location","Mark a location you want to retrieve the weather forecast for.") [UpdateView (GetLocal toView, PutbackLocal fromView)] defaultMap >>+ (\{modelValue} -> UserActions [(ActionOk,if (oneLocation modelValue) (Just modelValue) Nothing)])
	>>=	transform (\{markers=ms=:[m:_]} -> m.position)
where
	toView = id
	fromView map=:{markers} _ _ = {map & markers = if (isEmpty markers) [] [hd (reverse markers)]}
	
	oneLocation {markers} = (length markers) == 1
		
getLocation (Left loc) = return loc
getLocation (Right {lat,lng}) =
		reverse_geocoding (toString lat+++","+++toString lng) "json" False GOOGLE_API_KEY parseJSON
where
	parseJSON info = case jsonQuery "Placemark/0/address" (fromString info) of
		(Just addr) = replaceSubString ", " "\n" addr
		_			= "Address Unknown"

formatResponse :: !String -> HtmlDisplay
formatResponse xmlStr = case fromString xmlStr of
	Ok xmlDoc 	= toHtmlDisplay (formatXML xmlDoc)
	Error err	= toHtmlDisplay [Text err]
	
formatXML (XMLDoc _ _ e) = [ul [formatElem e]]
formatElem (XMLElem (XMLQName _ n) [XMLAttr (XMLQName _ "data") data] children) = li [formattedData,ul (map formatElem children)]
where
	formattedData
		| n == "icon"	= ImgTag [SrcAttr ("http://www.google.com" +++ data), HeightAttr "80"]
		| otherwise		= Text (formatName n +++ ": " +++ data)
formatElem (XMLElem (XMLQName _ n) attr children)
	# childrenHtml = map formatElem children
	| isMember n showElements	= li [Text (formatName n),ul (map formatAttr attr ++ childrenHtml)]
	| otherwise					= ul childrenHtml
formatAttr (XMLAttr (XMLQName _ n) v) = li [Text (formatName n +++ ": " +++ v)]

ul = UlTag [StyleAttr "list-style: disc;padding-left:20 px"]
li = LiTag [StyleAttr "margin-left: 20px"]

showElements = ["forecast_information","current_conditions","forecast_conditions"]

formatName name = upperCaseFirst (replaceSubString "_" " " name)
