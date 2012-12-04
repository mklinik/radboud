module FindReport
/*
	This application lets a team map possible "dangerous" finds during patrols.
	Depending on the properties of a find, specialist team members are tasked with classifying
	or investigating the find more closely.

	This app defines its own user database as a team of specialists
*/

//Import of core framework + extension libraries
import iTasks, GoogleMaps, Collection

//Application specific constants
BASE_POSITION	:== {lat = 51.842168, lng = 5.859413}
BASE_MAP		:== {defaultMap & perspective = {defaultMap.perspective & center = BASE_POSITION, zoom = 12}}

//We define a custom 'user-model' tailored to the application
:: TeamMember =
	{ name			:: TeamMemberName
	, credentials	:: Credentials
	, specialty		:: Maybe Specialty
	, position		:: GoogleMapPosition
	}
:: TeamMemberName	:== String

:: Specialty
	= ChemExpert
	| BombExpert

//We define a data type for tracking information about suspicious "finds"
:: Find =
	{ findNo			:: FindNo
	, description		:: FindDescription
	, position			:: GoogleMapPosition
	, reportedBy		:: User
	, reportedAt		:: DateTime
	, classification	:: FindClassification
	}
:: FindNo :== Int

:: FindDescription =
	{ description	:: Note
	, smells		:: Bool
	, ticks			:: Bool
	}

:: FindClassification
	= Suspicious
	| Dangerous
	| Safe

//Generate boilerplate code for persistance and ui
derive class iTask TeamMember, Specialty, Find, FindDescription, FindClassification

//Define a custom authentication task for this application
toUser :: TeamMember -> User
toUser {TeamMember|credentials={Credentials|username=Username u}} = NamedUser u

authenticate :: Credentials -> Task (Maybe User)
authenticate creds
	=	get team
	@	\l -> case [t \\ t <- l | t.credentials === creds] of
		[t]	= Just (toUser t)
		_	
			| creds === {Credentials|username=Username "admin",password=Password "secret"}	= Just RootUser
			| otherwise																		= Nothing

//Define application wide global data sources
finds :: Shared [Find]
finds = sharedStore "finds" [] 

team :: Shared [TeamMember]
team = sharedStore "team" []

//Define queries on the global data as derived data sources
memberPosition :: TeamMemberName -> Shared GoogleMapPosition
memberPosition name = mapReadWrite (toPrj,fromPrj) team
where	
	toPrj l	= case [t \\ t <- l| t.TeamMember.name == name] of
		[{TeamMember|position}] = position
		_						= BASE_POSITION
	
	fromPrj position l = Just [if (t.TeamMember.name == name) {TeamMember|t & position = position} t \\ t <- l]
	
myPosition :: Shared GoogleMapPosition
myPosition = mapRead userName currentUser >+> memberPosition

// 'Field' tasks. Operational stuff
// There are two tasks for people in the field:
// - report their current position
// - report a find

reportPosition :: Task GoogleMapPosition
reportPosition
	=	updateSharedInformation ("Report location","Where are you now?") [] myPosition
	>>* [WithResult ActionFinish (const True) return]
	
reportFind :: Task Find
reportFind
	=	(enterInformation ("Report find","Describe the suspicious object as good as possible") []
		-&&-
		(get myPosition >>= updateInformation "Where did you find the suspicious object?" []))
	>>! \(description,position) ->
		addFind description position
	>>= respondToFind
	>>= viewInformation (Title "Find stored") [DisplayView (GetLocal (\f -> "Stored find as number " <+++ f.findNo))]
	>>* ok

//Add a find to the shared data model	
addFind :: FindDescription GoogleMapPosition -> Task Find
addFind description position
	=	get (currentUser |+| currentDateTime)
	>>=	\(reportedBy,reportedAt) ->
		update (\l -> l ++ [{Find|findNo = length l, description = description,position = position
							,reportedBy = reportedBy, reportedAt = reportedAt, classification = initialClass description}]) finds @ last
where
	//If something is ticking classify it as dangerous immediately
	initialClass {ticks,smells}
		| ticks = Dangerous
				= Suspicious

//Update an existing find	
updateFind :: FindNo (Find -> Find) -> Task FindNo
updateFind findNo fun
	= update (\l -> [if (f.findNo == findNo) (fun f) f \\ f <- l]) finds @ const findNo

//Respond to a find, (completely automatic in this case)
respondToFind :: Find -> Task Find
respondToFind find=:{Find|description={ticks,smells},position}
	| smells
		=	findSpecialist ChemExpert
		>>= \expert ->
			appendTopLevelTaskFor expert (classifyFind find)
		>>| return find
	| ticks
		=	findNearestSpecialist BombExpert position
		>>= \expert ->
			appendTopLevelTaskFor expert (investigateFind find)
		>>| return find
	| otherwise
		= return find

//Classify a find based on the available information		
classifyFind :: Find -> Task FindClassification
classifyFind find
	=	enterChoice("Classify find","Classify the following find")
			[ChoiceContext find, ChoiceView (AutoChoiceView,id)]
			[Dangerous,Suspicious,Safe]
	>>*	[WithResult (Action "Update classification") (const True) (updateClassification find.findNo)]

//Investigate a find 
investigateFind :: Find -> Task FindClassification
investigateFind find
	= 	enterChoice ("Investigate find","Go to the following find and investigate it")
			[ChoiceContext find, ChoiceView (AutoChoiceView,id)]
			[Dangerous,Suspicious,Safe]
	>>*	[WithResult (Action "Update classification") (const True) (updateClassification find.findNo)]
	
updateClassification :: FindNo FindClassification -> Task FindClassification
updateClassification findNo classification
	=	updateFind findNo (\f -> {f & classification = classification})
	@	const classification

//Choose a random team member with the specified specialty		 
findSpecialist :: Specialty -> Task User
findSpecialist specialty
	=	get team
	@	(\l -> [toUser t \\ t <- l | t.specialty === Just specialty])
	>>=	randomChoice

//Sort team members with a specialty by distance to a certain location and pick closest
findNearestSpecialist :: Specialty GoogleMapPosition -> Task User
findNearestSpecialist specialty position
	=	get team
	@	sortBy (closestTo position)
	@	(\l -> [toUser t \\ t <- l | t.specialty === Just specialty])
	>>=	firstChoice
where
	closestTo position {TeamMember|position=p1} {TeamMember|position=p2}
		= (distance position p1) < (distance position p2)

// Define a custom task for editing the application's users
editTeam :: Task Void
editTeam = forever (
	enterSharedChoice (Title "Edit team") [] team
	>>* [AnyTime ActionNew (const addTeamMember)
		,WithResult ActionEdit (const True) editTeamMember
		,WithResult ActionDelete (const True) deleteTeamMember
		,AnyTime (Action "Load demo team") (const loadDemoTeam)
		]
	)
where
	addTeamMember
		= 	 enterInformation "Enter name" []
		-&&- enterInformation "Enter credentials details" [] 
		-&&- enterInformation "Enter speciality" []
		>>* [AnyTime ActionCancel (const (return Void))
			,WithResult ActionOk (const True) save
			]
	where
		save (name,(credentials,specialty))
			=	update (\l -> l ++ [{name = name,credentials=credentials,position = BASE_POSITION, specialty = specialty}]) team
			@ 	const Void
			
	editTeamMember member
		= 	viewInformation "Edit team member" [] member
		>>* [AnyTime (Action "Close") (const (return Void))]
	
	deleteTeamMember member 
		=	update (\l -> [m \\ m <- l | m =!= member]) team
		@	const Void

	loadDemoTeam
		=	importCSVFile "demoteam.csv"
		>>=	\lines ->
			set [{TeamMember|name=n,credentials={Credentials|username=Username n,password = Password p},specialty=specialty s,position=BASE_POSITION}
				\\ [n,p,s:_] <- lines] team
		@	const Void
	where
		specialty "bomb"	= Just BombExpert
		specialty "chem"	= Just ChemExpert
		specialty _			= Nothing

//Use a library task for basic crud tasks on finds data
editFinds :: Task Void
editFinds = manageCollection (Title "Manage finds") "find" (\f -> f.findNo) finds @ const Void

//Geo-spatial awareness task, where is everyone, and where are the finds	
viewTeamAndFindsPositions :: Task Void
viewTeamAndFindsPositions
	=	viewSharedItemsOnMap (Title "View positions") toMarker BASE_MAP (eitherShare team finds)  <<@ AfterLayout (tweakTUI fill) //Full screen
	@	const Void
where
	toMarker (Left member=:{TeamMember|credentials,position})
		= {GoogleMapMarker
		  |position = position,title= Just (userName (toUser member)),icon = Nothing
		  ,infoWindow = Nothing, draggable = False, selected = False}
	toMarker (Right {Find|findNo,description,position,classification})
		= {GoogleMapMarker
		  |position = position,title = Just ("Find #" <+++ findNo), icon = Just (icon classification)
		  ,infoWindow = Just {GoogleMapInfoWindow|content=toString description.FindDescription.description}
		  ,draggable = False, selected = False}

	icon Suspicious		= "find-unknown"
	icon Safe			= "find-safe"
	icon Dangerous		= "find-dangerous"
	
Start :: *World -> *World
Start world = startEngine main world
where
	//The 'meta' task for a user's interaction with the application
	main :: Task [Void]
	main = forever (catchAll (doAuthenticateWith authenticate useApplication) viewError) 
	where
		useApplication = allTasks
			[manageWorklist flows
			,editTeam
			,editFinds
			,viewTeamAndFindsPositions
			] 								<<@ SetLayout tabbedLayout <<@ AfterLayout (tweakTUI fill) //Custom layout
			
		viewError e = viewInformation ("Error","Oops something went wrong") [] e @ const [] >>* ok
				
		flows = [workflow "Report position" "Report position" reportPosition
				,workflow "Report find" "Report find" reportFind
				]

//General stuff that should have been in the libraries
viewSharedItemsOnMap :: d (r -> GoogleMapMarker) GoogleMap (ReadWriteShared [r] w) -> Task GoogleMapPerspective | descr d & iTask r
viewSharedItemsOnMap desc toMarker initMap items 
	= withShared initMap.perspective viewMap
where
	viewMap perspective
		= updateSharedInformation Void [UpdateView (GetShared toMapView) fromMapView] (items |+< perspective)
	toMapView (items,perspective)
		= {initMap & perspective = perspective, markers = map toMarker items}
	
	fromMapView	viewmap _ _	= viewmap.perspective

eitherShare :: (ReadWriteShared [r1] w1) (ReadWriteShared [r2] w2) -> (ReadOnlyShared [Either r1 r2])
eitherShare s1 s2 = mapRead (\(l,r) -> map Left l ++ map Right r) (s1 |+| s2)

ok = [WithResult ActionOk (const True) return]

firstChoice [a:_]	= return a
firstChoice []		= throw "Cannot choose from an empty list"

distance :: GoogleMapPosition GoogleMapPosition -> Real
distance p1 p2 = 42.0	//Obviously incorrect :) Should be in the GoogleMaps library
