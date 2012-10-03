module FlightCheckIn

import iTasks,Tasklet,FlightSupport

ifStable (Value v Stable) = True
ifStable _ = False

always = const True

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

maybeStable :: (Maybe a) -> (TaskValue a)
maybeStable (Just v) = Value v Stable
maybeStable _        = NoValue

:: BookingInfo = BookingReference String | PassangerLastName String
:: Booking = {bookingRef :: String, firstName :: String, lastName :: String, flightNumber :: String, id :: Hidden String, seat :: Maybe Seat}
:: Flight = {flightNumber :: String, rows :: Int, layout :: [Int], freeSeats :: [Seat]}

derive class iTask BookingInfo, Booking, MaybeError, Flight

derive gVisualizeEditor Seat
derive gHeaders Seat
derive gGridRows Seat
derive gUpdate Seat
derive gDefaultMask Seat
derive gVerify Seat
derive JSONEncode Seat
derive JSONDecode Seat
derive gEq Seat

gVisualizeText{|Seat|} _ seat = [toString seat]

commitCheckIn b seat (bs, fs)
	= (updateBooking b.id seat bs, removeSeat b.Booking.flightNumber seat fs)

removeSeat flightNumber seat [f:fs]
	| f.Flight.flightNumber == flightNumber
		= [{f & freeSeats = removeMember seat f.freeSeats}:fs]
		= [f:removeSeat flightNumber seat fs] 

updateBooking pid seat [] = []
updateBooking pid seat [p:ps]
	| fromHidden p.id == fromHidden pid 
		= [{p & seat = Just seat}:ps]
		= [p:updateBooking pid seat ps] 

findFlight fno = 
	get flightStore >>= return o find (\f -> f.Flight.flightNumber == fno)

findBooking ref = 
	get bookingStore >>= return o find (\{bookingRef} -> bookingRef == ref)

getBookings f =
	get bookingStore >>= return o filter f

bookingStore :: Shared [Booking]
bookingStore = sharedStore "Bookings" 
/*	[ {firstName = "Will", lastName = "Smith", bookingRef = "BHJZ345", flightNumber = "BA2334", id = Hidden "ID1L", seat = Nothing}
	, {firstName = "Adam",   lastName = "Smith", bookingRef = "BHJZ346", flightNumber = "BA5623", id = Hidden "ID8I", seat = Nothing}
	, {firstName = "Teodor", lastName = "Domoszlai", bookingRef = "BHJZ347", flightNumber = "BA1234", id = Hidden "ID3T", seat = Just (Seat 1 1)}]
*/
	[ {firstName = "Laszlo", lastName = "Domoszlai", bookingRef = "BHJZ345", flightNumber = "BA1234", id = Hidden "ID1L", seat = Nothing}
	, {firstName = "Igor",   lastName = "Domoszlai", bookingRef = "BHJZ346", flightNumber = "BA1234", id = Hidden "ID8I", seat = Nothing}
	, {firstName = "Teodor", lastName = "Domoszlai", bookingRef = "BHJZ347", flightNumber = "BA1234", id = Hidden "ID3T", seat = Just (Seat 1 1)}]


flightStore :: Shared [Flight]
flightStore = sharedStore "Flights" 
	[{flightNumber = "BA1234", rows = 20, layout = [3,3], freeSeats = [Seat 13 1,Seat 13 5, Seat 11 1,Seat 10 2]}]

task_checkin :: Task Void
task_checkin
	= catchAll (	
	    enterInformation "Please enter booking information:" []
	>>=	\ref  -> lookUpBooking ref
	>>= \mbP  -> verifyBooking mbP
	>>= \p    -> findFlight p.Booking.flightNumber 
	>>= \f    -> chooseSeat f
	>>= \seat -> update (commitCheckIn p seat) (bookingStore >+< flightStore)
	>>| findBooking p.bookingRef // refresh booking record
	>>= viewInformation "Check-in succeeded:" []
	>>| task_checkin
	//>>*	[OnAction ActionNext always (const task_checkin)]
	) errorHandler
where
	errorHandler msg = viewInformation "Error:" [] msg >>| task_checkin

	lookUpBooking (BookingReference ref) = findBooking ref
	lookUpBooking (PassangerLastName lname) 
		=   getBookings (\p -> p.lastName == lname && isNothing p.seat)
		>>= \bs -> case bs of 
			[] = return Nothing
			fs = enterChoice "Please choose passenger:" [] fs >>= return o Just	

	verifyBooking Nothing = throw "Passenger cannot be found."
	verifyBooking (Just b) | isJust b.seat = throw "Passenger already checked-in."
	verifyBooking (Just p) =  viewInformation "Passenger:" [] p 
								||-
								enterInformation "Please enter you id number:" []
		>>= \id -> if (fromHidden p.id == id) (return p) (throw "Identification falied.")

/*
	chooseSeat (Just f)
	    =   enterChoice "Please choose seat:" [] (map toString (sort f.freeSeats))
		>>= return o fromString
*/

chooseSeat (Just f) = mkTask seatTasklet
where
	seatTasklet :: Tasklet (Maybe Seat) Seat
	seatTasklet = 
		{ generatorFunc		= (\_ iworld -> (TaskletHTML gui, Nothing, iworld))
		, resultFunc		= maybeStable
		, tweakUI  			= \t = (paneled (Just "Seat chooser Tasklet") (Just "Choose seat:") Nothing [t])	
		}

	occupiedStyle = StyleAttr "float: left; border-style:solid; background-color:blue; border-color:black; width: 15px; height: 15px; margin: 1px;"
	freeStyle = StyleAttr "float: left; border-style:solid; background-color:white; border-color:black; width: 15px; height: 15px; margin: 1px;"
	corridorStyle = StyleAttr "float: left; background-color:white; width: 20px; height: 15px;"

	rowLayout = intercalate [-1] (numbering 1 f.layout)
	numbering i [] = []
	numbering i [x:xs] = [take x [i..] : numbering (i+x) xs]

	genSeatId seat = "_seat_" +++ toString seat

	genRowUI (Seat _ -1) = DivTag [corridorStyle] []
	genRowUI seat | isMember seat f.freeSeats
			= DivTag [TitleAttr (toString seat), IdAttr (genSeatId seat), freeStyle] []
			= DivTag [TitleAttr (toString seat), occupiedStyle] []

	attachHandlers seat = 
 		[HtmlEvent (genSeatId seat) "click" (setState (Just seat)),
 		 HtmlEvent (genSeatId seat) "mouseover" (setColor "red"),
 		 HtmlEvent (genSeatId seat) "mouseout" (setColor "white")]
 		
 	setState newst _ _ _ d  = (d, newst)
 	setColor color st _ e d	=
		(fst3 (setObjectAttr d e "target.style.backgroundColor" color), st)

	htmlui = DivTag [] (intercalate [DivTag [StyleAttr "clear: both;"] []]
								    [map (\s -> genRowUI (Seat r s)) rowLayout \\ r <- [1 .. f.rows]])
			
	gui = { TaskletHTML |
				  width  		= Fixed 300
				, height 		= Fixed 300
				, html   		= HtmlDef htmlui
				, eventHandlers = concatMap attachHandlers f.freeSeats
				}
	
								 
taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Check-in" "Flight check-in" task_checkin]
    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world



