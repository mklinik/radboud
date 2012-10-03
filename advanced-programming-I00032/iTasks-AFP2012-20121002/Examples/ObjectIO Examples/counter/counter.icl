module counter

//	**************************************************************************************************
//
//	Open a dialog that displays a number that can be incremented and decremented using two buttons.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import StdEnv, StdIO

::	CounterSt
	=	{	count	:: !Int
		}

Start :: *World -> *World
Start world
	= startIO NDI Void initIO [] world

initIO pst
	# (dialogid, pst)	= accPIO openId pst
	# (displayid,pst)	= accPIO openId pst
	# (_,pst)			= openDialog Void (dialog dialogid displayid) pst
	= pst
where
	dialog dialogId displayId
		= Dialog "Counter" 
			{	newLS	= { count=init }
			,	newDef	=	EditControl (toString init)	displaywidth displayheight 
								[	ControlPos			(Center,NoOffset)
								,	ControlId			displayId
								,	ControlSelectState	Unable
								]
						:+:	LayoutControl
						(	ButtonControl "&-" 
								[	ControlFunction		(upd (-1))
								,	ControlTip			"Decrement counter value"
								]
						:+:	ButtonControl "&+"
								[	ControlFunction		(upd 1)
								,	ControlTip			"Increment counter value"
								]
						)	[ControlPos (Center,zero)]
			}
			[	WindowClose	(noLS closeProcess)
			,	WindowId	dialogId
			]
	where
		displaywidth	= PixelWidth 100
		displayheight	= 1
		init			= 0
		
		upd :: Int (CounterSt,PSt .l) -> (CounterSt,PSt .l)
		upd dx ({count},pst)
			# count	= count+dx
			= ({count=count},appPIO (setControlText displayId (toString count)) pst)
