module pickRGB


//	**************************************************************************************************
//
//	This program creates a windows that allows a user to create a RGB colour.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************


import StdEnv, StdIO


Start :: *World -> *World
Start world
	# (rgbid,world)		= openR2Id  world
	# (ids,  world)		= openIds 7 world
	= startColourPicker rgbid ids world
where
	startColourPicker rgbid ids world
		= startIO SDI Void initialise [ProcessClose closeProcess] world
	where
		initialise pst
			# (rgbsize,pst)	= controlSize (ColourPickControl rgbid ids initrgb Nothing) 
			                              True Nothing Nothing Nothing pst
			# wdef			= Window "Pick a colour" (ColourPickControl rgbid ids initrgb Nothing)
								[	WindowViewSize   rgbsize
								,	WindowPen        [PenBack LightGrey]
								]
			# mdef			= Menu "PickRGB"
								(	MenuItem "MinRGB" [	MenuFunction (noLS (set rgbid BlackRGB))
													  ,	MenuShortKey 'n'
													  ]
								:+:	MenuItem "MaxRGB" [	MenuFunction (noLS (set rgbid WhiteRGB))
													  ,	MenuShortKey 'm'
													  ]
								:+:	MenuSeparator     []
								:+:	MenuItem "Quit"   [	MenuFunction (noLS closeProcess)
													  ,	MenuShortKey 'q'
													  ]
								)	[]
			# (_,pst)		= openWindow undef wdef pst
			# (_,pst)		= openMenu   undef mdef pst
			= pst
		where
		 	initrgb			= {r=MaxRGB,g=MaxRGB,b=MaxRGB}

		set rid rgb pst		= snd (syncSend2 rid (InSet rgb) pst)



/*	The definition of the text-slider component:	*/

::	RGBPickControl ls pst
	:==	:+: SliderControl TextControl ls pst

RGBPickControl :: RGBColour (String,Id,Id) Id (RGBColour->Int) (Int->RGBColour->RGBColour)
				 (Maybe ItemPos)
	-> RGBPickControl RGBColour (PSt .l)
RGBPickControl rgb (text,sid,tid) did get set maybePos
	=	  SliderControl Horizontal length sliderstate slideraction
													[ControlId sid:controlPos]
	  :+: TextControl   (ColourText text (get rgb))	[ControlId tid]
where
	controlPos	= case maybePos of
					Just pos	-> [ControlPos pos]
					_			-> []
	length		= PixelWidth (MaxRGB-MinRGB+1)
	sliderstate	= {sliderMin=MinRGB, sliderMax=MaxRGB, sliderThumb=get rgb}
	
	slideraction :: SliderMove (RGBColour,PSt .l) -> (RGBColour,PSt .l)
	slideraction move (rgb,pst)
		= (	newRGBColour newrgb
		  ,	appListPIO [ setSliderThumb sid y
		  			   , setControlText tid (ColourText text y)
		  			   , setColourBox did newrgb
		  			   ] pst
		  )
	where
		y		= case move of
					SliderIncSmall	-> min (get rgb+1 ) MaxRGB
					SliderDecSmall	-> max (get rgb-1 ) MinRGB
					SliderIncLarge	-> min (get rgb+10) MaxRGB
					SliderDecLarge	-> max (get rgb-10) MinRGB
					SliderThumb x	-> x
		newrgb	= set y rgb
	
ColourText :: String Int -> String
ColourText text x
	= text+++" "+++toString x



/*	The definition of a colour box:		*/

::	ColourBoxControl ls pst
	:==	CustomControl ls pst

ColourBoxControl :: RGBColour Id (Maybe ItemPos) -> ColourBoxControl .ls .pst
ColourBoxControl rgb did maybePos
	= CustomControl {w=40,h=40} (ColourBoxLook rgb)
			[	ControlId did
			:	case maybePos of (Just pos) -> [ControlPos pos];_->[]
			]

ColourBoxLook :: RGBColour SelectState UpdateState *Picture -> *Picture
ColourBoxLook colour _ {newFrame} picture
	# picture	= setPenColour	(RGB colour) picture
	# picture	= fill			newFrame	 picture
	# picture	= setPenColour	Black		 picture
	# picture	= draw			newFrame	 picture
	= picture

setColourBox :: Id RGBColour (IOSt .l) -> IOSt .l
setColourBox id rgb iost
	= setControlLook id True (True,ColourBoxLook rgb) iost



/*	The definition of the RGB access control:	*/

::	In		=	InGet				| InSet RGBColour
::	Out		=	OutGet RGBColour	| OutSet
::	RGBId	:==	R2Id In Out

::	ColourPickAccess pst	:==	Receiver2 In Out RGBColour pst

ColourPickAccess :: RGBId [(String,Id,Id)] Id -> ColourPickAccess (PSt .l)
ColourPickAccess rid rgbpicks did
	= Receiver2 rid accessRGB []
where
	accessRGB :: In (RGBColour,PSt .l) -> (Out,*(RGBColour,PSt .l))
	accessRGB InGet (rgb,pst)
		= (OutGet rgb,(newRGBColour rgb,pst))
	accessRGB (InSet rgb=:{r,g,b}) (_,pst=:{io})
		# io	= setColourBox    did rgb io
		# io	= setSliderThumbs (map (\(y,(_,sid,_))->(sid,y)) settings) io
		# io	= setControlTexts (map (\(y,(text,_,tid))->(tid,ColourText text y)) settings) io
		= (OutSet,(newRGBColour rgb,{pst & io=io}))
	where
		settings= zip2 [r,g,b] rgbpicks



/*	The definition of the assembled colour picking control:	*/

::	ColourPickControl ls pst
	:==	NewLS
		(	LayoutControl
			(	:+:	(LayoutControl (ListLS (:+: SliderControl TextControl)))
			(	:+:	CustomControl
					(Receiver2 In Out)
			))
		)	ls	pst

ColourPickControl :: RGBId [Id] RGBColour (Maybe ItemPos) -> ColourPickControl .ls (PSt .l)
ColourPickControl rgbid ids initrgb maybePos
	= {	newLS = newRGBColour initrgb
	  ,	newDef= LayoutControl
					(	LayoutControl
					(	ListLS
					[	RGBPickControl initrgb rpicks did (\rgb->rgb.r) (\x rgb->{rgb&r=x}) left
					,	RGBPickControl initrgb gpicks did (\rgb->rgb.g) (\x rgb->{rgb&g=x}) left
					,	RGBPickControl initrgb bpicks did (\rgb->rgb.b) (\x rgb->{rgb&b=x}) left
					])	[ControlHMargin 0 0,ControlVMargin 0 0]
					:+: ColourBoxControl initrgb did Nothing
					:+:	ColourPickAccess rgbid [rpicks,gpicks,bpicks] did
					)	(case maybePos of (Just pos) -> [ControlPos pos]; _->[])
	  }
where	
	[rid,rtid,gid,gtid,bid,btid,did:_]	= ids
	(rtext,gtext,btext)					= ("Red","Green","Blue")
	(rpicks,gpicks,bpicks)				= ((rtext,rid,rtid),(gtext,gid,gtid),(btext,bid,btid))
	left								= Just (Left,NoOffset)

newRGBColour :: !RGBColour -> RGBColour
newRGBColour {r,g,b}
	= {r=r,g=g,b=b}
