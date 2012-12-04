implementation module tmdialog


import	StdBool, StdChar, StdFunc, StdList, StdMisc, StdTuple
import	StdControl, StdId, StdMenuElement, StdPSt, StdPStClass, StdSystem, StdWindow
import	showtm, tmfile, Notice


::	TmIds
	=	{	windowID		:: Id
		,	tapeWdID		:: Id
		,	fileMenuId		:: Id
		,	saveItemId		:: Id
		,	machineMenuId	:: Id
		,	stepItemId		:: Id
		,	haltItemId		:: Id
		}
::	Tm
	=	{	tmstate			:: !TmState
		,	name			:: !String
		,	delay			:: !Int
		,	saved			:: !Bool
		,	tmids			:: !TmIds
		}


HelpFile
	:== "TuringHelp"


/*	Create the necessary Ids for the turing program.
*/
openTmIds :: !*env -> (!TmIds,!*env)	| Ids env
openTmIds env
	# (ids,env)			= openIds 7 env 
	  [windowID,tapeID,fileMenuId,saveItemId,machineMenuId,stepItemId,haltItemId:_]
						= ids
	= (	{	windowID		= windowID
		,	tapeWdID		= tapeID
		,	fileMenuId		= fileMenuId
		,	saveItemId		= saveItemId
		,	machineMenuId	= machineMenuId
		,	stepItemId		= stepItemId
		,	haltItemId		= haltItemId
		}
	  ,	env
	  )


/*	The dialog to alter the contents of a tape cell.
*/
AlterCell :: Int (PSt Tm) -> PSt Tm
AlterCell pos pst=:{ls=tm=:{tmstate={turing={tape}},tmids={tapeWdID}}}
	# (ids,pst)	= accPIO (openIds 3) pst
	# pst		= appPIO (appWindowPicture tapeWdID (HiliteCell pos cell)) pst
	# (_,pst)	= openModalDialog undef (dialog ids) pst
	= pst
where
	cell		= CellContents pos tape
	
	dialog ids	= Dialog "Change Tape Cell"
					(	LayoutControl
					(	TextControl		"Write:"						[]
					:+:	EditControl		"" (PixelWidth (hmm 15.0)) 1	[ControlId celId]
					)	[ControlPos (Center,zero)]
					:+:	LayoutControl
					(	ButtonControl	"Cancel"						[ControlFunction (noLS (Cancel pos cell))]
					:+:	ButtonControl	"OK"							[ControlFunction (noLS (Ok     pos     )),ControlId okId]
					)	[ControlPos (Center,zero)]
					)
					[	WindowOk	okId
					,	WindowId	wId
					]
	where
		wId		= ids!!0
		okId	= ids!!1
		celId	= ids!!2
		
		Ok :: Int (PSt Tm) -> PSt Tm
		Ok pos pst=:{ls=tm=:{tmstate,tmids={tapeWdID}},io}
			# (maybeDialog,io)	= getWindow wId io
			  cell				= FirstChar (fromJust (snd (getControlText celId (fromJust maybeDialog))))
			  newtape			= ChangeCellContents pos cell tmstate.turing.tape
			  tm				= {tm & tmstate={tmstate & turing={tmstate.turing & tape=newtape}}}
			# io				= appWindowPicture tapeWdID (DrawTapeCell pos cell) io
			= closeWindow wId {pst & ls=tm,io=io}
		
		Cancel :: Int Char (PSt Tm) -> PSt Tm
		Cancel pos cell pst=:{ls={tmids={tapeWdID}},io}
			= closeWindow wId {pst & io=appWindowPicture tapeWdID (DrawTapeCell pos cell) io}

//	The dialog to alter a transition.
AlterTransition :: Int (PSt Tm) -> PSt Tm
AlterTransition tnr pst=:{ls=tm=:{tmstate={turing={transitions}},tmids={windowID,tapeWdID}}}
	# (ids,pst)	= accPIO (openIds 6)													pst
	# pst		= appPIO (appWindowPicture windowID (HiliteTransition tnr transition))	pst
	# pst		= appPIO (appWindowPicture tapeWdID EraseError)							pst
	# (_,pst)	= openModalDialog undef (dialog ids)									pst
	= pst
where
	transition	= GetTransition tnr transitions
	
	ctos :: Char -> String
	ctos c		= if (c==' ') "" (toString c)
	
	dialog ids	= Dialog "Change Transition" 
					(	LayoutControl
					(	TextControl		"From:"												[	ControlPos (Left,zero)
																							,	ControlWidth (PixelWidth (hmm 20.0))
																							]
					:+:	EditControl		transition.start (PixelWidth (hmm 25.0)) 1			[	ControlId fromId
																			 				]
					:+:	TextControl		"With:"												[	ControlPos (Left,zero)
																							,	ControlWidth (PixelWidth (hmm 20.0))
																							]
					:+:	EditControl		(ctos transition.sigma) (PixelWidth (hmm 25.0)) 1	[	ControlId headId
																							]
					)	[ControlHMargin 0 0,ControlVMargin 0 0]
					:+:	LayoutControl
					(	TextControl		"To:"												[	ControlPos (Left,zero)
																							,	ControlWidth (PixelWidth (hmm 20.0))
																							]
					:+:	EditControl		transition.end (PixelWidth (hmm 25.0)) 1			[	ControlId toId
																							]
					:+:	TextControl		"Action:"											[	ControlPos (Left,zero)
																							,	ControlWidth (PixelWidth (hmm 20.0))
																							]
					:+:	EditControl		(ctos transition.move) (PixelWidth (hmm 25.0)) 1	[	ControlId moveId
																							]
					)	[ControlHMargin 0 0,ControlVMargin 0 0]
					:+:	LayoutControl
					(	ButtonControl	"Cancel"	[ControlFunction (noLS (Cancel tnr transition))]
					:+:	ButtonControl	"Remove"	[ControlFunction (noLS (Remove tnr))]
					:+:	ButtonControl	"OK"		[ControlFunction (noLS (Ok tnr)),ControlId okId]
					)	[ControlPos (Center,zero)]
					)
					[	WindowId	wId
					,	WindowOk	okId
					]
	where
		[wId,fromId,headId,toId,moveId,okId:_]
				= ids
		
		Ok :: Int (PSt Tm) -> PSt Tm
		Ok tnr pst=:{ls=tm=:{tmstate,tmids={windowID,fileMenuId,saveItemId}},io}
			# (maybeDialog,io)	= getWindow wId io
			  dialog			= fromJust maybeDialog
			  transition		= {	start	= FourCharString	(fromJust (snd (getControlText fromId dialog)))
								  ,	sigma	= FirstChar			(fromJust (snd (getControlText headId dialog)))
								  ,	end		= FourCharString	(fromJust (snd (getControlText toId   dialog)))
								  ,	move	= FirstChar			(fromJust (snd (getControlText moveId dialog)))
								  }
			  newtransitions	= ChangeTransition tnr transition turing.transitions
			# io				= appWindowPicture windowID (ShowTrans tnr transition) io
			# io				= enableMenuElements [saveItemId] io
			# pst				= {pst & ls={tm & tmstate={tmstate & turing={turing & transitions=newtransitions}},saved=False},io=io}
			# pst				= closeWindow wId pst
			= pst
		where
			turing				= tmstate.turing
		
		Cancel :: Int Transition (PSt Tm) -> PSt Tm
		Cancel tnr transition=:{start} pst=:{ls={tmids={windowID}}}
			# pst			= closeWindow wId pst
			| start==""		= appPIO (appWindowPicture windowID (EraseTrans tnr)) pst
			| otherwise		= appPIO (appWindowPicture windowID (ShowTrans  tnr transition)) pst
		
		Remove :: Int (PSt Tm) -> PSt Tm
		Remove tnr pst=:{ls=tm=:{tmstate,tmids={fileMenuId,saveItemId}}}
			# pst			= closeWindow wId pst
			# pst			= appPIO (enableMenuElements [saveItemId]) pst
			# pst			= {pst & ls={tm & tmstate = {tmstate & turing={turing & transitions=newtransitions},transition=0}
										    , saved   = False
									    }}
			= ReDraw pst
		where
			turing			= tmstate.turing
			newtransitions	= RemoveTransition tnr turing.transitions


//	The dialog to alter the state of the T.M.
AlterState :: (PSt Tm) -> PSt Tm
AlterState pst=:{ls=tm=:{tmstate={turing={state}},tmids={tapeWdID,windowID}}}
	# pst		= appPIO (appWindowPicture tapeWdID EraseError) pst
	# pst		= appPIO (appWindowPicture windowID (HiliteState state)) pst
	# (ids,pst)	= accPIO (openIds 3) pst
	# (_,pst)	= openModalDialog undef (dialog ids) pst
	= pst
where
	dialog ids	= Dialog "Change State" 
					(	TextControl		"State:"						[ControlPos (Left,zero)]
					:+:	EditControl		"" (PixelWidth (hmm 25.0)) 1	[ControlId editId]
					:+:	LayoutControl
					(	ButtonControl	"Cancel"						[ControlFunction (noLS (Cancel state))]
					:+:	ButtonControl	"OK"							[ControlId okId,ControlFunction (noLS Ok)]
					)	[ControlPos (Center,zero)]
					)
					[	WindowOk	okId
					,	WindowId	wId
					]
	where
		[wId,okId,editId:_]
				= ids
		
		Ok :: (PSt Tm) -> PSt Tm
		Ok pst=:{ls=tm=:{tmstate,tmids={windowID,machineMenuId,stepItemId,haltItemId}},io}
			# (dialog,io)	= getWindow wId io
			  dialog		= fromJust dialog
			  state			= FourCharString (fromJust (snd (getControlText editId dialog)))
			# io			= appWindowPicture windowID (ShowNextState state) io
			# io			= (if (state=="halt") disableMenuElements enableMenuElements) [stepItemId,haltItemId] io
			# pst			= {pst & io=io, ls={tm & tmstate={tmstate & turing={tmstate.turing & state=state}}}}
			# pst			= closeWindow wId pst
			= pst
		
		Cancel :: String (PSt Tm) -> PSt Tm
		Cancel state pst=:{ls={tmids={windowID}},io}
			# pst	= closeWindow wId pst
			# pst	= appPIO (appWindowPicture windowID (ShowNextState state)) pst
			= pst


//	The function to redraw the entire machine when an update event takes place.
ReDraw :: (PSt Tm) -> PSt Tm
ReDraw pst=:{ls=tm=:{tmstate={turing={transitions,tape,state}},tmids={tapeWdID,windowID}},io}
	# io	= setWindowLook tapeWdID True (True,\_ _->ShowTape tape)						io
	# io	= setWindowLook windowID True (True,\_ _->ShowTransitions transitions state)	io
	= {pst & io=io}


//	General alert dialog.
Alert :: String String (PSt Tm) -> PSt Tm
Alert mes1 mes2 pst
	= openNotice (Notice [mes1,mes2] (NoticeButton "OK" id) []) pst


//	Save before close dialog.
SaveBeforeClose :: String (IdFun (PSt Tm)) (PSt Tm) -> PSt Tm
SaveBeforeClose message action pst=:{ls=tm=:{name},io}
	= openNotice notice pst
where
	notice	= Notice
				[	"Save changes to \""+++RemovePath name+++"\""
				,	"before "+++message+++"?"
				]
				(	NoticeButton "Yes"    (noLS (action o SvBfClSave)))
				[	NoticeButton "No"     (noLS action)
				,	NoticeButton "Cancel" id
				]
	
	SvBfClSave :: (PSt Tm) -> PSt Tm
	SvBfClSave pst=:{ls=tm=:{tmstate={turing},name,tmids={saveItemId}},io}
		# pst	= {pst & ls={tm & saved=True}, io=disableMenuElements [saveItemId] io}
		= snd (WriteTuringToFile turing name pst)
