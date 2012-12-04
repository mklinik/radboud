module database


//	**************************************************************************************************
//
//	A database application that can read, generate, browse and edit *.dbs files.
//
//	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
//	
//	**************************************************************************************************


import StdEnv, StdIO, Notice

::	Record
	:==	Entry								// [Content]
::	Descriptor
	:== Entry								// [Fieldname]
::	Entry
	=	{	maxwidth	:: Int				// Max string width
		,	fields		:: [String]			// The entries
		}
::	DataBase
	=	{	records		:: [Record]			// All records
		,	descriptor	:: Descriptor		// All fieldnames
		,	selection	:: Int				// Indicating current record selected
		,	query		:: [String]			// Record to look for (its fields)
		,	name		:: String			// Name of database
		,	editinfoid	:: Id				// Id of info about use of edit dialog (query or record)
		,	edittextids	:: [Id]				// Ids of the edit text fields of the edit dialog
		,	separatorw	:: Int				// The width of the separator string
		,	dbfont		:: InfoFont			// The database font information
		,	dffont		:: InfoFont			// The dialog   font information
		}
::	InfoFont
	=	{	font		:: Font				// The font which is used
		,	width		:: Int				// Its widest character
		,	height		:: Int				// Its line height
		}

instance zero Entry where
	zero = {maxwidth=0,fields=[""]}

CharsInInputBox :== 20						// Input width (number of characters)

Replace			:== True					// Replace current selection when adding new record
Separator		:==	": "					// Separates field names and contents
CurrentRecordNr	:==	"Current Record Nr: "	// The text field that displays the current record nr

Start :: *World -> *World
Start world
	# ((dbInfo,dfInfo),world)	= accScreenPicture getInfoFont world
	# (sw,             world)	= accScreenPicture (getFontStringWidth dbInfo.font Separator) world
	# (recordWindowId, world)	= openId world				// Id of window that shows the records
	# (edDialogId,     world)	= openId world				// Id of window that edits a record
	# (fieldDialogId,  world)	= openId world				// Id of window that edits the set up
	# (editInfoId,     world)	= openId world				// Id of text that shows current record nr
	  initState					= {	records		= [zero]
								  ,	descriptor	= zero
								  ,	selection	= 0
								  ,	query		= [""]
								  ,	name		= "Untitled"
								  ,	editinfoid	= editInfoId
								  ,	edittextids	= []
								  ,	separatorw	= sw
								  ,	dbfont		= dbInfo
								  ,	dffont		= dfInfo
								  }
	= StartDataBase (recordWindowId,edDialogId,fieldDialogId) initState world
where
	getInfoFont :: *Picture -> ((InfoFont,InfoFont),*Picture)
	getInfoFont picture
		# ((_,dbFont),picture)	= openFont {NonProportionalFontDef & fSize=9} picture
		# (dbMetrics, picture)	= getFontMetrics dbFont picture
		# (dfFont,    picture)	= openDialogFont picture
		# (dfMetrics, picture)	= getFontMetrics dfFont picture
		= (	(	{ font=dbFont,width=dbMetrics.fMaxWidth,height=fontLineHeight dbMetrics }
			,	{ font=dfFont,width=dfMetrics.fMaxWidth,height=fontLineHeight dfMetrics }
			)
		  ,	picture
		  )

StartDataBase :: (Id,Id,Id) DataBase *World -> *World
StartDataBase ids=:(recordWindowId,edDialogId,fieldDialogId) initState world
	= startIO
			SDI												// Show one database a time
			initState										// The initial database state
			(seq [OpenMenu,ShowRecords,ShowEditDialog])		// The initialisation actions
			[ProcessOpenFiles OpenFiles,ProcessClose Quit]	// The process attributes
			world
where
//	The menu of the database:
	OpenMenu :: (PSt DataBase) -> PSt DataBase
	OpenMenu pState
		= snd (openMenu undef menu pState)
	where
		menu	= Menu "&Commands"
					(	MenuItem "Show &Records"		[MenuShortKey 'r', MenuFunction (noLS ShowRecords)]
					:+:	MenuItem "&Edit..."				[MenuShortKey 'e', MenuFunction (noLS ShowEditDialog)]
					:+:	MenuItem "Change Set &Up..."	[MenuShortKey 'u', MenuFunction (noLS ShowFieldDialog)]
					:+:	MenuItem "Read &New..."			[MenuShortKey 'o', MenuFunction (noLS (ReadNew Nothing))]
					:+:	MenuItem "&Save As..."			[MenuShortKey 's', MenuFunction (noLS SaveRecords)]
					:+:	MenuSeparator					[]
					:+:	MenuItem "&Quit"				[MenuShortKey 'q', MenuFunction (noLS Quit)]
					)	[]

//	The callback and initialisation functions of the menu:
	OpenFiles :: [String] (PSt DataBase) -> PSt DataBase
	OpenFiles dbnames database
		= ReadNew (if (isEmpty dbnames) Nothing (Just (hd dbnames))) database
	
	ReadNew :: (Maybe String) (PSt DataBase) -> PSt DataBase
	ReadNew maybe_dbname database
		= seq (map closeWindow [recordWindowId,edDialogId,fieldDialogId] ++ [ReadDataBase maybe_dbname,ShowRecords,ShowEditDialog]) database
	
	ReadDataBase :: (Maybe String) (PSt DataBase) -> PSt DataBase
	ReadDataBase maybe_dbname database=:{ls={dbfont={font}}}
		# (maybe_dbname,database)	= case maybe_dbname of
										(Just _)	-> (maybe_dbname,database)
										nothing		-> selectInputFile database
		| isNothing maybe_dbname
			= appPIO beep database
		# dbname					= fromJust maybe_dbname
		# (open,dbfile,database)	= fopen dbname FReadText database
		| not open
			= appPIO beep database
		# (descr,dbfile)			= FReadDescr dbfile
		# (recs, dbfile)			= FReadRecords (length descr+1) dbfile	// lines = length descr + empty line
		# recs						= if (isEmpty recs) [repeatn (length descr) ""] recs
		# (close,database)			= accFiles (fclose dbfile) database
		| not close
			= appPIO beep database
		| otherwise
			# (recs, database)		= accPIO (accScreenPicture (GetRecordEntries font recs)) database
			# (descr,database)		= accPIO (accScreenPicture (GetEntryFromFields font descr)) database
			= appPLoc (\db->{db & records   = recs
								, descriptor= descr
								, query     = repeatn (length descr.fields) ""
								, selection = 0
								, name      = dbname
							   }) database
	where
		FReadDescr file
			# (nroffields,file)		= FReadStrippedLine file
			# nroffields			= toInt nroffields
			| nroffields==0			= ([""],file)
			| otherwise				= seqList (repeatn nroffields FReadStrippedLine) file
		
		FReadRecords nroflines file
			| sfend file
				= ([], file)
			# ([_:record],file)		= seqList (repeatn nroflines FReadStrippedLine) file
			# (records,   file)		= FReadRecords nroflines file
			| otherwise
				= ([record : records], file)
		
		FReadStrippedLine file
			# (line,file)	= freadline file
			= (line%(0,size line - 2),file)		// strip "\n"
	
	ShowRecords :: (PSt DataBase) -> PSt DataBase
	ShowRecords database=:{ls=state=:{records,descriptor,name,dbfont}}
		# domain		= DbViewDomain state 0 (max (length records) 1)
		# (_,database)	= openWindow undef (window domain) database
		= database
	where
		window domain
			= Window namewithoutdirectories NilLS
					[	WindowId			recordWindowId
					,	WindowPos			(LeftTop,OffsetVector {vx=5,vy=5})
					,	WindowMouse			(const True) Able (noLS1 MouseSelectItem)
					,	WindowHScroll		(stdScrollFunction Horizontal dbfont.width)
					,	WindowVScroll		(stdScrollFunction Vertical   dbfont.height)
					,	WindowViewDomain	domain
					,	WindowLook			True (RecordWindowLook state)
					,	WindowPen			[PenBack (RGB {r=240,g=240,b=240})]
					]
		namewithoutdirectories
			= toString (last (splitby dirseparator (fromString name)))
	
	ShowEditDialog :: (PSt DataBase) -> PSt DataBase
	ShowEditDialog database=:{ls=state=:{descriptor=descr,records=recs,selection,dffont,editinfoid}}
		# (ids,database)	= accPIO (openIds nr_of_ids) database
		# database			= {database & ls={state & edittextids=tl ids}}
		# (_,database)		= openDialog undef (editDialog ids) database
		# database			= SetTextFields infostring (if (isEmpty recs) [] (recs!!selection).fields) database
		= database
	where
		nr_descrs			= length descr.fields
		nr_of_ids			= nr_descrs + 1		// Generate Ids for descriptor fields and default button ("Add")
		inputboxwidth		= CharsInInputBox*dffont.width
		infostring			= CurrentRecordNr+++toString selection
		editDialog ids
			= Dialog "Edit Record" 
					(	TextControl "" [ControlId editinfoid,ControlWidth (PixelWidth inputboxwidth)]
					:+:	LayoutControl
					(	ListLS
					[	TextControl field [ControlPos (Left,NoOffset)]
					\\	field <- descr.fields
					])	[ControlPos (Left,NoOffset)]
					:+:	LayoutControl
					(	ListLS
					[	EditControl "" (PixelWidth inputboxwidth) 1 [ControlId (ids!!i),ControlPos (Left,NoOffset)]
					\\	i <- [1..nr_descrs]
					])	[]
					
					:+:	ButtonControl "Dis&plQ"		[ ControlFunction (noLS DisplQuery),left,buttonwidth
													, ControlTip "Show current query fields."
													]
					:+:	ButtonControl "Set&Q"		[ ControlFunction (noLS SetQuery),buttonwidth
													, ControlTip "Set current input fields as new query."
													]
					:+:	ButtonControl "Searc&hQ" 	[ ControlFunction (noLS Search),buttonwidth
													, ControlTip "Search next item matching query."
													]
					:+:	ButtonControl "Se&lectAllQ"	[ ControlFunction (noLS SelectAll),buttonwidth
													, ControlTip "Set database content to all items matching query."
													]
					
					:+:	ButtonControl "&Replace"	[ ControlFunction (noLS (AddRecord Replace)),left,buttonwidth
													, ControlTip "Replace current selection with input fields."
													]
					:+:	ButtonControl "&Delete"		[ ControlFunction (noLS DeleteRecord),buttonwidth
													, ControlTip "Remove current selection from database."
													]
					:+:	ButtonControl "&Add"		[ ControlFunction (noLS (AddRecord (not Replace))),ControlId addId,buttonwidth
													, ControlTip "Extend database with new item matching input fields."
													]
					:+:	ButtonControl "&Sort"		[ ControlFunction (noLS Sort),buttonwidth
													, ControlTip "Sort database using first field."
													]
					
					:+:	ButtonControl "&Clear"		[ ControlFunction (noLS Clear),left,buttonwidth
													, ControlTip "Clear all input fields."
													]
					:+: ButtonControl "&Next/Prev"	[ ControlModsFunction (noLS1 NextPrev),buttonwidth
													, ControlTip "Select next or previous (press shift) database item."
													]
					)
					[	WindowId	edDialogId
					,	WindowOk	addId
					,	WindowClose	(noLS (closeWindow edDialogId))
					]
		where
			buttonwidth		= ControlWidth (ContentWidth "SelectAllQ")
			left			= ControlPos (Left,NoOffset)
			addId			= hd ids
	
	ShowFieldDialog :: (PSt DataBase) -> PSt DataBase
	ShowFieldDialog database=:{ls=state=:{descriptor=d,dffont}}
		| isEmpty d.fields
			# inputboxwidth	= CharsInInputBox*dffont.width
			= inputdialog "Give first field" inputboxwidth (\input->FieldChangeIO (add (-1) input)) database
		| otherwise
			# database		= closeWindow edDialogId database
			# (ids,database)= accPIO (openIds 2) database
			# (_,database)	= openDialog undef (fielddialog ids) database
			= database
	where
		fielddialog ids
			= Dialog "Change Set Up" 
					(	TextControl "Select Field..." []
					:+:	RadioControl (radioitems d.fields) (Columns 1) 1 [ControlId selectId]
					:+:	LayoutControl
					(	ButtonControl "&Delete"  [	left,buttonwidth
												 ,	ControlFunction (noLS (DeleteField fieldDialogId getselectedfield))
												 ,	ControlTip		"Delete selected field."
												 ]
					:+:	ButtonControl "&Rename"  [	left,buttonwidth
												 ,	ControlFunction (noLS (RenameField fieldDialogId getselectedfield))
												 ,	ControlTip		"Rename selected field."
												 ]
					)	[left]
					:+:	LayoutControl
					(	ButtonControl "&Move"    [	left,buttonwidth
												 ,	ControlFunction (noLS (MoveField fieldDialogId getselectedfield))
												 ,	ControlTip		"Move selected field to other position."
												 ]
					:+:	ButtonControl "Add &New" [	left,buttonwidth
												 ,	ControlId       addId
												 ,	ControlFunction (noLS (AddField fieldDialogId getselectedfield))
												 ,	ControlTip		"Add new field to records."
												 ]
					)	[]
					)
					[	WindowId	fieldDialogId
					,	WindowOk	addId
					,	WindowClose (noLS (closeWindow fieldDialogId))
					]
		where
			left		= ControlPos (Left,NoOffset)
			buttonwidth	= ControlWidth (ContentWidth "Add New")
			getselectedfield dialoginfo
				= fromJust (snd (getRadioControlSelection selectId dialoginfo)) - 1
			[addId,selectId:_]
				= ids
	
	SaveRecords :: (PSt DataBase) -> PSt DataBase
	SaveRecords database=:{ls=state=:{name,descriptor={fields=desc},records}}
		# (maybe_dbname,database)	= selectOutputFile "Save As: " name database
		| isNothing maybe_dbname
			= database
		# dbname					= fromJust maybe_dbname
		# (open,dbfile,database)	= fopen dbname FWriteText database
		| not open
			= appPIO beep database
		# (close,database)			= accFiles (fclose (seq (writedescriptor++writerecords) dbfile)) database
		| close
			= database
		| otherwise
			= appPIO beep database
	where
		writedescriptor		= [fwritei (length desc), FWriteRecord desc]
		writerecords		= [FWriteRecord rec.fields \\ rec <- records]
		FWriteRecord rec	= fwrites (foldl (+++) "\n" (map (\field -> field +++ "\n") rec))
	
	Quit :: (PSt DataBase) -> PSt DataBase
	Quit database
		= closeProcess (warn ["Save database before quitting?"] "&No" OK SaveRecords database)
	
	// Field set up changes
	
	FieldChangeIO :: (IdFun (PSt DataBase)) (PSt DataBase) -> PSt DataBase
	FieldChangeIO changefun database
		# database	= changefun database
		# database	= seq (map closeWindow [fieldDialogId,edDialogId]) database
		# database	= UpdateDbDomain database
		= database
	
	AddField :: Id (WState -> Int) (PSt DataBase) -> PSt DataBase
	AddField windowid getfield database=:{ls=state=:{dffont}}
		# (maybe_wstate,database)	= accPIO (getWindow windowid) database
		| isNothing maybe_wstate
			= appPIO beep database
		| otherwise
			# wstate				= fromJust maybe_wstate
			  fieldname				= getfield wstate
			  infotext				= "Add after '"+++state.descriptor.fields!!fieldname+++"' new field"
			  inputboxwidth			= CharsInInputBox*dffont.width
			= inputdialog infotext inputboxwidth (\input->FieldChangeIO (add fieldname input)) database
	
	RenameField :: Id (WState -> Int) (PSt DataBase) -> PSt DataBase
	RenameField windowid getfield database=:{ls=state=:{dffont}}
		# (maybe_wstate,database)	= accPIO (getWindow windowid) database
		| isNothing maybe_wstate
			= appPIO beep database
		| otherwise
			# wstate				= fromJust maybe_wstate
			  fieldtorename			= getfield wstate
			  infotext				= "Rename '"+++state.descriptor.fields!!fieldtorename+++"' to"
			  inputboxwidth			= CharsInInputBox*dffont.width
			= inputdialog infotext inputboxwidth (\input->FieldChangeIO (rename fieldtorename input)) database
	
	MoveField :: Id (WState -> Int) (PSt DataBase) -> PSt DataBase
	MoveField windowid getfield database=:{ls={descriptor=d}}
		# (maybe_wstate,database)	= accPIO (getWindow windowid) database
		| isNothing maybe_wstate
			= appPIO beep database
		| otherwise
			# wstate				= fromJust maybe_wstate
			  fieldtomove			= getfield wstate
			  (ids,database)		= accPIO (openIds 2) database
			= snd (openDialog undef (movedialog ids fieldtomove) database)
	where
		movedialog ids fieldtomove
			= Dialog "Move Field"
					(	TextControl   ("Move '" +++ d.fields!!fieldtomove +++ "' before: ") []
					:+:	RadioControl  (radioitems (d.fields++[""])) (Rows (length d.fields+1)) 1
												[ControlId selectId,        ControlPos      (Left,NoOffset)]
					:+:	ButtonControl Cancel	[ControlPos (Left,NoOffset),ControlFunction (noLS cancel)]
					:+:	ButtonControl "Move"	[ControlId okId,            ControlFunction (noLS (ok (move fieldtomove)))]
					)
					[	WindowOk okId	]
		where
			[okId, selectId:_]
				= ids
			
			ok mvf pState
				# (maybe_id,pState)				= accPIO getActiveWindow pState
				| isNothing maybe_id
					= pState
				| otherwise
					# id						= fromJust maybe_id
					# (Just dialoginfo,pState)	= accPIO (getWindow id) pState
					  destinationfield			= fromJust (snd (getRadioControlSelection selectId dialoginfo))-1
					= FieldChangeIO (mvf destinationfield) (closeWindow id pState)
	
	DeleteField :: Id (WState -> Int) (PSt DataBase) -> PSt DataBase
	DeleteField windowid getfield database
		# (maybe_wstate,database)	= accPIO (getWindow windowid) database
		| isNothing maybe_wstate
			= appPIO beep database
		| otherwise
			# wstate				= fromJust maybe_wstate
			= warn ["Are you sure?"] Cancel OK (FieldChangeIO (delete (getfield wstate))) database
	
	//	Handling the edit dialog
	
	DisplQuery :: (PSt DataBase) -> PSt DataBase
	DisplQuery database=:{ls={descriptor,query}}
		= SetTextFields "Query :" query database
	
	SetQuery :: (PSt DataBase) -> PSt DataBase
	SetQuery database=:{ls=state}
		# (nquery,database)	= GetTextFields database
		= {database & ls={state & query = nquery.fields}}
	
	Search :: (PSt DataBase) -> PSt DataBase
	Search database=:{ls=state=:{records,query,selection=sel,edittextids},io}
		| isEmpty found
			= appPIO beep database
		| otherwise
			# database	= ChangeSelection edittextids sel nsel {database & ls = {state & selection=nsel}}
			= MakeSelectionVisible database
	where
		nsel	= hd found
		found	= [i \\ e <- el ++ bl & i <- [sel+1 .. length records - 1] ++ [0..] | QueryRecord query e]
		(bl,el)	= splitAt (sel+1) records
	
	QueryRecord :: [String] Record -> Bool
	QueryRecord query {fields}
		= and [ EqPref qf f \\ f <- fields & qf <- query ]
	where
		EqPref pref name
			| size_pref > size_name	= False
			| otherwise				= pref == name%(0,size_pref - 1) || EqPref pref (name%(1,size_name - 1))
		where
			size_pref				= size pref
			size_name				= size name
	
	SelectAll :: (PSt DataBase) -> PSt DataBase
	SelectAll database=:{ls=state=:{records,query,selection,descriptor,dbfont,edittextids}}
		# recs				= filter (QueryRecord query) records
		| isEmpty recs
			= appPIO beep database
		| otherwise
			# database		= appPIO (setWindowTitle recordWindowId "Select") database
			  state			= {state & selection=0,records=recs,name="Select"}
			# database		= {database & ls=state}
			# database		= ChangeSelection edittextids selection 0 database
			# database		= UpdateDbDomain database
			= database
	
	MakeSelectionVisible :: (PSt DataBase) -> PSt DataBase
	MakeSelectionVisible database=:{ls=state=:{records,selection,descriptor,dbfont},io}
		| isEmpty records
			= database
		# (viewframe,io)	= getWindowViewFrame recordWindowId io
		  visibletop		= viewframe.corner1.y
		  visiblebot		= viewframe.corner2.y
		| selthumb >= visibletop && selthumb < visiblebot		// selection visible
			= {database & io=io}
		| otherwise												// selection invisible
			= {database & io=moveWindowViewFrame recordWindowId {vx=0,vy=selthumb-visibletop} io}
	where
		selthumb			= toPicCo dbfont descriptor selection
	
	DeleteRecord :: (PSt DataBase) -> PSt DataBase
	DeleteRecord database=:{ls=state=:{records=oldrecs,selection=oldindex}}
		| isEmpty oldrecs
			= appPIO beep database
		| otherwise
			= UpdateDbDomain {database & ls={state & records = newrecs, selection = newindex}}
	where
		newrecs	= removeAt oldindex oldrecs
		newindex= if (isEmpty newrecs) 0 (oldindex rem length newrecs) 
	
	AddRecord :: Bool (PSt DataBase) -> PSt DataBase
	AddRecord replace database=:{ls=state=:{selection,records=recs,dbfont},io}
		| isEmpty recs && replace
			= appPIO beep database
		| otherwise
			# (newrec,database)	= GetTextFields database
			  (index,newrecs)	= insertindex (<=) newrec (if replace (removeAt selection recs) recs)
			  nstate			= {state & records=newrecs,selection=index}
			= UpdateDbDomain {database & ls=nstate}
	
	Sort :: (PSt DataBase) -> PSt DataBase
	Sort database=:{ls=state=:{records=recs}}
		= UpdateDbDomain {database & ls={state & records = sort recs}}
	
	Clear :: (PSt DataBase) -> PSt DataBase
	Clear database=:{ls={editinfoid,edittextids}}
		= appPIO (setControlTexts [(id,"") \\ id<-edittextids]) database
	
	NextPrev :: Modifiers (PSt DataBase) -> PSt DataBase
	NextPrev {shiftDown} database=:{ls=state=:{records,selection=sel,edittextids}}
		# database	= ChangeSelection edittextids sel nsel {database & ls = {state & selection=nsel}}
		= MakeSelectionVisible database
	where
		max_i	= length records - 1
		nsel	= if shiftDown
				 (if (sel==0) max_i (sel-1))
				 (if (sel==max_i) 0 (sel+1))
	
	GetTextFields :: (PSt DataBase) -> (Entry,PSt DataBase)
	GetTextFields database=:{ls={edittextids,dbfont}}
		# (Just dialog,database)	= accPIO (getWindow edDialogId) database
		  fields					= [text \\ (_,Just text) <- getControlTexts edittextids dialog]
		# (widths,database)			= accPIO (accScreenPicture (getFontStringWidths dbfont.font fields)) database
		= ({maxwidth=maxList widths,fields=fields},database)
	
	SetTextFields :: String [String] (PSt DataBase) -> PSt DataBase
	SetTextFields s rec database=:{ls={editinfoid,edittextids}}
		= appPIO (setControlTexts (zip2 [editinfoid:edittextids] [s:rec])) database
	
	//	Handling mouse clicks in database window
	
	MouseSelectItem	:: MouseState (PSt DataBase) -> PSt DataBase
	MouseSelectItem (MouseDown {y} _ _) database=:{ls=state=:{records,descriptor,selection,dbfont,edittextids},io}
		| isEmpty records
			= database
		| otherwise
			# (Just viewDomain,database)	= accPIO (getWindowViewDomain recordWindowId) database
			# index							= toRecCo dbfont descriptor (min y (viewDomain.corner2.y-1))
			# nstate						= {state & selection=index}
			= ChangeSelection edittextids selection index {database & ls=nstate}
	MouseSelectItem _ database
		= database
	
	//	Update the whole window in case the ViewDomain has changed
	
	UpdateDbDomain :: (PSt DataBase) -> PSt DataBase
	UpdateDbDomain database=:{ls=state}
		# viewdomain= DbViewDomain state 0 (max (length state.records) 1)
		# database	= appPIO (setWindowLook recordWindowId True (True,RecordWindowLook state)) database
		# database	= appPIO (setWindowViewDomain recordWindowId viewdomain) database
		# database	= MakeSelectionVisible database
		= database

	ChangeSelection :: [Id] Int Int (PSt DataBase) -> PSt DataBase
	ChangeSelection edittextids old new database=:{ls=state=:{descriptor=descr,records},io}
		# io		= appWindowPicture recordWindowId (HiliteSelection state new o (HiliteSelection state old)) io
		# io		= setWindowLook recordWindowId False (True,RecordWindowLook state) io
		= SetTextFields infostring (records!!new).fields {database & io=io}
	where
		infostring	= CurrentRecordNr+++toString new

//	Functions that change the content of particular fields

add afterfield fieldname database=:{ls=state=:{records=rs,descriptor=d,query=q,dbfont}}
	# (w,database)= accPIO (accScreenPicture (getFontStringWidth dbfont.font fieldname)) database
	= {database & ls={state & records    = map (\r->{r & fields=ins afterfield "" r.fields}) rs
							, descriptor = {d  & fields=ins afterfield fieldname d.fields,maxwidth=max w d.maxwidth}
							, query      = ins afterfield "" q
					 }
	  }

ins afterfield x ys   = insertAt (afterfield+1) x ys

rename selectedfield newfieldname database=:{ls=state=:{descriptor=d,dbfont}}
	# (w,database)= accPIO (accScreenPicture (getFontStringWidth dbfont.font newfieldname)) database
	= {database & ls={state & descriptor={d & maxwidth=max w d.maxwidth,fields=newfields}}}
where
	newfields = updateAt selectedfield newfieldname d.fields
	
move sf df database=:{ls=state=:{records=rs,descriptor=d,query=q}}
	= {database & ls={state & records    = map (\r=:{fields}->{r & fields=moveinlist sf df fields}) rs
							, descriptor = {d & fields=moveinlist sf df d.fields}
							, query      = moveinlist sf df q
					 }
	  }

delete i database=:{ls=state=:{records,descriptor={fields=d},query=q,dbfont}}
	# (recs,database)	= accPIO (accScreenPicture (GetRecordEntries dbfont.font [removeAt i r.fields \\ r<-records])) database
	# (desc,database)	= accPIO (accScreenPicture (GetEntryFromFields dbfont.font (removeAt i d))) database
	= {database & ls={state & records    = recs
							, descriptor = desc
							, query      = removeAt i q
					   }
	  }

//	Drawing utilities

DbViewDomain :: DataBase Int Int -> ViewDomain
DbViewDomain {descriptor,records,dbfont,separatorw} fr to
	= { corner1 = {x = ~whiteMargin                      ,y = toPicCo dbfont descriptor fr }
	  , corner2 = {x = dw + separatorw + fw + whiteMargin,y = toPicCo dbfont descriptor to }
	  }
where
	whiteMargin	= dbfont.width
	dw			= descriptor.maxwidth
	fw			= maxList [rec.maxwidth \\ rec<-records]

RecordWindowLook :: DataBase SelectState UpdateState *Picture -> *Picture
RecordWindowLook state _ {updArea=domains} picture
	= appClipPicture (toRegion domains) (RecordWindowLook` state domains) (seq (map unfill domains) picture)
where
	RecordWindowLook` :: DataBase [Rectangle] *Picture -> *Picture
	RecordWindowLook` state=:{records=recs,descriptor=descr,selection,dbfont} domains picture
		# picture	= setPenFont dbfont.font picture
		# picture	= seq (map Update domains) picture
		# picture	= HiliteSelection state selection picture
		= picture
	where
		Update :: Rectangle *Picture -> *Picture
		Update domain=:{corner1={y=top},corner2={y=bottom}} picture
		//	#picture	= unfill domain picture
			| isEmpty recs
				= picture
			| otherwise
				# picture	= setPenPos {x=0,y=topofvisiblerecs} picture
				# picture	= seq (map (DrawRec descr) (recs%(toprec,botrec))) picture
				= picture
		where
			topofvisiblerecs= toPicCo dbfont descr toprec
			toprec			= toRecCo dbfont descr top
			botrec			= toRecCo dbfont descr (bottom-1)
		
		DrawRec :: Descriptor Record *Picture -> *Picture
		DrawRec {fields=descr} {fields=rec} picture
			# picture	= drawLine "" picture
			# picture	= seq [drawLine (d +++ Separator +++ f) \\ d<-normwidth descr & f<-rec] picture
			= picture
		where
			normwidth descr = [f +++ toString (spaces ((maxList (map (size ) descr)) - size f)) \\ f <- descr]
			drawLine s picture
				# (curPenPos,picture)	= getPenPos picture
				# picture				= draw s picture
				# picture				= setPenPos {curPenPos & y=curPenPos.y+dbfont.height} picture
				= picture

HiliteSelection :: DataBase Int *Picture -> *Picture
HiliteSelection s i pict
	= hilite (DbViewDomain s i (i+1)) pict

//	Switching between picture coordinates and indices in the list of records ('record coordinates')

toPicCo:: InfoFont Descriptor Int -> Int
toPicCo dbfont descr n = n * (inc (length descr.fields) * dbfont.height)

toRecCo:: InfoFont Descriptor Int -> Int
toRecCo dbfont descr n = n / (inc (length descr.fields) * dbfont.height)

// Various useful functions

instance < Size where
	(<) :: !Size !Size -> Bool
	(<) {w=w1,h=h1} {w=w2,h=h2} = w1<w2 && h1<h2

instance < Entry where
	(<) :: !Entry !Entry -> Bool
	(<) {fields=f1} {fields=f2} = f1<f2

radioitems titles
	= [(t,Nothing,id) \\ t <- titles]

GetEntryFromFields :: Font [String] *Picture -> (Entry,*Picture)
GetEntryFromFields font fields picture
	# (widths,picture)	= getFontStringWidths font fields picture
	= ({maxwidth=maxList widths,fields=fields},picture)

GetRecordEntries :: Font [[String]] *Picture -> ([Entry],*Picture)
GetRecordEntries font recs picture
	# picture			= setPenFont font picture
	# (widths,picture)	= seqList (map getPenFontStringWidths recs) picture
	= ([{maxwidth=maxList ws,fields=fs} \\ ws<-widths & fs<-recs],picture)


// Functions that should be library functions

Cancel		:==	"&Cancel"
OK			:==	"&OK"

inputdialog name width fun pState
	# (ids,pState)	= accPIO (openIds 2) pState
	# (_,pState)	= openDialog undef (dialogdef ids) pState
	= pState
where
	dialogdef ids
		= Dialog name
				(	TextControl  (name+++": ")           []
				:+: EditControl  "" (PixelWidth width) 1 [ControlId inputId]
				:+:	ButtonControl Cancel                 [ControlPos (BelowPrev,NoOffset),ControlFunction (noLS cancel)]
				:+:	ButtonControl OK                     [ControlId okId,                 ControlFunction (noLS (ok fun))]
				)
				[	WindowOk	okId
				]
	where
		[inputId,okId:_]
			= ids
		
		ok fun pState
			# (Just id,    pState)	= accPIO (getParentId inputId) pState
			# (Just dialog,pState)	= accPIO (getWindow id) pState
			  input					= fromJust (snd (getControlText inputId dialog))
			= fun input (closeWindow id pState)

warn info canceltext oktext fun pState
	= openNotice (Notice info (NoticeButton oktext (noLS (fun o cancel))) [NoticeButton canceltext id]) pState

cancel pState
	# (maybeId,pState)	= accPIO getActiveWindow pState
	| isNothing maybeId = pState
	# id				= fromJust maybeId
	= closeWindow id pState

insertindex :: !(a -> a -> Bool) !a !u:[a] -> (!Int,!v:[a]), [u <= v]
insertindex r x ls
	= inserti r 0 x ls
where
	inserti :: !(a -> .(a -> .Bool)) !Int !a !u:[a] -> (!Int,!v:[a]), [u <= v]
	inserti r i x ls=:[y:ys]
		| r x y
			= (i,[x:ls])
		| otherwise
			# (index,list) = inserti r (inc i) x ys
			= (index,[y:list])
	inserti _ i x _
		= (i,[x])

moveinlist :: !Int !Int !.[a] -> [a]
moveinlist src dest l				// should be in StdList
	| src < dest
		= removeAt src beforedest ++ [l!!src : atdest]
	| src > dest
		= beforedest ++ [l!!src : removeAt (src - dest) atdest]
	| otherwise
		= l
where
	(beforedest,atdest)	= splitAt dest l	

splitby :: a !.[a] -> [.[a]] | Eq a
splitby x ys
	= case rest of [] -> [firstpart]; [r:rs] -> [firstpart:splitby x rs]
where
	(firstpart,rest)	= span ((<>) x) ys
