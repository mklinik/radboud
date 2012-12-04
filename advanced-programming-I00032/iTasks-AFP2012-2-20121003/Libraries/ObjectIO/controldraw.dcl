definition module controldraw


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Drawing operations on customised controls.
//	********************************************************************************


import	ossystem, ostypes
import	wstate


/*	draw(Compound/Custom(Button))Look(`) render their control using the current look.
	Drawing is clipped inside the argument OSRect (in window coordinates).
	It is assumed that they are applied to a WItemHandle(`) argument that refers to a (Compound/CustomButton/Custom)Control respectively.
	drawCompoundLook(`) furthermore assumes that the compound control's ClipState is valid.
*/
drawCompoundLook		:: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCompoundLook`		:: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !OSRect ! WItemHandle`          !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)
drawCustomButtonLook	::                  !Bool !OSWindowPtr !Point2 !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomButtonLook`	::                  !Bool !OSWindowPtr !Point2 !OSRect ! WItemHandle`          !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)
drawCustomLook			::                  !Bool !OSWindowPtr !Point2 !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomLook`			::                  !Bool !OSWindowPtr !Point2 !OSRect ! WItemHandle`          !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)

/*	drawIn(Compound/Custom(Button))(`) apply a Picture access function to the given control Picture.
	Drawing is clipped inside the argument OSRect.
	It is assumed that they are applied to a WItemHandle(`) argument that refers to a (Compound/CustomButton/Custom)Control respectively.
	drawInCompound(`) furthermore assumes that the compound control is non-transparent and has a valid ClipState.
*/
drawInCompound			:: !.(St *Picture .x) !OSWindowPtr !Point2 !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCompound`			:: !.(St *Picture .x) !OSWindowPtr !Point2 !OSRect ! WItemHandle`          !*OSToolbox -> (.x,!WItemHandle`,        !*OSToolbox)
drawInCustomButton		:: !.(St *Picture .x) !OSWindowPtr !Point2 !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCustomButton`		:: !.(St *Picture .x) !OSWindowPtr !Point2 !OSRect ! WItemHandle`          !*OSToolbox -> (.x,!WItemHandle`,        !*OSToolbox)
drawInCustom			:: !.(St *Picture .x) !OSWindowPtr !Point2 !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCustom`			:: !.(St *Picture .x) !OSWindowPtr !Point2 !OSRect ! WItemHandle`          !*OSToolbox -> (.x,!WItemHandle`,        !*OSToolbox)
