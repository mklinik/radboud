definition module windowcontrols

//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Operations to open, close, and reposition controls in windows.
//	********************************************************************************

import	windowhandle
import	osdocumentinterface, ossystem, ostoolbox


opencontrols		:: !OSWindowMetrics .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox
																	-> (!WindowStateHandle .pst, !*OSToolbox)
/*	opencontrols adds the given controls to the given window.
	It is assumed that no Id conflicts can arise with the current set of controls.
*/

opencompoundcontrols:: !OSDInfo !OSWindowMetrics !Id .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox
																		  -> (!Bool, !WindowStateHandle .pst, !*OSToolbox)
/*	opencompoundcontrols adds the given controls to the compound control of the given window.
	It is assumed that no Id conflicts can arise with the current set of controls.
	The Boolean result is True iff the compound control could be found.
*/

openrecursivecontrols :: !OSDInfo !OSWindowMetrics !Id .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox
																			-> (!Bool, !WindowStateHandle .pst, !*OSToolbox)
/*	openrecursivecontrols adds the given controls to the indicated (Compound/Layout)Control of the given window.
	It is assumed that no Id conflicts can arise with the current set of controls.
	The Boolean result is True iff the (Compound/Layout)Control could be found.
*/

closecontrols		:: !OSWindowMetrics ![Id] !Bool !(WindowStateHandle .pst) !*OSToolbox
				   -> (![Id],![Id],!IdFun *OSToolbox,!WindowStateHandle .pst, !*OSToolbox)
/*	closecontrols removes the indicated controls from the window.
	The first result [Id] are the R(2)Ids of receiver(2)s that have been removed.
	The second result [Id] are the Ids of the other controls that have been removed.
	The controls are only hidden, not removed. To do this, the (IdFun *OSToolbox) function must be applied.
	If the Boolean argument is True then the layout of the remaining controls will be recalculated.
*/

closeallcontrols	:: !(WindowStateHandle .pst) !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,!WindowStateHandle .pst,!*OSToolbox)
/*	closeallcontrols removes all controls from the window.
	The first result [Id] are the R(2)Ids of receiver(2)s that have been removed.
	The second result [Id] are the Ids of the other controls that have been removed.
	The controls are only hidden, not removed. To do this, the (IdFun *OSToolbox) function must be applied.
*/

setcontrolpositions	:: !OSWindowMetrics ![(Id,ItemPos)] !(WindowStateHandle .pst) !*OSToolbox
											   -> (!Bool,!WindowStateHandle .pst, !*OSToolbox)
/*	setcontrolpositions sets the positions of the indicated controls to their new positions.
	The Boolean result is True iff all controls could be found and their new positions are legal. 
	It is assumed that the argument WindowStateHandle is either a Window or a Dialog.
*/
