definition module ListBox


/*	Definition of the list box control.
	This definition has been written in Clean 2.0 and uses the Object I/O library, version 1.2.2
	The list box control is constructed out of predefined control elements, and is therefore platform independent.
	In future versions it will be added as a standard library component.
*/


import StdControl, StdControlClass, StdId, StdPSt


::	ListBoxControl ls ps
	=	{	listboxState	:: ListBoxState
		,	listboxAtts		:: [ControlAttribute *(ls,ps)]
		}
::	ListBoxState
::	ListBoxId

/*	ListBoxControl windowid max items selection id attributes
	creates a list box definition that can be used within windows and dialogues.
	Due to limitations of the I/O library, the maximum number of items that can be displayed is fixed.
	The max        argument is the maximum number of items that the listbox can contain (will be removed in future versions).
	The items      argument are the initial items (only the first max items are considered).
	The selection  argument is the initial selection which may be empty (indices range from 1 upto #items).
	The id         argument must be a ListBoxId.
	The attributes argument can be any of the following ControlAttributes (all other attributes are ignored):
		*	ControlFunction		f:				this function is applied at each MouseDown action
		*	ControlHide:						the listbox is initially hidden
		*	ControlPos			itemPos:		the layout position of the listbox
		*	ControlSelectState	selectState:	the initial SelectState of the listbox
		*	ControlSize			size:			the initial size of the listbox
	The environment argument must atleast be a FontEnv.
*/
ListBoxControl			:: Int [String] [Index] ListBoxId [ControlAttribute *(.ls,.ps)] !*env
						-> (!.ListBoxControl .ls .ps,!*env) | accScreenPicture env

instance Controls ListBoxControl

openListBoxId			:: !*env -> (!ListBoxId,!*env)	| Ids env

getListBoxSelection 	:: !ListBoxId					!(PSt *l) -> (!(!Bool,![(String,!Index)]),!PSt *l)
setListBoxSelection 	:: !ListBoxId ![Index]			!(PSt *l) -> PSt *l
getListBoxItems			:: !ListBoxId					!(PSt *l) -> (!(!Bool,![String]),!PSt *l)
openListBoxItems		:: !ListBoxId !Index ![String]	!(PSt *l) -> PSt *l
closeListBoxItems		:: !ListBoxId ![Index]			!(PSt *l) -> PSt *l

showListBoxControl		:: !ListBoxId !(IOSt .l) -> IOSt .l
hideListBoxControl		:: !ListBoxId !(IOSt .l) -> IOSt .l
enableListBoxControl	:: !ListBoxId !(IOSt .l) -> IOSt .l
disableListBoxControl	:: !ListBoxId !(IOSt .l) -> IOSt .l
