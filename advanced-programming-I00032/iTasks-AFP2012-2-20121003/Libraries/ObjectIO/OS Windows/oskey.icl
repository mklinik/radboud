implementation module oskey


import	StdBool, StdClass, StdInt, StdOverloaded, StdString


::	SpecialKey
	=	{	virtual	:: !Int
		}

BackSpaceVirtualCode:==   8		// BackSpace
BeginVirtualCode	:== 115		// Begin of text
ClearVirtualCode	:==  71		// Clear
DeleteVirtualCode	:== 117		// Delete
DownVirtualCode		:== 125		// Arrow down
EndVirtualCode		:== 119		// End of text
EnterVirtualCode	:==  13		// Enter
EscapeVirtualCode	:==  53		// Escape
F1VirtualCode		:== 122		// Function 1
F2VirtualCode		:== 120		// Function 2
F3VirtualCode		:==  99		// Function 3
F4VirtualCode		:== 118		// Function 4
F5VirtualCode		:==  96		// Function 5
F6VirtualCode		:==  97		// Function 6
F7VirtualCode		:==  98		// Function 7
F8VirtualCode		:== 100		// Function 8
F9VirtualCode		:== 101		// Function 9
F10VirtualCode		:== 109		// Function 10
F11VirtualCode		:== 103		// Function 11
F12VirtualCode		:== 111		// Function 12
F13VirtualCode		:== 105		// Function 13
F14VirtualCode		:== 107		// Function 14
F15VirtualCode		:== 113		// Function 15
HelpVirtualCode		:== 114		// Help
LeftVirtualCode		:== 123		// Arrow left
PgDownVirtualCode	:== 121		// Page down
PgUpVirtualCode		:== 116		// Page up
ReturnVirtualCode	:==  -1		// Return (dummy under Windows)
RightVirtualCode	:== 124		// Arrow right
UpVirtualCode		:== 126		// Arrow up

instance == SpecialKey where
	(==) {virtual=v1} {virtual=v2} = v1==v2

instance toString SpecialKey where
	toString {virtual}
		= specialKeyCodeName virtual
	where
		specialKeyCodeName :: !Int -> {#Char}
		specialKeyCodeName BackSpaceVirtualCode	= "BackSpaceKey"
		specialKeyCodeName BeginVirtualCode		= "BeginKey"
		specialKeyCodeName ClearVirtualCode		= "ClearKey"
		specialKeyCodeName DeleteVirtualCode	= "DeleteKey"
		specialKeyCodeName DownVirtualCode		= "DownKey"
		specialKeyCodeName EndVirtualCode		= "EndKey"
		specialKeyCodeName EnterVirtualCode		= "EnterKey"
		specialKeyCodeName EscapeVirtualCode	= "EscapeKey"
		specialKeyCodeName F1VirtualCode		= "F1Key"
		specialKeyCodeName F2VirtualCode		= "F2Key"
		specialKeyCodeName F3VirtualCode		= "F3Key"
		specialKeyCodeName F4VirtualCode		= "F4Key"
		specialKeyCodeName F5VirtualCode		= "F5Key"
		specialKeyCodeName F6VirtualCode		= "F6Key"
		specialKeyCodeName F7VirtualCode		= "F7Key"
		specialKeyCodeName F8VirtualCode		= "F8Key"
		specialKeyCodeName F9VirtualCode		= "F9Key"
		specialKeyCodeName F10VirtualCode		= "F10Key"
		specialKeyCodeName F11VirtualCode		= "F11Key"
		specialKeyCodeName F12VirtualCode		= "F12Key"
		specialKeyCodeName F13VirtualCode		= "F13Key"
		specialKeyCodeName F14VirtualCode		= "F14Key"
		specialKeyCodeName F15VirtualCode		= "F15Key"
		specialKeyCodeName HelpVirtualCode		= "HelpKey"
		specialKeyCodeName LeftVirtualCode		= "LeftKey"
		specialKeyCodeName PgDownVirtualCode	= "PgDownKey"
		specialKeyCodeName PgUpVirtualCode		= "PgUpKey"
		specialKeyCodeName ReturnVirtualCode	= "ReturnKey"
		specialKeyCodeName RightVirtualCode		= "RightKey"
		specialKeyCodeName UpVirtualCode		= "UpKey"
		specialKeyCodeName otherCode			= "toSpecialKey "+++toString otherCode

backSpaceKey:: SpecialKey;			backSpaceKey= {virtual=BackSpaceVirtualCode}// BackSpace
beginKey	:: SpecialKey;			beginKey	= {virtual=BeginVirtualCode}	// Begin of text
clearKey	:: SpecialKey;			clearKey	= {virtual=ClearVirtualCode}	// Clear
deleteKey	:: SpecialKey;			deleteKey	= {virtual=DeleteVirtualCode}	// Delete
downKey		:: SpecialKey;			downKey		= {virtual=DownVirtualCode}		// Arrow down
endKey		:: SpecialKey;			endKey		= {virtual=EndVirtualCode}		// End of text
enterKey	:: SpecialKey;			enterKey	= {virtual=EnterVirtualCode}	// Enter
escapeKey	:: SpecialKey;			escapeKey	= {virtual=EscapeVirtualCode}	// Escape
f1Key		:: SpecialKey;			f1Key		= {virtual=F1VirtualCode}		// Function 1
f2Key		:: SpecialKey;			f2Key		= {virtual=F2VirtualCode}		// Function 2
f3Key		:: SpecialKey;			f3Key		= {virtual=F3VirtualCode}		// Function 3
f4Key		:: SpecialKey;			f4Key		= {virtual=F4VirtualCode}		// Function 4
f5Key		:: SpecialKey;			f5Key		= {virtual=F5VirtualCode}		// Function 5
f6Key		:: SpecialKey;			f6Key		= {virtual=F6VirtualCode}		// Function 6
f7Key		:: SpecialKey;			f7Key		= {virtual=F7VirtualCode}		// Function 7
f8Key		:: SpecialKey;			f8Key		= {virtual=F8VirtualCode}		// Function 8
f9Key		:: SpecialKey;			f9Key		= {virtual=F9VirtualCode}		// Function 9
f10Key		:: SpecialKey;			f10Key		= {virtual=F10VirtualCode}		// Function 10
f11Key		:: SpecialKey;			f11Key		= {virtual=F11VirtualCode}		// Function 11
f12Key		:: SpecialKey;			f12Key		= {virtual=F12VirtualCode}		// Function 12
f13Key		:: SpecialKey;			f13Key		= {virtual=F13VirtualCode}		// Function 13
f14Key		:: SpecialKey;			f14Key		= {virtual=F14VirtualCode}		// Function 14
f15Key		:: SpecialKey;			f15Key		= {virtual=F15VirtualCode}		// Function 15
helpKey		:: SpecialKey;			helpKey		= {virtual=HelpVirtualCode}		// Help
leftKey		:: SpecialKey;			leftKey		= {virtual=LeftVirtualCode}		// Arrow left
pgDownKey	:: SpecialKey;			pgDownKey	= {virtual=PgDownVirtualCode}	// Page down
pgUpKey		:: SpecialKey;			pgUpKey		= {virtual=PgUpVirtualCode}		// Page up
returnKey	:: SpecialKey;			returnKey	= {virtual=ReturnVirtualCode}	// Return
rightKey	:: SpecialKey;			rightKey	= {virtual=RightVirtualCode}	// Arrow right
upKey		:: SpecialKey;			upKey		= {virtual=UpVirtualCode}		// Arrow up

toSpecialKey :: !Int -> SpecialKey
toSpecialKey specialkey = {virtual=specialkey}

isSpecialKey:: !Int -> Bool
isSpecialKey specialKey
	= containsSorted specialKey virtualKeyCodes
where
	containsSorted :: !Int ![Int] -> Bool
	containsSorted x [y:ys]
		| x>y		= containsSorted x ys
		| otherwise	= x==y
	containsSorted _ _
		= False

virtualKeyCodes :: [Int]						// The < sorted list of virtual key codes
virtualKeyCodes	=:	[	BackSpaceVirtualCode	//   8
					,	EnterVirtualCode		//  13
					,	EscapeVirtualCode		//  53
					,	ClearVirtualCode		//  71
					,	F5VirtualCode			//  96
					,	F6VirtualCode			//  97
					,	F7VirtualCode			//  98
					,	F3VirtualCode			//  99
					,	F8VirtualCode			// 100
					,	F9VirtualCode			// 101
					,	F11VirtualCode			// 103
					,	F13VirtualCode			// 105
					,	F14VirtualCode			// 107
					,	F10VirtualCode			// 109
					,	F12VirtualCode			// 111
					,	F15VirtualCode			// 113
					,	HelpVirtualCode			// 114
					,	BeginVirtualCode		// 115
					,	PgUpVirtualCode			// 116
					,	DeleteVirtualCode		// 117
					,	F4VirtualCode			// 118
					,	EndVirtualCode			// 119
					,	F2VirtualCode			// 120
					,	PgDownVirtualCode		// 121
					,	F1VirtualCode			// 122
					,	LeftVirtualCode			// 123
					,	RightVirtualCode		// 124
					,	DownVirtualCode			// 125
					,	UpVirtualCode			// 126
					]
