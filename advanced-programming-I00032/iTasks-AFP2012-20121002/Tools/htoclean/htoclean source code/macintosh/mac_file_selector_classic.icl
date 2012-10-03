implementation module mac_file_selector_classic;

import StdClass,StdBool,StdChar,StdInt,StdMisc,StdString, StdFile,StdArray;
import	standard_file,files,pointer,quickdraw;

SelectorWidth  :== 350;
SelectorHeight :== 250;

Error_i	:: !String !Int -> .x;
Error_i string i = abort (string +++ toString i);

Get_parent_id_of_file :: !Int !String !*Toolbox -> (!Int, !*Toolbox);
Get_parent_id_of_file volumeNumber fileName tb
|	0 == osError	= (parentId, tb1);
					= Error_i "Error code returned by GetCatInfo: " osError;
	where {
		(osError,parentId,tb1) = GetCatInfo1 volumeNumber fileName tb;
	};

Get_working_directory_info :: !Int !*Toolbox -> (!Int,!Int,!*Toolbox);
Get_working_directory_info workingDirectoryId tb
|	osError == 0	= (volumeNumber,directoryId,tb1);
					= Error_i "Error code returned by GetWDInfo: " osError;
	where {
		(osError,volumeNumber,directoryId,tb1) = GetWDInfo workingDirectoryId tb;
	};

Get_name_and_parent_id_of_directory :: !Int !Int !*Toolbox -> (!String,!Int,!*Toolbox);
Get_name_and_parent_id_of_directory volumeNumber directoryId tb
	| osError==0
		= (folderName,parentId,tb1);
		= Error_i "Error code returned by BPGetCatInfo: " osError;
	where {
		(osError,folderName,parentId,tb1) = GetCatInfo2 volumeNumber directoryId (createArray 64 '\0') tb;
	};

Get_directory_path :: !Int !Int !String !*Toolbox -> (!String, !*Toolbox);
Get_directory_path volumeNumber directoryId path tb
	| directoryId==2
		= (folderName +++ ":" +++ path, tb1);
		= Get_directory_path volumeNumber parentId (folderName +++ ":" +++ path) tb1;
	where {
		(folderName, parentId, tb1) = Get_name_and_parent_id_of_directory volumeNumber directoryId tb;
	};

do_tb_io :: !*Toolbox !*World -> *World;
do_tb_io _ w = w;

SelectInputFile	:: !*World -> (!Bool,!String,!*World);
SelectInputFile w
	# tb					= 0;
	  (selectorPos, tb1)	= SelectorPosition tb;
	  (good,copy,fType,vRefNum,version,fName,tb2)
							= SFGetFile selectorPos "" 0 (-1) "" 0 (createArray 64 '\0') tb1;
	| good
		# (directoryId,tb3)	= Get_parent_id_of_file vRefNum fName tb2;
		  (pathName,	 tb4)	= Get_directory_path vRefNum directoryId fName tb3;
		= (True,  pathName,	do_tb_io tb4 w);
		= (False, "",		do_tb_io tb2 w);

Find_last_colon :: !String !Int -> (!Bool,!Int);
Find_last_colon s p = Find_last_colon2 s p (dec (size s));

Find_last_colon2 :: String !Int !Int -> (!Bool,!Int);
Find_last_colon2 s p l
|	p >= l				= (False,p);
|	s.[p] == ':'	= (True,p);
						= Find_last_colon2 s (inc p) l;

Get_directory_and_file_name :: !String !*Toolbox -> (!Int,!Int,!String,!*Toolbox);
Get_directory_and_file_name pathName tb
|	not colon					= (sfSaveDisk, curDirStore, pathName,tb1);
|	pathName.[0] == ':'		= Get_directory_and_file_name2 pathName 0 sfSaveDisk curDirStore tb1;
|	0 == result					= Get_directory_and_file_name2 pathName colonPosition volumeNumber 2 tb`;
								= (sfSaveDisk`,curDirStore`,pathName,tb``);
	where {
		(colon,colonPosition)			= Find_last_colon pathName 0;
		(sfSaveDisk, curDirStore, tb1)	= Get_stored_dir_and_file tb;
		(result,volumeNumber,tb`)		= GetVInfo (pathName % (0, colonPosition)) tb;
		(sfSaveDisk`,curDirStore`,tb``)	= Get_stored_dir_and_file tb`;
	};

Get_stored_dir_and_file :: !*Toolbox -> (!Int,!Int,!*Toolbox);
Get_stored_dir_and_file tb
	=	(sfSaveDisk,curDirStore,tb2);
	where {
		(saveDisk,tb1)		= LoadWord 532 tb;
		sfSaveDisk			= 0 - saveDisk;
		(curDirStore,tb2)	= LoadLong 920 tb1;
	};

Get_directory_and_file_name2 :: !String !Int !Int !Int !*Toolbox -> (!Int,!Int,!String,!*Toolbox);
Get_directory_and_file_name2 pathName p v d tb
	| (p >= l) || (pathName.[p] <> ':')
		= (v,d,pathName % (p, l_sub_1),tb);
	| colon && (0 == result) && (0 <> (16 bitand attrib))
		= Get_directory_and_file_name2 pathName p2 v d2 tb1;
		= (v,d, pathName % (inc p, l_sub_1),tb1);
	where {
		l_sub_1					= dec l;
		(colon,p2)				= Find_last_colon pathName (inc p);
		(result,attrib,d2,tb1)	= GetCatInfo3 v d (pathName % (inc p, dec p2)) tb;
		l						= size pathName;
	};

Set_directory :: !Int !Int !*Toolbox -> *Toolbox;
Set_directory v d tb
	=	tb2;
	where {
		tb1 = StoreWord 532 (0-v) tb;
		tb2 = StoreLong 920 d tb1;
	};

SelectOutputFile:: !String !String !*World -> (!Bool,!String,!*World);
SelectOutputFile prompt originalName w
	# tb								= 0;
	  (selectorPos, tb1)				= SelectorPosition tb;
	  (v,d,fileName,tb2)				= Get_directory_and_file_name originalName tb1;
	  tb3								= Set_directory v d tb2;
	  (good,copy,fType,vRefNum,version,fName,tb4)
										= SFPutFile selectorPos prompt fileName 0 (createArray 64 '\0') tb3;
	| good
		# (volumeNumber,directoryId,tb5)	= Get_working_directory_info vRefNum tb4;
		  (pathName, tb6)					= Get_directory_path vRefNum directoryId fName tb5;
		= (True,  pathName,	do_tb_io tb6 w);
		= (False, "",		do_tb_io tb4 w);

SelectorPosition :: !*Toolbox -> (!(!Int,!Int),!*Toolbox);
SelectorPosition tb
	=	((hPos,vPos), tb1);
	where {
		hPos				= (sr-sl-SelectorWidth ) / 2;
		vPos				= (sb-st-SelectorHeight) / 3;
		(sl,st, sr,sb, tb1)	= QScreenRect tb;
	};
