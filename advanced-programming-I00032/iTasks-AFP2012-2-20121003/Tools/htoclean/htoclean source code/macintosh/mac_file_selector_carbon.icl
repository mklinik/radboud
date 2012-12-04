implementation module mac_file_selector_carbon;

import StdClass,StdBool,StdChar,StdInt,StdMisc,StdString, StdFile,StdArray;
import	standard_file,files,pointer,quickdraw,memory,appleevents,navigation;

String64 :: String;
String64 = createArray 64 '@';

Error_i	:: !String !Int -> .x;
Error_i string i = abort (string +++ toString i);

Get_name_and_parent_id_of_directory :: !Int !Int !*Toolbox -> (!String,!Int,!*Toolbox);
Get_name_and_parent_id_of_directory volumeNumber directoryId tb
	| osError==0
		= (folderName,parentId,tb1);
		= Error_i "Error code returned by BPGetCatInfo: " osError;
	where {
		(osError,folderName,parentId,tb1) = GetCatInfo2 volumeNumber directoryId String64 tb;
	};

Get_directory_path :: !Int !Int !String !*Toolbox -> (!String, !*Toolbox);
Get_directory_path volumeNumber directoryId path tb
	| directoryId==2
		= (folderName +++ ":" +++ path, tb1);
		= Get_directory_path volumeNumber parentId (folderName +++ ":" +++ path) tb1;
	where {
		(folderName, parentId, tb1) = Get_name_and_parent_id_of_directory volumeNumber directoryId tb;
	};

Find_colon :: !String !Int -> (!Bool,!Int);
Find_colon s p = Find_colon2 s p (dec (size s));

Find_colon2 :: String !Int !Int -> (!Bool,!Int);
Find_colon2 s p l
|	p >= l				= (False,p);
|	s.[p] == ':'	= (True,p);
						= Find_colon2 s (inc p) l;

Get_directory_and_file_name :: !String !*Toolbox -> (!Int,!Int,!String,!*Toolbox);
Get_directory_and_file_name pathName tb
|	not colon					= (sfSaveDisk, curDirStore, pathName,tb1);
|	pathName.[0] == ':'		= Get_directory_and_file_name2 pathName 0 sfSaveDisk curDirStore tb1;
|	0 == result					= Get_directory_and_file_name2 pathName colonPosition volumeNumber 2 tb`;
								= (sfSaveDisk`,curDirStore`,pathName,tb``);
	where {
		(colon,colonPosition)			= Find_colon pathName 0;
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
		(colon,p2)				= Find_colon pathName (inc p);
		(result,attrib,d2,tb1)	= GetCatInfo3 v d (pathName % (inc p, dec p2)) tb;
		l						= size pathName;
	};

do_tb_io :: !*Toolbox !*World -> *World;
do_tb_io _ w = w;

SelectInputFile	:: !*World -> (!Bool,!String,*World);
SelectInputFile w
	# tb = 0;
	# (nav_reply_record,_,tb) = NewPtr NavReplyRecordSize tb;
	# (err,tb) = NavGetFile 0 nav_reply_record 0 0 0 0 0 0 tb;
	# (ok,file_name,tb)=get_or_put_file_selector_result err nav_reply_record tb;
	= (ok,file_name,do_tb_io tb w);

SelectOutputFile:: !String !String !*World -> (!Bool, !String,!*World);
SelectOutputFile prompt originalName w
	# tb = 0;
	# (err,nav_dialog_options,tb) = NavGetDefaultDialogOptions tb;
	| err<>0
		# tb=DisposePtr nav_dialog_options tb;
		= (False,"",do_tb_io tb w);
	# (nav_reply_record,_,tb) = NewPtr NavReplyRecordSize tb;
	# (flags,tb) = LoadLong (nav_dialog_options+NavDialogOptionFlagsOffset) tb;	
	# flags=flags bitor kNavNoTypePopup;
	# tb = StoreLong (nav_dialog_options+NavDialogOptionFlagsOffset) flags tb;
	# tb = copy_string_to_memory originalName (nav_dialog_options+NavDialogOptionSavedFileNameOffset) tb
	# (err,tb) = NavPutFile 0 nav_reply_record nav_dialog_options 0 0 0 /*0x2a2a2a2a **** */ 0 tb;
//	# (err,tb) = NavPutFile 0 nav_reply_record 0 0 0 0 0 tb;
	# (ok,file_name,tb)=get_or_put_file_selector_result err nav_reply_record tb;
	# tb=DisposePtr nav_dialog_options tb;
	= (ok,file_name,do_tb_io tb w);

copy_string_to_memory s p tb
	# tb=StoreByte p (size s) tb;
	= copy_chars 0 (p+1) tb;
	{
		copy_chars i p tb
			| i>=size s
				= tb;
				# tb = StoreByte (p+i) (toInt s.[i]) tb;
				= copy_chars (i+1) p tb;
	}

get_or_put_file_selector_result err nav_reply_record tb
	| err<>0
		# tb=DisposePtr nav_reply_record tb;
		= (False,"",tb);
	# (valid_record,tb) = LoadByte (nav_reply_record+NavReplyValidRecordOffset) tb;
	| valid_record==0
		# (_,tb)=NavDisposeReply nav_reply_record tb;
		# tb=DisposePtr nav_reply_record tb;
		= (False,"",tb);
	# fs_spec=createArray 70 '\0';
	# (r,theAEKeyword,typeCode,actualSize,tb) = AEGetNthPtr (nav_reply_record+NavReplySelectionOffset) 1 KeyFssString fs_spec tb;
	| r<>0 || actualSize<>70
		# (_,tb)=NavDisposeReply nav_reply_record tb;
		# tb=DisposePtr nav_reply_record tb;
		= (False,"",tb);
	# file_name_size=toInt fs_spec.[6]
	# vRefNum=((toInt fs_spec.[0]<<8 bitor toInt fs_spec.[1])<<16)>>16;
	# directoryId=((toInt fs_spec.[2]<<8 bitor toInt fs_spec.[3])<<8 bitor toInt fs_spec.[4])<<8 bitor toInt fs_spec.[5];
	# file_name=fs_spec % (7,6+file_name_size);
	# (path_name,tb)=Get_directory_path vRefNum directoryId file_name tb;
	# (_,tb)=NavDisposeReply nav_reply_record tb;
	# tb=DisposePtr nav_reply_record tb;
	= (True,path_name,tb);
