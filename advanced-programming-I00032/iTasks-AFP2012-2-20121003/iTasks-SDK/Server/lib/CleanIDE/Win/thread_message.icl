implementation module thread_message;

import StdString,StdInt;

import code from "c_thread_message.";
import code from library "thread_kernel32_library";
import code from library "thread_user32_library";


get_message_number :: Int;
get_message_number  = code {
	ccall get_message_number ":I"
};
// int get_message_number ();

get_current_thread_id :: Int;
get_current_thread_id  = code {
	ccall get_current_thread_id ":I"
};
// int get_current_thread_id ();

start_compiler_process :: !String !String !String -> (!Int,!Int,!Int,!Int);
start_compiler_process a0 a1 a2
	= IF_INT_64_OR_32 (start_compiler_process_64 a0 a1 a2) (start_compiler_process_32 a0 a1 a2);
// int start_compiler_process (CleanString compiler_path,CleanString compiler_directory,CleanString command,int* compiler_thread_id_p,size_t* compiler_thread_handle_p,size_t* process_handle_p);

start_compiler_process_32 :: !String !String !String -> (!Int,!Int,!Int,!Int);
start_compiler_process_32 a0 a1 a2 = code {
	ccall start_compiler_process "SSS:IIII"
};

start_compiler_process_64 :: !String !String !String -> (!Int,!Int,!Int,!Int);
start_compiler_process_64 a0 a1 a2 = code {
	ccall start_compiler_process "SSS:IIpp"
};

send_string_to_thread :: !Int !Int !Int !String -> Int;
send_string_to_thread a0 a1 a2 a3 = code {
	ccall send_string_to_thread "IIIS:I"
};
// int send_string_to_thread (int thread_id,int thread_handle,int wm_number,CleanString s);

send_integers_to_thread :: !Int !Int !Int !Int -> Int;
send_integers_to_thread a0 a1 a2 a3 = code {
	ccall send_integers_to_thread "IIII:I"
};
// int send_integers_to_thread (int thread_id,int wm_number,int i1,int i2);

get_integers_from_message :: !Int -> (!Int,!Int,!Int);
get_integers_from_message a0 = code {
	ccall get_integers_from_message "I:III"
};
// int get_integers_from_message (int wm_number,int* i1_p,int* i2_p);

get_integers_from_thread_message :: !Int !Int -> (!Int,!Int,!Int);
get_integers_from_thread_message a0 a1 = code {
	ccall get_integers_from_thread_message "II:III"
};
// int get_integers_from_thread_message (int wm_number,int thread_handle,int* i1_p,int* i2_p);

get_string_from_file_map_and_delete_map :: !Int !String -> Int;
get_string_from_file_map_and_delete_map a0 a1 = code {
	ccall get_string_from_file_map_and_delete_map "IS:I"
};
// int get_string_from_file_map_and_delete_map (int file_map,CleanString s);
