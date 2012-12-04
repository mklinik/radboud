definition module thread_message;

import StdString;

get_message_number :: Int;
// int get_message_number ();
get_current_thread_id :: Int;
// int get_current_thread_id ();
start_compiler_process :: !String !String !String -> (!Int,!Int,!Int,!Int);
// int start_compiler_process (CleanString compiler_path,CleanString compiler_directory,CleanString command,int* compiler_thread_id_p,int* compiler_thread_handle_p,int* process_handle_p);
send_string_to_thread :: !Int !Int !Int !String -> Int;
// int send_string_to_thread (int thread_id,int thread_handle,int wm_number,CleanString s);
send_integers_to_thread :: !Int !Int !Int !Int -> Int;
// int send_integers_to_thread (int thread_id,int wm_number,int i1,int i2);
get_integers_from_message :: !Int -> (!Int,!Int,!Int);
// int get_integers_from_message (int wm_number,int* i1_p,int* i2_p);
get_integers_from_thread_message :: !Int !Int -> (!Int,!Int,!Int);
// int get_integers_from_thread_message (int wm_number,int thread_handle,int* i1_p,int* i2_p);
get_string_from_file_map_and_delete_map :: !Int !String -> Int;
// int get_string_from_file_map_and_delete_map (int file_map,CleanString s);
