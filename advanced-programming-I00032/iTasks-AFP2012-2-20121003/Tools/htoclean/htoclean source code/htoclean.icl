module htoclean;

import StdEnv;
import fclc,htoclean_file;

split_path_name_in_file_and_directory_name :: !{#Char} -> (!{#Char},{#Char});
split_path_name_in_file_and_directory_name path_name
	# last_directory_separator_index = find_last_directory_separator (size path_name-1);
		with {
			find_last_directory_separator n
				| n<0 || path_name.[n]==DirectorySeparator
					= n;
					= find_last_directory_separator (n-1);
		}
	= (path_name % (0,last_directory_separator_index),path_name % (last_directory_separator_index+1,size path_name-1));

write_errors [] stdio
	= stdio;
write_errors [HError string line:l] stdio
	| line<>0
		# stdio=stdio <<< string <<< " [line:" <<< line <<< "]\n";
		= write_errors l stdio;
		# stdio=stdio <<< string <<< '\n';
		= write_errors l stdio;

Start w
	# (ok,path_name,w) = get_path_name w;
	| not ok
		= w;
		# (directory_name,file_name) = split_path_name_in_file_and_directory_name path_name;
		  h_file_name = if (file_name % (size file_name-2,size file_name-1)==".h") (file_name % (0,size file_name-3)) file_name;
		  (errors,w) = accFiles (compile_header directory_name h_file_name) w;
		= case errors of {
			[]	->	w;
			_	# (stdio,w) = stdio w;
		  		  stdio = write_errors errors stdio;
		  		  (_,w) = fclose stdio w;
				-> wait_for_keypress w;
		  };
