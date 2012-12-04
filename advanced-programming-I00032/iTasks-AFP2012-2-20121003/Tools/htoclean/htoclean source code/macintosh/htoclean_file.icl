implementation module htoclean_file;

// for macintosh

import StdEnv;

DirectorySeparator :== ':';

import mac_file_selector_carbon;

wait_for_keypress :: !*World -> *World;
wait_for_keypress w
	# (stdio,w) = stdio w;
	  stdio = stdio <<< "Press any key to exit";
	  (ok,c,stdio) = freadc stdio;
	  (ok,w) = fclose stdio w;
	= w;

get_path_name :: !*World -> (!Bool,!String,!*World);
get_path_name w
	# (stdio_,w) = stdio w;
	  stdio_ = stdio_ <<< "Select the C header file\n";
	  (_,w) = fclose stdio_ w;
	= SelectInputFile w;
	