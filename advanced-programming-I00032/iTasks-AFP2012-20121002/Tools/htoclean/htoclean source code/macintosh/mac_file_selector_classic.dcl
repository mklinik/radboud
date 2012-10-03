definition module mac_file_selector_classic;

SelectInputFile	:: !*World -> (!Bool,!String,!*World);
SelectOutputFile:: !String !String !*World -> (!Bool,!String,!*World);
