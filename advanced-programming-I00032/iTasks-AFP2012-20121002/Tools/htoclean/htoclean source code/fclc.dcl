definition module fclc;

// from StdString import String;
from StdFile import :: Files;

:: HError = HError !String !Int;

compile_header :: !{#Char} !{#Char} !*Files -> *(![HError],!*Files);
//compile_header directory_name file_name files = (errors,files);
