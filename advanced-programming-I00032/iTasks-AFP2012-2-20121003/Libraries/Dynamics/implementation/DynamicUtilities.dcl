definition module DynamicUtilities

FromStringToInt :: !{#Char} !Int -> Int

WriteLong :: !*{#Char} !Int !Int -> *{#Char}

NF :: !.a -> .a

ends :: !String !String -> Bool;

ExtractPathAndFile :: !String -> (!String,!String);

ExtractPathFileAndExtension :: !String -> (!String,!String);