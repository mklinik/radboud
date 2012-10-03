definition module WriteOptionsFile

import
	StdFile,PmTypes;

ApplicationOptionsToFlags :: !ApplicationOptions -> Int
FlagsToApplicationOptions :: !Int !ApplicationOptions -> ApplicationOptions

write_options_file :: !{#.Char} !.Int !.Int !.Int !.Int !.Int !.Int !Bool !*a -> *(!Bool,!*a) | FileSystem a
