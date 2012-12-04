
#define Clean(a)

typedef struct clean_string *CleanString;

/*	a string in Clean is:
	struct clean_string {
		int clean_string_length;
		char clean_string_characters[clean_string_length];
	};
	The string does not end with a '\0' !
*/

#ifdef _WIN64

/* CleanStringLength(clean_string) returns length of the clean_string in characters */
#define CleanStringLength(clean_string) (*(unsigned __int64 *)(clean_string))

/* CleanStringCharacters(clean_string) returns a pointer to the characters of the clean_string */
#define CleanStringCharacters(clean_string) ((char*)(1+(unsigned __int64 *)(clean_string)))

/* CleanStringSizeInts(string_length) return size of CleanString in integers */
#define CleanStringSizeInts(string_length) (1+(((unsigned __int64)(string_length)+7)>>3))

/* CleanStringVariable(clean_string,string_length) defines variable clean_string with length string_length,
   before using the clean_string variable, cast to CleanString, except for the macros above  */
#define CleanStringVariable(clean_string,string_length) unsigned __int64 clean_string[CleanStringSizeInts(string_length)]

/* CleanStringSizeBytes(string_length) return size of CleanString in bytes */
#define CleanStringSizeBytes(string_length) (8+(((unsigned __int64)(string_length)+7) & -8))

#else

/* CleanStringLength(clean_string) returns length of the clean_string in characters */
#define CleanStringLength(clean_string) (*(unsigned int *)(clean_string))

/* CleanStringCharacters(clean_string) returns a pointer to the characters of the clean_string */
#define CleanStringCharacters(clean_string) ((char*)(1+(unsigned int *)(clean_string)))

/* CleanStringSizeInts(string_length) return size of CleanString in integers */
#define CleanStringSizeInts(string_length) (1+(((unsigned int)(string_length)+3)>>2))

/* CleanStringVariable(clean_string,string_length) defines variable clean_string with length string_length,
   before using the clean_string variable, cast to CleanString, except for the macros above  */
#define CleanStringVariable(clean_string,string_length) unsigned size_t clean_string[CleanStringSizeInts(string_length)]

/* CleanStringSizeBytes(string_length) return size of CleanString in bytes */
#define CleanStringSizeBytes(string_length) (4+(((unsigned int)(string_length)+3) & -4))

#endif