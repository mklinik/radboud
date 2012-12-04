
#define Clean(a)

typedef struct clean_string *CleanString;

/*	a string in Clean is:
	struct clean_string {
		int clean_string_length;
		char clean_string_characters[clean_string_length];
	};
	The string does not end with a '\0' !
*/

/* CleanStringLength(clean_string) returns the length of the clean_string in characters */
#define CleanStringLength(clean_string) (*(unsigned int *)(clean_string))

/* CleanStringCharacters(clean_string) returns a pointer to the characters of the clean_string */
#define CleanStringCharacters(clean_string) ((char*)(1+(unsigned int *)(clean_string)))

/* CleanStringSizeInts(string_length) return size of *CleanString in integers */
#define CleanStringSizeInts(string_length) (1+(((unsigned int)(string_length)+3)>>2))

/* CleanStringVariable(clean_string,string_length) defines variable clean_string with length string_length,
   before using the clean_string variable, cast to CleanString, except for the macros above  */
#define CleanStringVariable(clean_string,string_length) unsigned int clean_string[CleanStringSizeInts(string_length)]

/* CleanStringSizeBytes(string_length) return size of *CleanString in bytes */
#define CleanStringSizeBytes(string_length) (4+(((unsigned int)(string_length)+3) & -4))

typedef int *CleanIntArray;

/* CleanIntArraySize(clean_array) returns the size (number of elements) of the clean_int_array */
#define CleanIntArraySize(clean_int_array) (((unsigned int *)(clean_int_array))[-2])

typedef double *CleanRealArray;

/* CleanRealArraySize(clean_real_array) returns the size (number of elements) of the clean_real_array */
#define CleanRealArraySize(clean_real_array) (((unsigned int *)(clean_real_array))[-2])

typedef unsigned char *CleanCharArray;

/* CleanCharArraySize(clean_char_array) returns the size (number of elements) of the clean_char_array */
#define CleanCharArraySize(clean_char_array) (((unsigned int *)(clean_char_array))[-1])
