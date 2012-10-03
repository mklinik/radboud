definition module FastString

/**
* Predicate which tests if a string starts with another substring
*
* @param The substring.
* @param The string that is being searched.
*/
startsWith :: !String !String -> Bool

/**
* Predicate which tests if a string ends with another substring
*
* @param The substring.
* @param The string that is being searched.
*/
endsWith :: !String !String -> Bool

/**
* Find the first occurence of a character in another string
* starting from a given character position
*
* @param The string that is being searched.
* @param The start position.
* @param The character.
*/
charIndex :: !String !Int !Char -> (!Bool,!Int)

/**
* Find the first occurence of a character in another string
* starting from a given character position BACKWARDS.
*
* @param The string that is being searched.
* @param The start position.
* @param The character.
*/
charIndexBackwards :: !String !Int !Char -> (!Bool,!Int)

/**
* Predicate which tests if a substring matches another string
* at a given position
*
* @param The substring that is being matched.
* @param The string that is being searched.
* @param The start position.
*/
matchAt :: !String !String !Int -> Bool