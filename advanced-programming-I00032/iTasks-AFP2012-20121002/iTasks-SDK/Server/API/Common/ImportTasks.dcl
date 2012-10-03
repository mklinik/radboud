definition module ImportTasks
/**
* This module provides tasks for importing external data into a workflow.
*/
import FilePath, SystemTypes, Task
/**
* Import a file on the server's filesystem as a Document
*
* @param File path: The path of the file to import
*
* @return The imported document
* @throws FileException
* 
* @gin-icon page_white
*/
importDocument		:: !FilePath -> Task Document
/**
* Import the content of  a text file on the server's filesystem.
*
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException
* 
* @gin-icon page_white_text
*/
importTextFile		:: !FilePath -> Task String
/**
* Import a comma separated vector (CSV) file on the server's filesystem.
*
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException
* 
* @gin-icon page_white_csv
*/
importCSVFile		:: !FilePath -> Task [[String]]
importCSVDocument	:: !Document -> Task [[String]]
/**
* Import a comma separated vector (CSV) file on the server's filesystem using
* custom separator characters.
*
* @param Separator: The field separator
* @param Quote character: The string quote character
* @param Escape character : The escape character
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException
* 
* @gin False
*/
importCSVFileWith		:: !Char !Char !Char !FilePath -> Task [[String]]
importCSVDocumentWith	:: !Char !Char !Char !Document -> Task [[String]]
/**
* Import and parse a JSON datafile on the server's filesystem.
*
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException 
* 
* @gin-icon page_white_json
*/
importJSONFile		:: !FilePath -> Task a | iTask a
/**
* Import and parse a JSON datafile on the server's filesystem using
* a custom parse function.
*
* @param Decoder function: The JSON decoder function
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException 
* 
* @gin False
*/
importJSONFileWith	:: !(JSONNode -> Maybe a) !FilePath -> Task a | iTask a
