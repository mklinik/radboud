definition module ExportTasks
/**
* This module provides tasks for exporting data from a workflow to an external source
*/
import FilePath, SystemTypes, Task

/**
* Export a document to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Document: The document to export
*
* @return The exported document
* @throws FileException
* 
* @gin-icon page_white
*/
exportDocument		:: !FilePath !Document -> Task Document

/**
* Export a string as text file to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Text: The content to export
*
* @return The exported content
* @throws FileException
* 
* @gin-icon page_white_text
*/
exportTextFile		:: !FilePath !String -> Task String

/**
* Export a list of rows of fields to a comma separated vector (CSV) document.
*
* @param File name: A name of the created CSV file
* @param Cells: The content to export as a list of rows of lists of fields
*
* @return The exported content as a document
* 
* @gin-icon page_white_csv
*/
createCSVFile		:: !String ![[String]] -> Task Document
/**
* Export a list of rows of fields to a comma separated vector (CSV) file on the server's filesystem.
*
* @param File path: The path of the exported file
* @param Cells: The content to export as a list of rows of lists of fields
*
* @return The exported content
* @throws FileException
* 
* @gin-icon page_white_csv
*/
exportCSVFile		:: !FilePath ![[String]] -> Task [[String]]

/**
* Export a list of rows of fields to a comma separated vector (CSV) file on the server's filesystem
* using custom separator characters.
*
* @param Separator: The field separator
* @param Quote character: The string quote character
* @param Escape character: The escape character
* @param File path: The path of the exported file
* @param Cells: The content to export as a list of rows of lists of fields
*
* @return The exported content
* @throws FileException
* 
* @gin False
*/
exportCSVFileWith	:: !Char !Char !Char !FilePath ![[String]] -> Task [[String]]

/**
* Encode and export a JSON datafile to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Value: The content to encode as JSON using the generic JSON encoder
*
* @return The exported content
* 
* @gin-icon page_white_json
*/
exportJSONFile		:: !FilePath a -> Task a | iTask a
/**
* Encode and export a JSON datafile to the server's filesystem using a custom encode function.
* 
* @param Encoder function: The JSON encoder function
* @param File path: The path of the exported file
* @param Value: The content to encode as JSON
* 
* @return The exported content
* @throws FileException
* 
* @gin False
*/
exportJSONFileWith	:: !(a -> JSONNode) !FilePath a -> Task a | iTask a
