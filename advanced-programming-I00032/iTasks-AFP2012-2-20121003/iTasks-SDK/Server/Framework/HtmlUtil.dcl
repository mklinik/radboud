definition module HtmlUtil
/**
* This module provides rudimentary utilities for generating simple html pages.
*
*/
import HTML, JSON_NG, HTTP
/*
* Generate a 404 page
*/
notFoundResponse :: !HTTPRequest -> HTTPResponse
/**
* Gets the value of a parameter in the GET or POST values.
*/
paramValue :: !String !HTTPRequest -> String
/**
* Check if a parameter is set
*/
hasParam :: !String !HTTPRequest -> Bool
/**
* Convert newlines to br tags.
*/
nl2br :: !String -> HtmlTag
/**
* Convert a html string to pure text.
*/
html2text :: !String -> String
