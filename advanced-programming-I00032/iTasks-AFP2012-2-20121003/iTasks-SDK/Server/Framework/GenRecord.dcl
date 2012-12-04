definition module GenRecord
/**
* This module provides functions for generically copying record fields of similar record.
* An example is a record representing the data model and a similar one representing the view.
* All fields with the same name & type can be copied automatically, only different fields have to be mapped manually.
*/

import SystemTypes, GenUpdate

/**
* Copies all fields with same name & type from one record to another.
*
* @param The source record
* @param The destrination record
* @return The resulting record
*/
copyRecord :: !a !b -> b | GenRecord a & GenRecord b

/**
* Maps a record to another type.
* All fields with same name & type keep their value, for others default values are filled in.
*
* @param The record to be mapped
* @return The resulting record of another type
*/
mapRecord :: !a -> b | GenRecord a & GenRecord, gUpdate{|*|} b

class GenRecord r
	| gGetRecordFields{|*|}
	, gPutRecordFields{|*|} r
	
generic gGetRecordFields r :: !r ![GenType] !*RecordFields -> *RecordFields
generic gPutRecordFields r :: !r ![GenType] !*RecordFields -> (!r,!*RecordFields)

:: *RecordFields

derive gGetRecordFields UNIT, PAIR, EITHER, CONS, OBJECT, RECORD of {grd_type}, FIELD of {gfd_name,gfd_index}
derive gGetRecordFields Int, Real, Char, Bool, String
derive gGetRecordFields Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gGetRecordFields Note, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, User, RadioChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, HtmlTag, HtmlAttr
derive gGetRecordFields EmailAddress, Action

derive gPutRecordFields UNIT, PAIR, EITHER, CONS, OBJECT, RECORD of {grd_type}, FIELD of {gfd_name,gfd_index}
derive gPutRecordFields Int, Real, Char, Bool, String
derive gPutRecordFields Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gPutRecordFields Note, Password, Date, Time, DateTime, Document, FormButton, EUR, USD, User, RadioChoice, CheckMultiChoice, Map, TreeChoice, Tree, TreeNode, HtmlTag, HtmlAttr
derive gPutRecordFields EmailAddress, Action
