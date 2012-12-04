module FileHandlingTest

import iTasks, GeoDomain, DocumentDomain
import GenParse,GenPrint,GenVisualize,GenUpdate

docAction :: Task Void
docAction = 
	enterInformation "Upload Document to be stored" >>=
	\doc -> storeDocumentToFile doc "doc" >>=
	\ok = case ok of
		True
			= loadDocumentFromFile doc.Document.fileName "doc" >>= 
			updateInformation "Document" >>= 
			showMessageAbout "Document"
		False 
			= showMessage "Failed to store"

Start :: *World -> *World
Start world = startEngine [ workflow "Document Test" (docAction >>| return Void)
						  ] world