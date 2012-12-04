module SQLExample

import iTasks, SQLDatabase
/**
* This module demonstrates the use of (My)SQL from an iTasks application
*/
MYDB = {host="localhost",username="root",password="root",database="northwind"}

queryShare = sqlShare MYDB "SELECT * FROM customers LIMIT 10" []

example = viewSharedInformation ("SQL Query","This is the output of a query") [ViewWith toTable] queryShare
where
	toTable rows = Table headers [[Text (toString cell) \\ cell <- row] \\ row <- rows] Nothing
	headers = ["Customer","Company", "Contact name", "Contact title"]
	
	
Start :: *World -> *World
Start world = startEngine example world