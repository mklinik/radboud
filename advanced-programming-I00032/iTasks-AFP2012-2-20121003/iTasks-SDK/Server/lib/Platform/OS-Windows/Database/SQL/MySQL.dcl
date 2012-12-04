definition module MySQL
//This module defines implements the interface for relatational databases
//of SQL.dcl for the MySQL database engine

import SQL
import Maybe, StdString

:: MySQLContext
:: MySQLConnection
:: MySQLCursor

instance SQLEnvironment		World			MySQLContext
instance SQLContext			MySQLContext	MySQLConnection
instance SQLConnection		MySQLConnection	MySQLCursor
instance SQLCursor			MySQLCursor
