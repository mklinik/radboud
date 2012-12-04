implementation module UserAdmin

import iTasks, Text, Tuple

derive class iTask UserAccount

//Initial root user
ROOT_USER :== {credentials={Credentials|username=Username "root",password = Password "root"},title = Just "Root user", roles = ["admin"]}

userAccounts :: Shared [UserAccount]
userAccounts = sharedStore "UserAccounts" [ROOT_USER]

users :: ReadOnlyShared [User]
users = mapReadWrite (\accounts -> [AuthenticatedUser (toString a.UserAccount.credentials.Credentials.username) a.UserAccount.roles a.UserAccount.title
									\\ a <- accounts]
					 , \Void accounts -> Nothing) userAccounts

usersWithRole :: !Role -> ReadOnlyShared [User]
usersWithRole role = mapRead (filter (hasRole role)) users
where
	hasRole role (AuthenticatedUser _ roles _) = isMember role roles
	hasRole _ _ = False

userAccount :: UserId -> Shared (Maybe UserAccount)
userAccount userId = mapReadWrite (getAccount userId, \w r -> Just (setAccount w r)) userAccounts
where
	getAccount :: UserId [UserAccount] -> Maybe UserAccount
	getAccount userId accounts = case [a \\ a <- accounts | identifyUserAccount a == userId] of
		[a] = Just a
		_	= Nothing
		
	setAccount :: (Maybe UserAccount) [UserAccount] -> [UserAccount]
	setAccount Nothing accounts = accounts
	setAccount (Just updated) accounts = [if (identifyUserAccount a == identifyUserAccount updated) updated a \\ a <- accounts]

identifyUserAccount :: UserAccount -> UserId
identifyUserAccount {UserAccount|credentials={Credentials|username}} = toString username

accountToUser :: UserAccount -> User
accountToUser {UserAccount|credentials={Credentials|username},title,roles} = AuthenticatedUser (toString username) roles title

accountTitle :: UserAccount -> String
accountTitle {UserAccount|credentials={Credentials|username},title=Just title} = title  
accountTitle {UserAccount|credentials={Credentials|username}} = "Untitled (" +++ toString username +++ ")" 

authenticateUser :: !Username !Password	-> Task (Maybe User)
authenticateUser (Username username) password
	=	get (userAccount username)
	@	(maybe Nothing (\a -> if (a.UserAccount.credentials.Credentials.password == password) (Just (accountToUser a)) Nothing))
	
doAuthenticated :: (Task a) -> Task a | iTask a
doAuthenticated task = doAuthenticatedWith verify task
where
	verify {Credentials|username,password} = authenticateUser username password
	
doAuthenticatedWith :: (Credentials -> Task (Maybe User)) (Task a) -> Task a | iTask a
doAuthenticatedWith verifyCredentials task
	=	enterInformation ("Log in","Please enter your credentials") []
	>>!	verifyCredentials
	>>= \mbUser -> case mbUser of
		Nothing		= throw "Authentication failed"
		Just user	= workAs user task
	
createUser :: !UserAccount -> Task UserAccount
createUser account
	=	get (userAccount (identifyUserAccount account))
	>>= \mbExisting -> case mbExisting of
		Nothing
			= update (\accounts -> accounts ++ [account]) userAccounts @ const account
		_	
			= throw ("A user with username '" +++ toString account.UserAccount.credentials.Credentials.username +++ "' already exists.")

deleteUser :: !UserId -> Task Void
deleteUser userId = update (filter (exclude userId)) userAccounts @ const Void
where
	exclude user d	= identifyUserAccount d == userId 

manageUsers :: Task Void
manageUsers =
	(		enterSharedChoice ("Users","The following users are available") [] userAccounts @ identifyUserAccount
		>>*	[ AnyTime		(Action "New")									(\_ -> createUserFlow	@ const False)
			, WithResult	(ActionEdit) (const True)						(\u -> updateUserFlow u @ const False)
			, WithResult	(ActionDelete) (const True)						(\u -> deleteUserFlow u @ const False)
			, AnyTime 		(Action "Import & export/Import CSV file...")	(\_ -> importUserFileFlow @ const False)
			, AnyTime		(Action "Import & export/Export CSV file...")	(\_ -> exportUserFileFlow @ const False)
			, AnyTime		(Action "Import & export/Import demo users")	(\_ -> importDemoUsersFlow @ const False)
			, AnyTime		(ActionQuit)									(\_ -> return True)
			]
	) <! id @ const Void

createUserFlow :: Task Void
createUserFlow =
		enterInformation ("Create user","Enter user information") []
	>>*	[ AnyTime		ActionCancel	(\_ -> return Void)
		, WithResult	ActionOk 		(const True) (\user ->
											createUser user
										>>|	viewInformation "User created" [] "Successfully added new user"
										>>| return Void
									)
		]
		
updateUserFlow :: UserId -> Task UserAccount
updateUserFlow userId
	=	get (userAccount userId)
	>>= \mbAccount -> case mbAccount of 
		(Just account)
			=	(updateInformation ("Editing " +++ fromMaybe "Untitled" account.UserAccount.title ,"Please make your changes") [] account
			>>*	[ AnyTime ActionCancel (\_ -> return account)
				, WithResult ActionOk (const True)(\newAccount ->
												set (Just newAccount) (userAccount userId)
											>>=	viewInformation "User updated" [ViewWith (\(Just {UserAccount|title}) -> "Successfully updated " +++ fromMaybe "Untitled" title)]
											>>| return newAccount
											)
				])
		Nothing
			=	(throw "Could not find user details")
				
deleteUserFlow :: UserId -> Task UserAccount
deleteUserFlow userId
	=	get (userAccount userId)
	>>= \mbAccount -> case mbAccount of 
		(Just account)
			=	viewInformation "Delete user" [] ("Are you sure you want to delete " +++ accountTitle account +++ "? This cannot be undone.")
			>>*	[ AnyTime ActionNo	(\_ -> return account)
				, AnyTime ActionYes	(\_ -> deleteUser userId
									>>|	viewInformation "User deleted" [ViewWith (\account -> "Successfully deleted " +++ accountTitle account +++ ".")] account
									>>| return account
									)
				]
				
importUserFileFlow :: Task Void
importUserFileFlow = viewInformation "Not implemented" [] Void

exportUserFileFlow :: Task Document
exportUserFileFlow
	=	get userAccounts -&&- get applicationName
	>>= \(list,app) ->
		createCSVFile (app +++ "-users.csv") (map toRow list)
	>>=	viewInformation ("Export users file","A CSV file containing the users of this application has been created for you to download.") []
where
	toRow {credentials = {Credentials|username =(Username username), password = (Password password)}, title, roles}
		= [fromMaybe "" title,username,password:roles]
	
importDemoUsersFlow :: Task [UserAccount]
importDemoUsersFlow =
	allTasks [catchAll (createUser (demoUser n)) (\_ -> return (demoUser n)) \\ n <- names]
where
	demoUser name
		= {UserAccount
		  | credentials = {Credentials| username = Username (toLowerCase name), password = Password (toLowerCase name)}
		  , title = Just name
		  , roles = []
		  }
	names = ["Alice","Bob","Carol","Dave","Eve","Fred"]