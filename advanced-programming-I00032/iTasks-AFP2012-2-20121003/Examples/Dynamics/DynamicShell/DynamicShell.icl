module DynamicShell

import StdEnv, StdParsComb, StdDynamic, StdDynamicFileIO, Debug

:: Void = Void

:: Try a = Ok a | Raise String

:: Expression = (Apply) Expression Expression | Name String	| Value Dynamic			

Start :: *World -> *World
Start world 
	= case writeFunctions shellFunctions world of
		Ok world -> shell "Dynamic shell (enter empty line to exit)" world
		Raise error -> abort error

writeFunctions :: [(String, Dynamic)] *World -> Try *World
writeFunctions [(name, dyn):functions] world
	# (ok, world) = writeDynamic name dyn world
	| not ok = Raise ("Cannot write dynamic \"" +++ name +++ "\"")
	= writeFunctions functions world
writeFunctions _ world = Ok world

shellFunctions :: [(String, Dynamic)]
shellFunctions =
	[	("shell", dynamic shell "New Dynamic Shell" :: *World -> *World)
	,	("if", dynamic \x y z -> if x y z :: A.a: Bool a a -> a)
	,	("==", dynamic (==) :: Int Int -> Bool)
	,	("+", dynamic (+) :: Int Int -> Int)
	,	("-", dynamic (-) :: Int Int -> Int)
	]

shell :: String *World -> *World
shell message world
	# (console, world) = stdio world
	  console = fwrites (message +++ "\n> ") console
	  (cmdline, console) = freadline console
	  (_, world) = fclose console world
	| cmdline == "" || cmdline == "\n" = world
	= case parse cmdline of
		Ok expression 
			# (try, world) = resolve expression world
			-> case try of
				Ok expression -> case compose expression of
					Ok dyn
						# (result, world) = evaluate dyn world
						-> shell (valueOf result +++ " :: " +++ typeOf result) world
					Raise error -> shell error world
				Raise error -> shell error world
		Raise error -> shell error world

parse :: String -> Try Expression
parse cmdline = case begin (sp shellParser <& sp endOfLine) (fromString cmdline) of
	[([], expression)] -> Ok expression
	_ -> Raise "Syntax error"
where
	endOfLine = <?> (symbol '\n') &> eof

resolve :: Expression *World -> (Try Expression, *World)
resolve (e1 Apply e2) world 
	# (try1, world) = resolve e1 world
	  (try2, world) = resolve e2 world
	= case (try1, try2) of
		(Ok e1, Ok e2) -> (Ok (e1 Apply e2), world)
		(Raise error, _) -> (Raise error, world)
		(_, Raise error) -> (Raise error, world)
resolve (Name name) world
	# (ok, dyn, world) = readDynamic name world
	| ok = (Ok (Value dyn), world)
	= (Raise ("Cannot read dynamic \"" +++ name +++ "\""), world)
resolve other world = (Ok other, world)

compose :: Expression -> Try Dynamic
compose (e1 Apply e2) = case (compose e1, compose e2) of
	(Ok d1, Ok d2) -> apply d1 d2
	(Raise error, _) -> Raise error
	(_, Raise error) -> Raise error
compose (Value dyn) = Ok dyn
compose _ = Raise "Cannot compose expression"

apply :: Dynamic Dynamic -> Try Dynamic
apply (f :: a -> b) (x :: a) = Ok (dynamic f x :: b)
apply df dx = Raise ("Cannot apply: " +++ typeOf df +++ " to: " +++ typeOf dx)

evaluate :: Dynamic *World -> (Dynamic, *World)
evaluate (x :: A.a: a) world 
	= (dynamic x :: A.a: a, world)
evaluate (x :: A.a b: a -> b) world 
	= (dynamic x :: A.a b: a -> b, world)
evaluate (x :: A.a: a -> a) world 
	= (dynamic x :: A.a: a -> a, world)
evaluate (x :: A.a c: a -> (b, c)) world 
	= (dynamic x :: A.a c: a -> (b, c), world)
evaluate (x :: A.a: a -> (b, a)) world 
	= (dynamic x :: A.a: a -> (b, a), world)
evaluate d=:(f :: *World -> *(a, *World)) world
	# (x, world) = f world
	= (dynamic x :: a, world)
evaluate d=:(f :: *World -> *World) world
	# world = f world
	= (dynamic Void :: Void, world)
evaluate (x :: a) world 
	= (dynamic x :: a, world)

typeOf :: Dynamic -> String
typeOf d = toString (typeCodeOfDynamic d)

valueOf :: Dynamic -> String
valueOf dyn = foldl (+++) "" (debugShowWithOptions [DebugMaxChars 79, DebugTerminator ""] (fst (valueAndType dyn)))
where
	valueAndType :: !Dynamic -> (a, TypeCode)
	valueAndType _ = code inline {
			pop_a	0
		}

shellParser :: CParser Char Expression t
shellParser = expression
where
	expression = simple <&> \e -> <*> (sp simple) <@ \es -> foldl (Apply) e es

	simple
		= symbol '(' &> sp expression <& spsymbol ')'
		<!> value <@ Value
		<!> name <@ Name
	
	name = <+> (satisfy (\c -> not (isMember c [' \t\n\r']))) <@ toString
	
	value
		= bool
		<!> char
		<!> real
		<!> int
		<!> string
	where			
		bool = token ['True'] <@ const (dynamic True :: Bool) <!> token ['False'] <@ const (dynamic False :: Bool)

		char = symbol '\'' &> character '\'' <& symbol '\'' <@ (\c -> dynamic c :: Char)

		int 
			= <?> (symbol '+') &> nat <@ (\i -> dynamic i :: Int)
			<!> symbol '-' &> nat <@ (\i -> dynamic ~ i :: Int)
	
		real = (realWithExp <!> realWithoutExp) <@ \cs -> dynamic toReal (toString cs) :: Real
		where
			realWithoutExp = int <++> symbol '.' <:&> nat
			realWithExp = int <++> (<?> (symbol '.' <:&> nat) <@ flatten) <++> symbol 'E' <:&> int
			int = symbol '-' <:&> nat <!> <?> (symbol '+') &> nat
			nat = <+> (satisfy (\c -> isMember c ['0'..'9']))

		string = symbol '"' &> <*> (character '"') <& symbol '"' <@ (\cs -> dynamic toString cs :: String)

		character sep 
			=	satisfy ((<>) sep) 
			<!> symbol '\\' &> satisfy (const True)
