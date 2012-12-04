implementation module SaplParser

import StdEnv, StdMaybe, Map, Void, Error
import SaplTokenizer

instance ==	SaplTerm
where
	(==) (SName name1 _) (SName name2 _) = name1 == name2
	(==) (SStrictName name1) (SStrictName name2) = name1 == name2	
	(==) _ _ = False

instance < SaplTerm
where
	(<) (SName name1 _) (SName name2 _) = name1 < name2
	(<) (SStrictName name1) (SStrictName name2) = name1 < name2	
	(<) _ _ = False

(>>=) infixl 1
(>>=) f g = \st0 ->
		case f st0 of
			Ok (r, st1) = g r st1
			Error str		 = Error str

returnS r :== \s -> Ok (r,s)
returnE e :== \s -> Error e

mandatory errmsg (Just t, ts)
		= returnS (t, ts)
mandatory errmsg (Nothing, ts)
		= returnE (ts, errmsg)

incLevel a :== \s -> Ok (a, {s & ps_level = s.ps_level + 1})
decLevel a :== \s -> Ok (a, {s & ps_level = s.ps_level - 1})
getLevel :== \s -> Ok (s.ps_level, s)
addConstructor name args :== \s -> Ok (name, {s & ps_constructors = put name args s.ps_constructors})
addFunction name args :== \s -> Ok (name, {s & ps_functions = put name args s.ps_functions})
addCAF name :== \s -> Ok (name, {s & ps_CAFs = put name Void s.ps_CAFs})
defaultState = {ps_level = 0, ps_constructors = newMap, ps_functions = newMap, ps_CAFs = newMap}

factor [TIdentifier name:ts] = getLevel >>= \level = returnS (Just (SName name level), ts)
factor [TConst const:ts] = returnS (Just (SConst const), ts)
factor [TOpenParenthesis:ts] = 
				application ts
			>>=	\(t, ts) = case hd ts of
						TCloseParenthesis = returnS (Just t, tl ts)
										  = returnE (ts, "Missing close parenthesis")
factor ts = returnS (Nothing, ts)

application [TOpenParenthesis:ts] = 
				application ts
			>>=	\(t, ts) = case hd ts of
						TCloseParenthesis = returnS (t, tl ts)
										  = returnE (ts, "Missing close parenthesis")

application ts = 
				factor ts 
			>>= \(t, ts) = case t of 
					Just t = returnS (t, ts)
						   = returnE (ts, "Invalid application")
			>>= \(t,  ts) = args_factor ts
			>>= \(as, ts) = case as of
								[] = returnS (t, ts) // !!!
								   = returnS (SApplication t as, ts)

selectexpr [TCaseKeyword:ts] = 
				arg_adv ts 
			>>= mandatory "Missing predicate"
			>>= \(pred, ts) = arg_adv ts
			>>= mandatory "Missing left hand side"
			>>= \(lhs, ts) = arg_adv ts
			>>= mandatory "Missing right hand side"			
			>>= \(rhs, ts) = returnS (Just (SCase pred lhs rhs), ts)
				
selectexpr [TSelectKeyword:ts] = 
				args_adv ts
			>>= \(as, ts) = returnS (Just (SSelect as), ts)
			
selectexpr ts = returnS (Nothing, ts)

mainexpr ts = selectexpr ts 
			>>= \(t, ts) = case t of
								Just t = returnS (t, ts)
						   			   = application ts

letdefinitions ts = letdef_1 ts []
where
	letdef_1 [TIdentifier name, TAssignmentOp:ts] as = 
				getLevel
			>>= \level = application ts 
			>>= \(t, ts) = letdef_2 ts [SLetDefinition (SName name level) t:as]
	letdef_1 ts as = returnE (ts, "Invalid \"let\" definition")
	letdef_2 [TColon: ts] as = letdef_1 ts as
	letdef_2 ts as = returnS (reverse as, ts)

body [TLetKeyword:ts] =
				incLevel ts
			>>= \ts = letdefinitions ts
			>>= \(ds, ts) = case hd ts of
					TInKeyword = returnS (tl ts)
							   = returnE (ts, "Missing \"in\" keyword")
			>>= \ts = mainexpr ts
			>>= \(t, ts) = returnS (SLet t ds, ts)
			>>= decLevel
	
body [TOpenBracket:ts] = skip ts	// ABC code: skip it	
where
	skip [TCloseBracket:ts] = returnS (SAbortBody, ts)
	skip [] = returnE ([], "Missing close bracket in ABC code definition")
	skip [t:ts] = skip ts
			
body ts = mainexpr ts

lambdaexpr [TLambda:ts] =
			incLevel ts		
		>>= \ts = args ts
		>>= \(as, ts) = case hd ts of
				TAssignmentOp = body (tl ts)
							  = returnE (ts, "Missing asignment operator")
		>>= \(t, ts) = returnS (Just (SLambda t as), ts)							  
		>>= decLevel

lambdaexpr ts = returnS (Nothing, ts)

args_factor ts = args_ ts []
where
	args_ ts as = factor ts 
				>>= \(t, ts) = case t of
						Just r = args_ ts [r:as]
							   = returnS (reverse as, ts)

args_adv ts = args_ ts []
where
	args_ ts as = arg_adv ts 
				>>= \(t, ts) = case t of
						Just r = args_ ts [r:as]
							   = returnS (reverse as, ts)

arg_adv [TOpenParenthesis:ts] = 
				lambdaexpr ts
			>>= \(t, ts) = case t of
					Just _ = returnS (t, ts)
						   = body ts >>= \(t, ts) = returnS (Just t, ts)
			>>=	\(t, ts) = case hd ts of
						TCloseParenthesis = returnS (t, tl ts)
										  = returnE (ts, "Missing close parenthesis")
		
arg_adv ts = factor ts

args ts = args_ ts []
where
	args_ [TIdentifier name:ts] as = getLevel >>= \level = args_ ts [SName name level:as] 
	args_ ts as = returnS (reverse as, ts)

args_annotated ts = args_ ts []
where
	args_ [TIdentifier name:ts] as = getLevel >>= \level = args_ ts [SName name level:as] 
	args_ [TStrictIdentifier name:ts] as = args_ ts [SStrictName name:as]
	args_ ts as = returnS (reverse as, ts)

args_record ts = args_1 ts []
where
	args_1 [TIdentifier name:ts] as = getLevel >>= \level = args_2 ts [SName name level:as]	
	args_1 ts as = returnE (ts, "Missing argument")
	args_2 [TColon:ts] as = args_1 ts as
	args_2 ts as = returnS (reverse as, ts)

args_adt ts = args_1 ts [] 0
where
	args_1 [TIdentifier name:ts] cs i = 
			getLevel 
		>>= \level = args ts 
		>>= \(ss,ts) = addConstructor (SName name level) ss >>= \tname = args_2 ts [SConstructor tname i ss:cs] i
		
	args_1 ts cs _ = returnE (ts, "Missing argument")
	args_2 [TVerticalBar:ts] cs i = args_1 ts cs (i+1)
	args_2 ts cs _ = returnS (reverse cs, ts)

// record
constr [TTypeDef, TIdentifier name, TAssignmentOp, TOpenBracket: ts] =
				getLevel
			>>= \level = args_record ts
			>>= \(as, ts) = case hd ts of
						TCloseBracket = addConstructor (SName name level) as >>= \tname = returnS (FTRecord tname as, tl ts)
									  = returnE (ts, "Missing close parenthesis3")

// ADT
constr [TTypeDef, TIdentifier name, TAssignmentOp: ts] =
				getLevel
			>>= \level = args_adt ts 
			>>= \(as,ts) = returnS (FTADT (SName name level) as, ts)

constr [TTypeDef:ts] = returnE (ts, "Invalid type definition")
constr ts = returnE (ts, "Not a type definition")

func [TIdentifier name, TCAFAssignmentOp:ts] = 
				getLevel
			>>= \level = body ts
			>>= \(t, ts) = addCAF (SName name level) >>= \tname = returnS (FTCAF tname t, ts)
			
func [TIdentifier name:ts] = 
				getLevel
			>>= \level = args_annotated ts 
			>>= \(as, ts) = case hd ts of
					TAssignmentOp	   = returnS (True, tl ts)
					TMacroAssignmentOp = returnS (False, tl ts)					
									   = returnE (ts, "Missing assignment operator")
			>>= \(func, ts) = body ts 
			>>= \(t, ts) = if func 
							(addFunction (SName name level) as >>= \tname = returnS (FTFunc tname t as, ts))
							(addFunction (SName name level) as >>= \tname = returnS (FTMacro tname t as, ts))

func ts=:[TTypeDef:_] = constr ts >>= \(f,ts) = returnS (f, ts)
func ts = returnE (ts, "Not a function or type definition")

skip_newlines [TEndOfLine:ts] = skip_newlines ts
skip_newlines ts = returnS ts

program ts fs =
			skip_newlines ts
		>>= \ts = func ts
		>>= \(f, ts) = skip_newlines ts
		>>= \ts = if (length ts == 0) (returnS ([f:fs], ts)) (program ts [f:fs])
		
parse :: [PosToken] -> MaybeError ErrorMsg ([FuncType],ParserState)
parse pts 
	# ts = map (\(PosToken _ _ t) = t) pts
	= case (program ts []) defaultState of
				Ok ((fts, _),ps) = Ok (fts,ps)
				Error (ts, msg)  = let (lp, cp) = findpos ts in Error (msg+++" at line "+++toString lp+++" before charachter "+++toString cp)
where
	findpos rest_ts 
		# rest_pts = drop ((length pts)-(length rest_ts)-1) pts
		= case hd rest_pts of
			PosToken lp cp _ = (lp, cp)
		
parseExpr :: [PosToken] -> MaybeError ErrorMsg (SaplTerm,ParserState)		
parseExpr pts 
	# ts = map (\(PosToken _ _ t) = t) pts
	= case (body ts) defaultState of
				Ok ((fts, _),ps) = Ok (fts,ps)
				Error (ts, msg)  = let (lp, cp) = findpos ts in Error (msg+++" at line "+++toString lp+++" before charachter "+++toString cp)
where
	findpos rest_ts 
		# rest_pts = drop ((length pts)-(length rest_ts)-1) pts
		= case hd rest_pts of
			PosToken lp cp _ = (lp, cp)
		
mergeParserStates :: ParserState (Maybe ParserState) -> ParserState
mergeParserStates pst1 (Just pst2)
	= {pst1 &
	   ps_constructors = mergeMaps pst1.ps_constructors pst2.ps_constructors,
	   ps_functions    = mergeMaps pst1.ps_functions    pst2.ps_functions,
	   ps_CAFs         = mergeMaps pst1.ps_CAFs         pst2.ps_CAFs}
where
	mergeMaps m1 m2 = putList (toList m2) m1

mergeParserStates pst1 Nothing = pst1

