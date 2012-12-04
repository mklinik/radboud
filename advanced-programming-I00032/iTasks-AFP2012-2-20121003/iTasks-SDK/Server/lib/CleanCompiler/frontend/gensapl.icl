implementation module gensapl

// Generation of Sapl definition from Clean definition
// JMJ: May 2007

import StdEnv, syntax, transform, StdDebug

instance toString SaplConsDef  
where toString (SaplConsDef mod t name alt nrargs nralt) = makePrintableName (mod +++ "_" +++ name) +++  makeString [" a"+++toString n\\ n <- [1..nrargs]]

instance toString SaplFuncDef  
where toString (SaplFuncDef name nrargs args body kind) = makePrintableName name +++ makeArgs args +++ toString kind +++ toString body

instance toString SaplRecordDef
where toString (SaplRecordDef mod recname fields) = makeGetSets mod  recname fields

instance toString FunKind
where toString FK_Macro = " :== "
      toString FK_Caf = " =: "
      toString x = " = "

instance == SaplConsDef
where == (SaplConsDef _ _ name1 _ _ _) (SaplConsDef _ _ name2 _ _ _) = name1 == name2
 
instance == SaplExp
where == (SaplVar n1 ip1 a1) (SaplVar n2 ip2 a2) = cmpvar (SaplVar n1 ip1 a1) (SaplVar n2 ip2 a2)
      == _ _ = False  // only used for comparing vars  !!

makeString :: [String] -> String
makeString [] = ""
makeString [a:as] = a +++ makeString as 

instance toString SaplExp
where 
 toString e                                 = exp2string False e
exp2string b (SaplApp left right)           = bracks b (exp2string False left +++ " " +++ exp2string True right)
exp2string b (SaplInt i)                    = toString i
exp2string b (SaplReal r)                   = toString r 
exp2string b (SaplBool v)                   = toString v
exp2string b (SaplChar c)                   = c 
exp2string b (SaplString s)                 = s
exp2string b (SaplFun f)                    = makePrintableName f // makeSaplSysFunc (makePrintableName f)
exp2string b (AnFunc [] e)                  = exp2string b e
exp2string b (AnFunc as e)                  = bracks b ("\\" +++ makeArgs as +++ " = " +++ exp2string False e)
exp2string b (SaplVar n vi a)               = makePrintableName n // debugging
exp2string b (SaplSelect e ps def)          = bracks b (selectToString (SaplSelect e ps def))
exp2string b (SaplLet ves body)             = "" +++ bracks b ("let " +++ multiLet ves body) 
exp2string b (SaplError m)                  = bracks b ("error \"" +++ m +++ "\"")
exp2string b (SaplABCCode cs)				= "{" +++ makeCodeString cs +++ "}"

bracks b e | b = "(" +++ e +++ ")" 
               = e
              
multiLet :: [(SaplExp,SaplExp)] SaplExp -> String
multiLet []        body      =  toString body // empty let
multiLet [(arg,e)] body      =  toString arg +++ " = " +++ toString e +++ " in " +++ toString body
multiLet [(arg,e):ves] body  =  toString arg +++ " = " +++ toString e +++ ", " +++ multiLet ves body

makeArgs :: [SaplExp] -> String
makeArgs [     ]    = ""
makeArgs [SaplVar arg _ a]      = " " +++ makePrintableAnnotatedName (toString arg) a
makeArgs [SaplVar arg _ a:args] = " " +++ makePrintableAnnotatedName (toString arg) a +++ makeArgs args 

makeCodeString :: [String] -> String
makeCodeString []     = ""
//makeCodeString [c]    = c
makeCodeString [c:cs] = c +++ ";" +++ makeCodeString cs 

// Converting Clean like selects (pre-transformed) to Sapl selects
convertSelects :: [SaplFuncDef] [SaplConsDef] -> [SaplFuncDef]
convertSelects funcs consdefs = flatten (map (convertSelect consdefs) funcs)
convertSelect consdefs (SaplFuncDef fname nrargs args body kind) 
 # (repbody,nrdefs,defaults) = replaceDefaults fname 0 args body
 = [SaplFuncDef fname nrargs args (select2func args repbody consdefs []) kind] ++ 
   [SaplFuncDef (fname+++"_def"+++toString k) (length vs) vs (select2func vs defbody consdefs []) kind\\(k,vs,defbody) <- defaults]

select2func args  (SaplSelect case_exp conses newdef) consdefs def 
  | isMatchCons conses = multiApp [SaplApp (SaplFun ("select")) case_exp:[ (findcons args name nrargs conses (newdef++ def) consdefs)  
                                                   \\SaplConsDef _ type name alt nrargs nralt <- myconstrs (getconstype (getConsName conses)consdefs)consdefs]]
                       = makeConstantPats args case_exp conses consdefs (newdef ++ def)

select2func args  (SaplApp left right)       consdefs def = SaplApp (select2func args  left consdefs def) (select2func args  right consdefs def)
select2func args  (AnFunc as e)              consdefs def = AnFunc as (select2func args  e consdefs def)
select2func args  (SaplLet ves body)         consdefs def = SaplLet ves (select2func args  body consdefs def)
select2func args  func                       consdefs def = func

myconstrs mytype consdefs  =   [SaplConsDef mod type name alt nrargs nralt\\SaplConsDef mod type name alt nrargs nralt <- consdefs| mytype==type]
getconstype consname  consdefs =  hd ([type \\SaplConsDef mod type name alt nrargs nralt <- consdefs| consname==name] ++ ["_notype"])
getConsName conses = hd ([name\\(MatchCons name,_,_) <- conses] ++ ["_nocons"])

findcons args  name nrargs [] []  consdefs   = SaplFun "nomatch"
findcons args  name nrargs [] def consdefs   | nrargs == 0 = hd def
                                                           = AnFunc [SaplVar ("_uv" +++ toString k) nilPtr SA_None\\ k <- [1..nrargs]] (hd def) 

findcons fargs  name nrargs [(MatchCons consname,args,e):conses] def consdefs | name == consname = AnFunc args (select2func (args++fargs)  e consdefs def)
                                                                                                 = findcons fargs  name nrargs conses def consdefs

isMatchCons cs =  [1\\(MatchCons _,_,_) <- cs] <> []

makeConstantPats args case_exp [] consdefs []                    = SaplError "No constant match for this case"
makeConstantPats args case_exp [] consdefs def                   = select2func args (hd def) consdefs (tl def)
makeConstantPats args case_exp [(pat,_,res):conses] consdefs def = multiApp [(makeSingleConstMatch case_exp pat),(select2func args res consdefs def),
                                                                               makeConstantPats args case_exp conses consdefs def]

makeSingleConstMatch case_exp (MatchInt val)    = SaplApp (SaplFun "if") (multiApp [SaplFun /*"eq"*/"StdInt_==_16",case_exp,SaplInt val])
makeSingleConstMatch case_exp (MatchString val) = SaplApp (SaplFun "if") (multiApp [SaplFun "StdString_==_2",case_exp,SaplString val])
makeSingleConstMatch case_exp (MatchChar val)   = SaplApp (SaplFun "if") (multiApp [SaplFun /*"eq"*/"StdChar_==_18",case_exp,SaplChar val])
makeSingleConstMatch case_exp (MatchReal val)   = SaplApp (SaplFun "if") (multiApp [SaplFun "StdReal_==_11",case_exp,SaplReal val])
makeSingleConstMatch case_exp (MatchBool val)   = SaplApp (SaplFun "if") (multiApp [SaplFun "StdBool_==_3",case_exp,SaplBool val])
makeSingleConstMatch case_exp MatchSingleIf     = SaplApp (SaplFun "if") case_exp
makeSingleConstMatch case_exp val  = SaplError "not implemented const match"

// Extract default definitions and replace them by call to extracted definition
replaceDefaults :: String Int [SaplExp] SaplExp -> (SaplExp,Int,[(Int,[SaplExp],SaplExp)])
replaceDefaults fname nr vs (SaplSelect case_exp conses defs)    
 # (repdefs,newnr,repdefdefaults) = multireplacedefaults fname (nr + length defs) vs defs [] []  
 # (repconses,newnr,repcondefaults) = replacesconses fname newnr vs conses [] []      
 # defaults = [(k,vs,def)\\(def,k) <- zip (repdefs,[nr..])] 
 # alldefs = defaults ++ repdefdefaults ++ repcondefaults
 # defcalls = [multiApp [SaplFun (fname+++"_def"+++toString (nr+k)): vs]\\ k <- [0..length defs-1]]
 = (SaplSelect case_exp repconses defcalls,newnr,alldefs)   
replaceDefaults fname nr vs (SaplApp left right)                 
 # (newleft,newnr,leftdefaults)  = replaceDefaults fname nr vs left 
 # (newright,newnr,rightdefaults)= replaceDefaults fname nr vs right
 = (SaplApp newleft newright,newnr,leftdefaults ++ rightdefaults)
replaceDefaults fname nr vs (AnFunc as body)        
 # (newbody,newnr,defaults) = replaceDefaults fname nr (as++vs) body
 = (AnFunc as newbody,newnr,defaults)
replaceDefaults fname nr vs (SaplLet ves body)              // right hand sides let may not contain selects!!   
 # (newbody,newnr,defaults)= replaceDefaults fname nr (map fst ves ++ vs) body
 = (SaplLet ves newbody,newnr,defaults)
replaceDefaults fname nr vs e   = (e,nr,[])

multireplacedefaults fname nr vs [] handled defaults = (handled,nr,defaults)
multireplacedefaults fname nr vs [todo:todos] handled defaults  
 # (reptodo,newnr,tododefaults) = replaceDefaults fname nr vs todo
 = multireplacedefaults fname newnr vs todos (handled++[reptodo]) (defaults++tododefaults)

replacesconses fname nr vs [] handled defaults = (handled,nr,defaults)
replacesconses fname nr vs [(match,lvs,e):conses] handled defaults 
 # (repcons,newnr,consdefaults) = replaceDefaults fname nr (lvs++vs) e
 = replacesconses fname newnr vs conses (handled++[(match,lvs,repcons)]) (defaults++consdefaults)
 
getDefaults :: Int Int [SaplExp] SaplExp -> [([SaplExp],SaplExp)]
getDefaults level nr vs (SaplSelect case_exp conses defs)    = [(vs,def)\\def <- defs] ++ flatten [getDefaults level nr vs def\\def <- defs] ++ 
                                                                flatten [getDefaults level nr (lvs++vs) e\\ (_,lvs,e) <- conses]
getDefaults level nr vs (SaplApp left right)                 = getDefaults level nr vs left ++ getDefaults level nr vs right
getDefaults level nr vs (AnFunc as e)                       = getDefaults level nr vs e
getDefaults level nr vs (SaplLet ves body)                  = getDefaults level nr vs body
getDefaults level nr vs _                                   = []

counterMap :: (a Int -> b) [a] Int -> [b]
counterMap f [] c = []
counterMap f [x:xs] c = [f x c : counterMap f xs (c+1)]

// Converting a single Clean function to a Sapl function (case is only pre-transformed)
CleanFunctoSaplFunc  :: Int Int FunDef  [String] String {#DclModule} [IndexRange] -> SaplFuncDef 
CleanFunctoSaplFunc modindex funindex 
                    {fun_ident,fun_body=TransformedBody {tb_args,tb_rhs},fun_info={fi_free_vars,fi_local_vars,fi_def_level,fi_calls},fun_type,fun_kind} 
                    mns mymod dcl_mods icl_function_indices
        = SaplFuncDef (mymod +++ "_" +++ makeFuncName -1 (getName fun_ident) 0 funindex dcl_mods icl_function_indices mymod mns) //(getName fun_ident) +++ toString funindex)   
                       (length tb_args) (counterMap (getFreeFuncArgName (getStrictnessList fun_type)) tb_args 0)  
                       (cleanExpToSaplExp tb_rhs) fun_kind

where
	cleanExpToSaplExp (Var ident) = getBoundVarName ident
	cleanExpToSaplExp (App {app_symb, app_args, app_info_ptr})
	        = case app_symb.symb_kind of
	            SK_Generic _ kind
	                -> printApplicGen app_symb kind   app_args  //  does not apply?
	            _   -> multiApp [SaplFun (getSymbName app_symb) : map cleanExpToSaplExp  app_args]
	cleanExpToSaplExp (f_exp @ a_exp)                                           = multiApp [cleanExpToSaplExp f_exp: map cleanExpToSaplExp a_exp]
	cleanExpToSaplExp (Let {let_info_ptr, let_strict_binds, let_lazy_binds, let_expr}) = SaplLet (orderlets (map letToSapl (let_strict_binds ++ reverse let_lazy_binds)))  (cleanExpToSaplExp let_expr)
	cleanExpToSaplExp (Case {case_expr,case_guards,case_default=No})            = genSaplCase case_expr case_guards []
	cleanExpToSaplExp (Case {case_expr,case_guards,case_default= Yes def_expr}) = genSaplCase case_expr case_guards [def_expr]
	cleanExpToSaplExp (BasicExpr basic_value)                                   = basicValueToSapl basic_value
	cleanExpToSaplExp (FreeVar var)                                             = getFreeVarName var
	cleanExpToSaplExp (Conditional {if_cond,if_then,if_else=No})                = SaplSelect (cleanExpToSaplExp if_cond) [(MatchSingleIf,[],cleanExpToSaplExp if_then)] []
	cleanExpToSaplExp (Conditional {if_cond,if_then,if_else=Yes else_exp})      = multiApp[SaplFun "if": map cleanExpToSaplExp  [if_cond,if_then,else_exp]]
	cleanExpToSaplExp (Selection _ expr selectors)                              = makeSelector  selectors (cleanExpToSaplExp expr)  
	cleanExpToSaplExp (Update expr1 selections expr2)                           = makeArrayUpdate (cleanExpToSaplExp expr1) selections (cleanExpToSaplExp expr2)  
	cleanExpToSaplExp (RecordUpdate cons_symbol expression expressions)         = makeRecordUpdate (cleanExpToSaplExp expression) expressions 
	cleanExpToSaplExp (TupleSelect cons field_nr expr)                          = SaplApp (SaplFun ("_predefined_tupsels" +++ toString cons.ds_arity +++ "v" +++ toString field_nr)) (cleanExpToSaplExp expr)
	cleanExpToSaplExp (MatchExpr cons expr)  |cons.glob_object.ds_arity == 1    = SaplApp (SaplFun ("_predefined_tupsels1v0"))(cleanExpToSaplExp expr) 
	                                                                            = cleanExpToSaplExp expr
	cleanExpToSaplExp EE                                                        = SaplError "no EE"  
	cleanExpToSaplExp (DynamicExpr {dyn_expr,dyn_type_code})                    = SaplError "no DynamicExpr"   
	cleanExpToSaplExp (TypeCodeExpression type_code)                            = SaplError "no TypeCodeExpression" 
	
	cleanExpToSaplExp (ABCCodeExpr code_sequence do_inline)                     = SaplABCCode code_sequence //"no ABCCodeExpr" 
	cleanExpToSaplExp (AnyCodeExpr input output code_sequence)                  = SaplError "no AnyCodeExpr" 
	
	cleanExpToSaplExp (FailExpr _)                                              = SaplError "no FailExpr" 
	
	cleanExpToSaplExp (ClassVariable info_ptr)                                  = SaplError "ClassVariable may not occur"
	cleanExpToSaplExp (NoBind _)                                                = SaplError "noBind may not occur" 
	cleanExpToSaplExp (Constant symb _ _)                                       = SaplError "Constant may not occur"
	cleanExpToSaplExp expr                                                      = SaplError "no cleanToSapl for this case"  


	printApplicGen app_symb kind args   = multiApp [SaplFun (getSymbName app_symb  +++ "_generic"):map cleanExpToSaplExp args]

	// Converting let expressions
	letToSapl lb = (getFreeVarName lb.lb_dst,cleanExpToSaplExp lb.lb_src)
	orderlets lts  =  lts 
	
	                                                
	// Array and Record updates
	makeArrayUpdate expr1 sels expr2  = SaplApp (makeSelector sels expr1) expr2
	
	makeSelector  [] e = e
	makeSelector  [selector:sels] e  = makeSelector  sels (mksel selector e)
	where mksel (RecordSelection globsel ind)     exp = SaplApp (SaplFun (mns !! globsel.glob_module  +++ "_get_" +++ toString globsel.glob_object.ds_ident)) e 
	      mksel (ArraySelection globsel _ e)      exp = multiApp [SaplFun (mns !! globsel.glob_module +++ "_" +++ toString globsel.glob_object.ds_ident +++ "_" +++ toString globsel.glob_object.ds_index),exp, cleanExpToSaplExp e]                             
	      mksel (DictionarySelection var sels _ e)exp = multiApp [makeSelector sels (getBoundVarName var),exp,cleanExpToSaplExp e]
	
	makeRecordUpdate expression [         ]                      = expression
	makeRecordUpdate expression [upbind:us] | not(isNoBind value)= makeRecordUpdate (multiApp [SaplFun (field_mod +++ "_set_" +++ field),expression,cleanExpToSaplExp value]) us
	                                                             = makeRecordUpdate expression us
	where field               = toString upbind.bind_dst.glob_object.fs_ident
	      index               = toString upbind.bind_dst.glob_object.fs_index
	      field_mod           = mns !! upbind.bind_dst.glob_module
	      value               = upbind.bind_src
	      isNoBind (NoBind _) = True
	      isNoBind _          = False

	// bitmap!
	getStrictnessList :: (Optional SymbolType) -> Int
	getStrictnessList (Yes {st_args_strictness}) = case st_args_strictness of
														NotStrict = 0
														Strict x = x
	getStrictnessList No = 0

	// It uses the stricness bitmap to extract annotation
	getFreeFuncArgName :: Int FreeVar Int -> SaplExp 
	getFreeFuncArgName strictness {fv_ident,fv_info_ptr,fv_count} c | ((bitand) strictness (1 << c)) > 0
                       = SaplVar (toString fv_ident) fv_info_ptr SA_Strict
	getFreeFuncArgName strictness {fv_ident,fv_info_ptr,fv_count} c
                       = SaplVar (toString fv_ident) fv_info_ptr SA_None
                                  
	getFreeVarName :: FreeVar -> SaplExp 
	getFreeVarName {fv_ident,fv_info_ptr,fv_count} = SaplVar (toString fv_ident) fv_info_ptr SA_None 
	                                                                                    
	ptrToString ptr = toString (ptrToInt ptr)
	
	getBoundVarName{var_ident,var_info_ptr,var_expr_ptr} = SaplVar (toString var_ident ) var_info_ptr SA_None
	
	getName :: Ident -> String
	getName {id_name} = id_name 
	
	//getSymbName ::  SymbIdent -> String
	getSymbName symb=:{symb_kind = SK_Function symb_index } = printOverloaded symb.symb_ident  ( symb_index.glob_object) symb_index.glob_module
	getSymbName symb=:{symb_kind = SK_LocalMacroFunction symb_index }= printGeneratedFunction symb.symb_ident  ( symb_index)
	getSymbName symb=:{symb_kind = SK_GeneratedFunction _ symb_index }= printGeneratedFunction symb.symb_ident  ( symb_index)
	getSymbName symb=:{symb_kind = SK_LocalDclMacroFunction symb_index }= printOverloaded symb.symb_ident  ( symb_index.glob_object) symb_index.glob_module//= file <<< symb.symb_ident <<<  "[ldm]@" <<< symb_index
	getSymbName symb=:{symb_kind = SK_OverloadedFunction symb_index }= printOverloaded symb.symb_ident  ( symb_index.glob_object) symb_index.glob_module//=  file <<< symb.symb_ident <<<  "[o]@" <<< symb_index
	getSymbName symb=:{symb_kind = SK_Constructor symb_index } = printConsName symb.symb_ident  ( symb_index.glob_object) symb_index.glob_module
	getSymbName symb             = getName symb.symb_ident 
	
	printGeneratedFunction symbol symb_index  = decsymbol (toString symbol)
	where decsymbol s                         = mymod +++ "_"  +++ makeFuncName 0 s 0 symb_index dcl_mods icl_function_indices mymod mns 
//	where decsymbol s                         = mymod +++ "_"  +++ makeName s   +++ symb_index 
	
	printOverloaded symbol symb_index modnr   = decsymbol (toString symbol)
	where decsymbol s | startsWith "c;" s     = mymod +++ "__lc_"  +++ toString symb_index 
	                  | startsWith "g_c;" s   = mymod +++ "__lc_"  +++ toString symb_index 
	                                          = makemod modnr +++ makeFuncName 0 s modnr symb_index dcl_mods icl_function_indices  mymod mns
//	                                          = makemod modnr +++ makeName s   +++ toString symb_index 
	printConsName symbol symb_index modnr     = makemod modnr +++  toString symbol
	
	getmodnr sym = sym.glob_module
	makemod n =  mns!! n +++ "_"
	
	// Converting Case definitions
	genSaplCase case_exp (AlgebraicPatterns gindex pats) def = SaplSelect (cleanExpToSaplExp case_exp) (map getCasePat pats) (map cleanExpToSaplExp def) 
	genSaplCase case_exp (BasicPatterns gindex pats)    def  = SaplSelect (cleanExpToSaplExp case_exp) (map getConstPat pats) (map cleanExpToSaplExp def)
	genSaplCase case_exp (OverloadedListPatterns listtype exp pats)  def  = SaplSelect (cleanExpToSaplExp case_exp) (map getCasePat pats) (map cleanExpToSaplExp def) 
	genSaplCase case_exp  other                         def  = SaplError "no matching rule found" 
	
	getCasePat pat = (MatchCons (toString pat.ap_symbol.glob_object.ds_ident), map getFreeVarName pat.ap_vars,cleanExpToSaplExp pat.ap_expr)
	getConstPat pat = (basicValueToMatchSapl pat.bp_value, [], cleanExpToSaplExp pat.bp_expr)
	
basicValueToSapl :: BasicValue -> SaplExp
basicValueToSapl (BVI int)      = SaplInt (toInt int)
basicValueToSapl (BVInt int)    = SaplInt int
basicValueToSapl (BVC char)     = SaplChar (char)
basicValueToSapl (BVB bool)     = SaplBool bool
basicValueToSapl (BVR real)     = SaplReal (toReal real)
basicValueToSapl (BVS string)   = SaplString string
	
basicValueToMatchSapl (BVI int)     = MatchInt (toInt int)
basicValueToMatchSapl (BVInt int)   = MatchInt int
basicValueToMatchSapl (BVC char)    = MatchChar ( char)
basicValueToMatchSapl (BVB bool)    = MatchBool bool
basicValueToMatchSapl (BVR real)    = MatchReal (toReal real)
basicValueToMatchSapl (BVS string)  = MatchString string

cmpvar  (SaplVar n1 ip1 a1) (SaplVar n2 ip2 a2) | isNilPtr ip1 || isNilPtr ip2 = n1 == n2
	                                                                           = ip1 == ip2

// Printing select, they do not occur in final Sapl definitions!!
selectToString (SaplSelect e ps def) = "select " +++ exp2string True e +++ " " +++ dopats ps  +++ dodef def
where dopats [] = ""
      dopats [(MatchCons name,vars,exp):pats] = "(" +++ name +++ makeString [" "+++arg\\ SaplVar arg _ _<- vars] +++ " -> " +++ toString exp +++ ") "
                                                 +++ dopats pats
      dodef [] = ""
      dodef [def] = "(default -> " +++ toString def +++ ")"
      
getVarPrefix varname  =toString (takeWhile (\a -> a <> 'I' && a <> ';') lname)
where lname = [c\\c <-: varname]      
	
renameVars :: SaplFuncDef -> SaplFuncDef
renameVars (SaplFuncDef name nrargs args body kind) 
 # renargs = renamevars args 0 
 = SaplFuncDef name nrargs (map snd renargs) (doVarRename 1 renargs body) kind

renamevars vars 0 = [(SaplVar v ip a,SaplVar (getVarPrefix v +++ "_" +++ toString k) ip a)\\ (SaplVar v ip a,k) <- zip(vars,[0..])]
renamevars vars n = [(SaplVar v ip a,SaplVar (getVarPrefix v +++ "_" +++ toString n +++ "_" +++ toString k) ip a)\\ (SaplVar v ip a,k) <- zip(vars,[0..])]

doVarRename level rens (SaplApp left right)                  = SaplApp (doVarRename level rens left) (doVarRename level rens right)
doVarRename level rens (AnFunc as e)                         = AnFunc (map snd renargs) (doVarRename (level+1) (renargs++rens) e) where renargs = renamevars as level
doVarRename level rens (SaplVar n ip a)                      = findvar (SaplVar n ip a) rens
doVarRename level rens (SaplLet ves body)                    = doletrename level rens [] ves body
doVarRename level rens e                                     = e

doletrename level rens _ ves body 
 # renletvars = renamevars [v\\(v,_) <- ves] level
 # renletbodies = [doVarRename (level+1) (renletvars++rens) b\\(_,b) <- ves]
 # renlets = [(rv,b)\\ ((v,rv),b) <- zip (renletvars,renletbodies)]
 = removeVarBodyLets (SaplLet renlets (doVarRename (level+1) (renletvars++rens) body))

// Sapl does not allow let's with only a var on the right hand side
removeVarBodyLets (SaplLet ves body) 
 # rens = varbodies ves
 # novars = nonvarbodies ves
 # (SaplLet ves body) = varrename rens (SaplLet novars body)
 | ves == [] = body  // no lets left
             = SaplLet ves body

varbodies ves = [(v,SaplVar n ip a)\\ (v,SaplVar n ip a) <- ves]
nonvarbodies ves = [(v,e)\\ (v,e) <- ves| noVar e]
noVar (SaplVar _ _ _) = False
noVar _               = True

// Simple var renaming
varrename rens (SaplApp left right)                  = SaplApp (varrename rens left) (varrename rens right)
varrename rens (AnFunc as e)                         = AnFunc as (varrename rens e)
varrename rens (SaplVar n ip a)                      = findvar (SaplVar n ip a) (rens++[(SaplVar n ip a,SaplVar n ip a)])
varrename rens (SaplLet ves body)                    = SaplLet [(v,varrename rens e)\\ (v,e) <- ves] (varrename rens body)
varrename rens e                                     = e

findvar (SaplVar n ip a) rens = hd ([renvar\\ (var,renvar) <- rens| cmpvar (SaplVar n ip a) var]++[SaplVar ("error, " +++ n +++ " not found") nilPtr SA_None])

makeFuncName current_mod name mod_index func_index dcl_mods ranges mymod mns
              | name.[0] == '\\' = "anon_" +++ toString func_index
              | startsWith "c;" name = "_lc_" +++ toString func_index
              | startsWith "g_" name = "_lc_" +++ toString func_index
                                     = genFunctionExtension  current_mod name mod_index func_index dcl_mods ranges mymod mns
                                 
makeName name | name.[0] == '\\' = "anon_" 
              | startsWith "c;" name = "_lc_"
              | startsWith "g_" name = "_lc_"
                                     = name +++ "_"
                                 
multiApp [a]       = a
multiApp [a:b:as]  = multiApp [(SaplApp a b): as]        

makeMultiIf eqf var [] def = def
makeMultiIf eqf var [(cond,iff):iffs] def = multiApp [SaplFun "if",multiApp[eqf,var,cond] ,iff, makeMultiIf eqf var iffs def]

startsWith :: String String -> Bool
startsWith s1 s2 = s1 == s2%(0,size s1-1)
                                 
// Record access defintions
makeGetSets mod recname fields = ":: " +++ makePrintableName (mod +++ "_" +++ recname) +++ " = {" +++ makeconsargs (map ((+++) (mod +++ "_")) fields) +++ "}\n" +++
                                 mGets 1 (length fields) fields +++ mSets 1  (length fields) fields
where
 mGets _ _ [] = ""
 mGets k nf [field:fields] = makePrintableName (mod +++ "_get_" +++ field) +++ " rec = select rec (\\" +++ makeargs nf +++ " = a" +++ toString k +++ ")\n" +++ mGets (k+1) nf fields
 mSets _ _ [] = ""
 mSets k nf [field:fields] = makePrintableName (mod +++ "_set_" +++ field) +++ " rec val = select rec (\\" +++ makeargs nf +++ " = " +++ 
                             makePrintableName (mod +++ "_" +++ recname) +++ " " +++ makerepargs k nf +++ ")\n"  +++ mSets (k+1) nf fields
 makeconsargs [     ]  = ""
 makeconsargs [a]      = makePrintableName a
 makeconsargs [a:args] = makePrintableName a +++ ", " +++ makeconsargs args 
 
 makeargs 0 = ""
 makeargs n = makeargs (n-1) +++ " a" +++ toString n 
 
 makerepargs _ 0 = ""
 makerepargs k n | k == n = makerepargs k (n-1) +++ " val"
                          = makerepargs k (n-1) +++ " a" +++ toString n

makePrintableAnnotatedName :: String SaplAnnotation -> String
makePrintableAnnotatedName f SA_None = makePrintableName f
makePrintableAnnotatedName f SA_Strict = "!" +++ makePrintableName f

makePrintableName f | ss f                                   = "<{" +++ f +++ "}>"
                         | startsWith "_predefined__Cons" f  = "cons"
                         | startsWith "_predefined__Nil"   f = "nil" 
                                                             = f
where ss f = or [is_ss c\\ c <-: f]
      is_ss c = not (isAlphanum c || c == '_')          
      

genFunDepends :: [SaplFuncDef] [SaplConsDef]  [SaplRecordDef] -> [(String,[String])]   
genFunDepends fundefs consdefs recdefs = map (\(n,ns) -> (makePrintableName n, map makePrintableName ns))(  initdeps) ++ 
                                         [(makePrintableName (mod +++ "_" +++ consname),[])\\ SaplConsDef mod _ consname _ _ _ <- consdefs]++
                                         [(makePrintableName (mod +++ "_" +++ consname),[])\\ SaplRecordDef mod consname _ <- recdefs]++
                                         [(makePrintableName (mod +++ "_" +++ getset),[makePrintableName (mod +++ "_" +++ recname)])\\ SaplRecordDef mod recname fields <- recdefs,
                                                                                                   field <- fields, 
                                                                                                   getset <- ["get_"+++ field, "set_"+++ field]]
where
 initdeps = map fdep fundefs
 fdep (SaplFuncDef name _ _ body _) = (name,removeDup (deps body))  
 deps  (SaplApp left right)     = deps   left ++ deps   right
 deps  (SaplFun name)   | isMember name  ["if","select"]= []
                                                        = [name] 
 deps  (SaplSelect e sels def)  = sort (deps e ++ flatten [deps se\\(_,_,se) <- sels] ++ flatten (map deps def))
 deps  (AnFunc as e)            = deps e
 deps  (SaplLet as body)        = flatten [deps  e\\ (_,e) <- as] ++ deps body
 deps   e                       = []


// Replace non toplevel if & select by a function call
checkIfSelect :: SaplFuncDef -> [SaplFuncDef]
checkIfSelect (SaplFuncDef fname nrargs vs body kind) 
 # (newbody,_,newdefs) = rntls True vs 0 body
 = [SaplFuncDef fname nrargs vs newbody kind:newdefs]
where 
 rntls top vs nr (AnFunc as body)  
 # (newbody,newnr,newdefs)  = rntls top (as++vs) nr body                           
 = (AnFunc as newbody,newnr,newdefs)

 rntls top vs nr (SaplLet ves body)   
 # (newbody,newnr,newdefs) = rntls False (map fst ves++vs) nr body                               
 = (SaplLet ves newbody,newnr,newdefs)

 rntls top vs nr exp=:(SaplApp _ _)
 # (name,node,args) = getFArgs exp []
 | name == "if" || name == "select"  = doIfSelect name top vs nr args
                                     = doFunc node vs nr args

 rntls top vs nr exp 
 = (exp,nr,[])

 doIfSelect name top vs nr [cond:te] 
 # (newcond,newnr,conddefs) = rntls False vs nr cond
 # (newte,newnr,tedefs)     = multiIfSelect top vs newnr te [] []
 | top = (multiApp [SaplFun name:newcond:newte],newnr,conddefs++tedefs) 
       = (multiApp [SaplFun (callname newnr):vs],newnr+1,conddefs++tedefs++
                                                       [SaplFuncDef (callname newnr) (length vs) vs (multiApp [SaplFun name:newcond:newte]) FK_Unknown])
 where callname newnr =  (fname+++"_select" +++ toString newnr)    
  
 doFunc node vs nr args
 # (newargs,newnr,newdefs) = multiIfSelect False vs nr args [] []
 = (multiApp [node:newargs],newnr,newdefs) 

 multiIfSelect top vs nr [] handled newdefs = (handled,nr,newdefs)
 multiIfSelect top vs nr [b:bs] handled newdefs 
  # (newb,newnr,bdefs) = rntls top vs nr b 
  = multiIfSelect top vs newnr bs (handled++[newb]) (newdefs ++ bdefs)
  
 getFArgs (SaplApp l r) args  = getFArgs l [r:args]
 getFArgs node=:(SaplFun n)   args  = (n,node,args)                 
 getFArgs node                args  = ("none",node,args)                 


// Which functions must be extended with a number 
genFunctionExtension current_mod name mod_index func_index dcl_mods ranges mymod mns
| current_mod == -1 || mns!!mod_index == mymod = genFunctionExtForMain name func_index ranges
//| mod_index == current_mod = genFunctionExtForMain name func_index ranges
                           = genFunctionExtForDCL name mod_index func_index dcl_mods
where                           
	genFunctionExtForDCL name mod_index func_index dcl_mods = gfn dcl_mods.[mod_index]
	where
	 	gfn {dcl_name, dcl_common, dcl_functions, dcl_type_funs, dcl_instances}
			=	functionName name func_index [{ir_from = 0, ir_to = dcl_instances.ir_from}, dcl_type_funs]                                                                           
	
	genFunctionExtForMain name func_index ranges = functionName name func_index ranges                                                                           
	
	functionName  name func_index ranges 
		| index_in_ranges func_index ranges
		=	name
		=	(name +++ "_" +++ toString func_index)
	
	index_in_ranges index [{ir_from, ir_to}:ranges] = (index>=ir_from && index < ir_to) || index_in_ranges index ranges
	index_in_ranges index [] = False

/*
saplsysfuncs = [("<{StdInt_+_6}>","add"),("<{StdInt_-_7}>","sub"),("StdInt_zero_8","0"),("<{StdInt_*_9}>","mult"),("<{StdInt_/_10}>","div"),
                ("StdInt_one_11","1"),("<{StdInt_==_16}>","eq"),("StdInt_rem_28","mod"),("<{StdChar_==_18}>","eq")]
                                                               
saplsysfuncs2 = [("StdInt_+_6","add"),("StdInt_-_7","sub"),("StdInt_zero_8","0"),("StdInt_*_9","mult"),("StdInt_/_10","div"),
                ("StdInt_one_11","1"),("StdInt_==_16","eq"),("StdInt_rem_28","mod"),("StdChar_==_18","eq")]

makeSaplSysFunc name = msf name saplsysfuncs
where                                                               
  msf name [] = name
  msf name [(n,sf):sfs] | name == n = sf
                                    = msf name sfs
*/


                                    
                     