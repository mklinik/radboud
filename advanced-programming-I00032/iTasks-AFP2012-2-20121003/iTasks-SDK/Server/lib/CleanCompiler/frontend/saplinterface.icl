	implementation module saplinterface

import StdEnv, CoclSystemDependent, syntax, partition, gensapl

// Generation of Sapl definition from Clean definitions
// JMJ: May 2007 

gensaplfiles :: !Files {#DclModule} {#{#CheckedTypeDef}} !*{!Component} !*{# FunDef} CommonDefs {#CommonDefs} Ident  [IndexRange] !String
                -> (!Files,!*{!Component}, !*{# FunDef})
gensaplfiles files dcl_mods types components fun_defs icl_common common_defs icl_name icl_function_indices csf_path
	# (ok, files)
		= ensureCleanSystemFilesExists csf_path files
	| not ok
		= abort ("can't create folder \"" +++ csf_path +++"\"\n")
	
	# gfp_path = csf_path +++ {DirectorySeparator} +++ toString icl_name +++ ".gfp"	  
	# (ok,f,files) = fopen gfp_path FWriteText files 
	| not ok
		= abort ("can't create file \"" +++ gfp_path +++"\"\n")	
	
	# deps_path = csf_path +++ {DirectorySeparator} +++ toString icl_name +++ ".deps"
	# (ok,fdeps,files) = fopen deps_path FWriteText files 
	| not ok
		= abort ("can't create file \"" +++ deps_path +++"\"\n")
	
	# modnames = getModNames dcl_mods // modules used by this Clean module
 	# (components,fun_defs,f,fdeps) = convert2sapl components fun_defs icl_common common_defs icl_name f fdeps modnames (toString icl_name) dcl_mods icl_function_indices
	  (ok,files) = fclose f files
	| ok<>ok 		= abort "";
	# (ok,files) = fclose fdeps files 
	| ok<>ok 		= abort "";
	= (files,components,fun_defs) 

convert2sapl :: !*{!Component} !*{# FunDef} CommonDefs {#CommonDefs} Ident !*File !*File [String] String {#DclModule} [IndexRange] -> (!*{!Component}, !*{# FunDef},!*File,!*File)
convert2sapl comps fun_defs icl_common comdefs icl_name file fdeps modnames mymod dcl_mods icl_function_indices 
  # saplcons  = getSaplConstructors mymod icl_common        // only this module
  # extcons  = getExternalConstructors modnames comdefs     // all including this module!
  # saplrecs  = getSaplRecords  icl_common mymod
  # saplcons = remRecs saplcons saplrecs
  # (comps,fun_defs,saplfuncs) = getSaplFunDefs comps 0 fun_defs modnames mymod dcl_mods icl_function_indices
  # saplfuncs = convertSelects saplfuncs extcons           // convert clean like select to Sapl select
  # saplfuncs = map renameVars saplfuncs                   // give vars a unique name
  # saplfuncs = flatten (map checkIfSelect saplfuncs)      // extract non toplevel if/select
  # fundeps   = genFunDepends saplfuncs saplcons saplrecs  // for linking
  # file = file <<< "|| Generated Sapl file for " <<< icl_name <<< ".icl\n"
  # file = file <<< "\n\n"
  # file = writedef2file saplfuncs file
  # file =  writedef2file (consgroups saplcons) (file <<< "\n")
  # file = writerecs2file saplrecs (file <<< "\n|| Converted Records\n")
  # fdeps = writedeps2file fundeps (fdeps <<< "|| Function dependencies\n")
  = (comps,fun_defs,file,fdeps)
where
  consgroups conss = [":: " +++ (\ (SaplConsDef mod t _ _ _ _) = mod +++ "_" +++ t) (hd a) +++ " = " +++ makeConsGroup a \\ a <- group eqcg conss] 
  eqcg (SaplConsDef a1 a2 _ _ _ _) (SaplConsDef b1 b2 _ _ _ _) = a1 == b1 && a2 == b2
  group f [] = []
  group f [x:xs] = let (as,bs) = span (f x) xs in [[x:as] : group f bs]  
  writedef2file [] file = file
  writedef2file [f:fs] file = writedef2file fs (file <<< toString f <<< "\n")
  writerecs2file [] file = file
  writerecs2file [f:fs] file = writerecs2file fs (file <<< toString f <<< "\n")
  makeString [] = ""
  makeString [f:fs] = f +++ " " +++ makeString fs
  writedeps2file [] file = file
  writedeps2file [(name,names):deps] file = writedeps2file deps (file <<< name <<< "\t\t" <<< mkString names <<< "\n") 
  mkString [] = ""
  mkString [a:as] = " " +++ a +++ mkString as
  remRecs cs recs = [SaplConsDef mod t name alt nrargs nralt \\ SaplConsDef mod t name alt nrargs nralt <- cs| not (isMember name recnames)]
  where recnames = [recname\\ SaplRecordDef mod recname fields <- recs]
  
makeConsGroup :: [SaplConsDef] -> String
makeConsGroup [     ]    = ""
makeConsGroup [arg]      = toString arg
makeConsGroup [arg:args] = toString arg +++ " | " +++ makeConsGroup args   

getSaplFunDefs :: !*{!Component} !Int  !*{# FunDef} [String] String {#DclModule} [IndexRange] -> (!*{!Component}, !*{# FunDef},[SaplFuncDef])
getSaplFunDefs comps comp_index fun_defs  mod_names mymod dcl_mods icl_function_indices
	| comp_index >= size comps
		= (comps,fun_defs,[])
		# (comp, comps) = comps![comp_index]
		# (fun_defs,saplfuncs) = show_component comp.component_members fun_defs []
		# (comps,fun_defs,sfuncs) = getSaplFunDefs comps (inc comp_index) fun_defs mod_names mymod dcl_mods icl_function_indices
		= (comps,fun_defs,saplfuncs ++ sfuncs)
where
	show_component NoComponentMembers fun_defs sapdefs
		= (fun_defs, sapdefs)
	show_component (ComponentMember fun funs) fun_defs sapdefs 
		# (fun_def, fun_defs) = fun_defs![fun]
		# saplfunc = CleanFunctoSaplFunc comp_index fun fun_def mod_names mymod dcl_mods icl_function_indices
		= show_component funs fun_defs [saplfunc:sapdefs]
	show_component (GeneratedComponentMember fun _ funs) fun_defs sapdefs 
		# (fun_def, fun_defs) = fun_defs![fun]
		# saplfunc = CleanFunctoSaplFunc comp_index fun fun_def mod_names mymod dcl_mods icl_function_indices
		= show_component funs fun_defs [saplfunc:sapdefs]

getExternalConstructors :: [String] {#CommonDefs} -> [SaplConsDef]			
getExternalConstructors mods comdefs = flatten [getSaplConstructors mod comdef\\(comdef,mod) <-  zip (lcomdefs,mods)]
where lcomdefs = [comdef\\ comdef <-: comdefs] 

getSaplConstructors :: String CommonDefs -> [SaplConsDef]			
getSaplConstructors mod icl_common = collectTypes [consdef\\ consdef <-: icl_common.com_cons_defs] 
where collectTypes conses = makeSaplCons  (group eqc (tosapl conses))
      tosapl conses = [(getConsType cons,getName cons,getNrArgs cons)\\ cons <- conses]
      makeSaplCons conses = [SaplConsDef mod type name alt nrarg (length css)\\ css <- conses, (alt,(type,name,nrarg)) <- zip ([1..],css)]
      group f [] = []
      group f [x:xs] = let (as,bs) = span (f x) xs in [[x:as] : group f bs]
      eqc (a,_,_) (b,_,_) = a == b
      getName cons = toString cons.cons_ident
      getNrArgs cons = length cons.cons_type.st_args //length cons.cons_exi_vars
      getConsType cons = (icl_common.com_type_defs).[cons.cons_type_index].td_ident.id_name 
      //getType cons = makeType cons (toString cons.cons_type.st_result.at_attribute)//a.td_ident
      //makeType cons "*" = (getConsType cons) // getName cons //mod +++ "*"   // **** for Record use the consname!!!
      //makeType cons x   = x
      
getSaplRecords :: CommonDefs String -> [SaplRecordDef]
getSaplRecords icl_common mymod = map makeRec [rectype\\ type <-: icl_common.com_type_defs, RecordType rectype <-  [type.td_rhs]]
where makeRec rectype = SaplRecordDef mymod (toString rectype.rt_constructor.ds_ident) [toString field.fs_ident\\ field <-: rectype.rt_fields]

getModNames :: {#DclModule} -> [String]
getModNames dcl_mods = [dcl_mod.dcl_name.id_name\\ dcl_mod <-: dcl_mods]

showConstructors :: *File CommonDefs -> *File
showConstructors file icl_common = file <<< [type.rt_constructor.ds_ident\\ consdef <-: icl_common.com_type_defs, RecordType type <-  [consdef.td_rhs]]

