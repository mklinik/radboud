definition module gensapl

import StdEnv,syntax,transform  
  
:: SaplAnnotation = SA_None | SA_Strict  
  
:: SaplExp = SaplApp SaplExp SaplExp | SaplInt Int |SaplReal Real | SaplBool Bool |
             SaplString String | SaplFun String | AnFunc [SaplExp] SaplExp | SaplVar String VarInfoPtr SaplAnnotation | 
             SaplChar String |
             SaplSelect SaplExp [(SaplMatchExp,[SaplExp],SaplExp)] [SaplExp] | SaplLet [(SaplExp,SaplExp)] SaplExp | 
             SaplError String | SaplABCCode [String]

::SaplConsDef = SaplConsDef String String String Int Int Int
::SaplFuncDef = SaplFuncDef String Int [SaplExp] SaplExp FunKind
::SaplRecordDef = SaplRecordDef String String  [String] 
::SaplMatchExp = MatchCons String | MatchInt Int | MatchChar String | MatchBool Bool | MatchReal Real | MatchString String | MatchSingleIf

instance toString SaplExp
instance toString SaplConsDef
instance toString SaplFuncDef
instance toString SaplRecordDef

//getSaplFunDefs :: !*{! Group} !Int  {# FunDef}  -> [SaplFuncDef]
//getSaplConstructors :: CommonDefs -> [SaplConsDef]			
convertSelects :: [SaplFuncDef] [SaplConsDef] -> [SaplFuncDef]
renameVars :: SaplFuncDef -> SaplFuncDef
checkIfSelect :: SaplFuncDef -> [SaplFuncDef]

CleanFunctoSaplFunc  :: Int Int FunDef  [String] String  {#DclModule} [IndexRange] -> SaplFuncDef

// Transform a multicase Expression with optional default to single function body
//select2func :: SaplExp [SaplConsDef] -> SaplExp
//checkFuncs :: [SaplFuncDef] [SaplConsDef] [SaplRecordDef]-> [String] 
genFunDepends :: [SaplFuncDef] [SaplConsDef] [SaplRecordDef] -> [(String,[String])]