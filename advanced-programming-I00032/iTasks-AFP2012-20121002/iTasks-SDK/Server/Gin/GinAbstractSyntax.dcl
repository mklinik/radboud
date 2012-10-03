definition module GinAbstractSyntax

import StdOverloaded
import GinTypes

from iTaskClass import class iTask, gVisualizeText, gVisualizeHtml, gVisualizeEditor, gUpdate, gDefaultMask, gVerify, JSONEncode, JSONDecode, gEq

from GinParser import ::GPath, ::GPathNode, ::GParseState
from GinPrinter import ::LineMap, ::Map
import Void

:: AModule = { name        :: AIdentifier
             , types       :: [GTypeDefinition]
             , definitions :: [ADefinition]
             , imports     :: [AImport]
             }
             
:: AImport :== String

:: ADefinition = { name         :: AIdentifier
                 , formalParams :: [GFormalParameter]
                 , returnType   :: GTypeExpression
                 , body         :: AExpression Void
                 , locals       :: [ADefinition]
                 }

:: Locals :== [ADefinition]
                 
:: AExpression ex = 
    Unparsed String
    | Lit String
    | Var AIdentifier
    | App [AExpression ex]
    | AppInfix AIdentifier AFix APrecedence (AExpression ex) (AExpression ex) 
    | Lambda APattern (AExpression ex)
    | Let 	 [(APattern, (AExpression ex))] (AExpression ex)
    | Case (AExpression ex) [ACaseAlt ex]
    | Tuple [AExpression ex]
    | List [AExpression ex]
    | ListComprehension (AListComprehension ex)
    | PathContext GPath (AExpression ex)
    | Extension ex

:: ACaseAlt ex = CaseAlt APattern (AExpression ex)

:: AListComprehension ex = { output :: (AExpression ex)
                           , generators :: (AGeneratorList ex)
                           , guards :: [AExpression ex]
                           }

:: AGeneratorList ex = NestedGeneratorList [AGenerator ex] | ParallelGeneratorList [AGenerator ex]

:: AGenerator ex = Generator APattern (AExpression ex)

:: APattern :== String

:: AIdentifier :== String

:: AFix = Infixl | Infixr | Infix
:: APrecedence :== Int

:: Vars :== [AIdentifier]

emptyVars :: Vars
addVar :: AIdentifier Vars -> Vars
addVars :: [AIdentifier] Vars -> Vars
mergeVars :: [Vars] -> Vars
inVars :: AIdentifier Vars -> Bool

:: Scope :== Vars
emptyScope :== emptyVars
bind :== addVar
inScope :== inVars

derive class iTask AModule, ADefinition, AExpression, ACaseAlt, AListComprehension, AGeneratorList, AGenerator, AFix

expandModule :: AModule -> AModule

:: PrintOption = PODCL | POICL | POSyntaxCheck | POWriteDynamics

instance == PrintOption

:: FunctionMap :== Map AIdentifier GPath

prettyPrintAModule :: PrintOption AModule -> String

syntaxCheckPrintAModule :: AModule -> (String, FunctionMap, LineMap)

