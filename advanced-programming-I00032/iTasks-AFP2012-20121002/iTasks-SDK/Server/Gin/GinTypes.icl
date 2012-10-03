implementation module GinTypes

import StdBool
import StdList
import GenEq
import Maybe, HTML, TUIDefinition

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, :: StaticVisualizationMode
from iTasks import class iTask, generic gVisualizeText, generic gVisualizeHtml, generic gVisualizeEditor, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

import GinPrinter

derive bimap (,)
derive bimap Maybe

derive class iTask      GTypeExpression, GTypeDefinition, GTypeRhs, GDataConstructor, GRecordField, GFormalParameter

typeIsDefined :: GTypeExpression -> Bool
typeIsDefined (GConstructor t)			= True
typeIsDefined (GList e)					= typeIsDefined e
typeIsDefined (GTuple es)				= all typeIsDefined es
typeIsDefined (GTypeApplication es)		= all typeIsDefined es
typeIsDefined (GTypeVariable v)			= True
typeIsDefined (GFunction a b)			= typeIsDefined a && typeIsDefined b
typeIsDefined GUndefinedTypeExpression	= False

printGTypeExpression :: Bool GTypeExpression -> a | Printer a
printGTypeExpression withParens (GConstructor t)			= text t
printGTypeExpression withParens (GList e)					= brackets (printGTypeExpression False e)
printGTypeExpression withParens (GTuple es)					= tupled (map (printGTypeExpression False) es)
printGTypeExpression withParens (GTypeApplication es)		= addParens withParens (fillSep (map (printGTypeExpression True) es))
printGTypeExpression withParens (GTypeVariable v)			= text v
printGTypeExpression withParens (GFunction e1 e2)			= addParens withParens (printGTypeExpression False e1 </> text "->" </> printGTypeExpression False e2)
printGTypeExpression withParens GUndefinedTypeExpression	= text "<<undefined type expression>>"

addParens :: Bool a -> a | Printer a
addParens withParens a = if withParens (parens a) a

printGTypeDefinition :: GTypeDefinition -> a | Printer a
printGTypeDefinition gt = def (	text "::" </> text gt.GTypeDefinition.name
                          		</> printGTypeRhs gt.GTypeDefinition.rhs
                          	  )

printGTypeRhs :: GTypeRhs -> a | Printer a                          
printGTypeRhs (GAlgebraicTypeRhs conss) = text "=" </> fillSep (punctuate (text "|") (map printGDataConstructor conss))
printGTypeRhs (GRecordTypeRhs fields)   = text "=" </> braces (fillSep ((punctuate comma (map printGRecordField fields))))
printGTypeRhs (GSynonymTypeRhs exp)     = text ":==" </> printGTypeExpression False exp
printGTypeRhs GAbstractTypeRhs          = empty

printGDataConstructor :: GDataConstructor -> a | Printer a
printGDataConstructor cons = text cons.GDataConstructor.name 
                         </> fillSep (map (printGTypeExpression True) cons.GDataConstructor.arguments)

printGRecordField :: GRecordField -> a | Printer a
printGRecordField field = text field.GRecordField.name
                          </> text "::" </> printGTypeExpression False field.GRecordField.type

instance toString GTypeExpression
where
	toString typeExp = prettyPrint (printGTypeExpression False typeExp)

isTask :: !GTypeExpression -> Bool
isTask (GTypeApplication [GConstructor "Task", _])	= True
isTask _										 	= False

gTask :: GTypeExpression -> GTypeExpression
gTask e = GTypeApplication [GConstructor "Task", e]

gVoid :: GTypeExpression
gVoid = GConstructor "Void"
