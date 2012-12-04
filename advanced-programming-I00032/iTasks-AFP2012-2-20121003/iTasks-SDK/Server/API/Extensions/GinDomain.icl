implementation module GinDomain

from StdEnv import id

import iTasks, Text, HtmlUtil
import GinSyntax, GinFlowLibrary
import GinCompiler, GinParser

import GinORYX

gVisualizeText{|ORYXEditor|} _ _ = ["(ORYX editor: No textual representation available)"]
gVisualizeHtml{|ORYXEditor|} _ _ = [Text "(ORYX editor: No html representation available)"]
gVisualizeEditor{|ORYXEditor|} val vst = visualizeControlSimple (TUIORYXControl oryx.ORYXEditor.stencilset.ORYXStencilSetReference.url) val vst
where
	oryx = fromMaybe emptyORYXEditor val
	
instance toString ORYXEditor
where
	toString {diagram} = toString (toJSON diagram)

gUpdate{|ORYXEditor|} mode ust = basicUpdate mode parseUpdate emptyORYXEditor ust
where
	parseUpdate diagram orig = { ORYXEditor | orig & diagram = diagram }

gDefaultMask{|ORYXEditor|} _ = [Touched []]
gVerify{|ORYXEditor|} _ vst = alwaysValid vst
derive JSONEncode ORYXEditor
derive JSONDecode ORYXEditor
derive gEq ORYXEditor