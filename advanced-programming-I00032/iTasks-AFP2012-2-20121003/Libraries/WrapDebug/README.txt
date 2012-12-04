WrapDebug

Version  1.0.5
Ronny Wichers Schreur
ronny@cs.ru.nl

The WrapDebug package lets you print arbitrary expressions for debugging
purposes. The main functions are in Debug.


FILES

    README.txt
        This file
    CHANGES.txt
        Changes history
    Debug.dcl, Debug.icl
        Debug functions (uses Wrap and ShowWrapped)
    Wrap.dcl, Wrap.icl
        Wrap Clean nodes
    ShowWrapped.dcl, ShowWrapped.icl
        Convert a wrapped node to a list of strings
    Examples.icl, Examples.prj
        Some examples with explanations
	ClosureWrap.dcl, ClosureWrap.icl,
	ClosureShowWrapped.dcl, ClosureShowWrapped.icl
	ClosureDebug.dcl, ClosureDebug.icl
	ClosureExamples.icl, ClosureExamples.prj
		Experimental support for printing closures, only tested under Windows
		(originally implemented by Arjen van Weelden)