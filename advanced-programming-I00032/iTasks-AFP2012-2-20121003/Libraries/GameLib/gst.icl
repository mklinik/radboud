implementation module gst

import ostoolbox

from StdFunc		import :: St
from StdIOBasic		import :: IdFun

::  *GSt gs
    =   { gs    :: gs
        , tb    :: !*OSToolbox
        }

fromGSt :: !*(GSt .gs) -> (.gs,!*OSToolbox)
fromGSt {gs,tb} = (gs,tb)

toGSt :: .gs !*OSToolbox -> *GSt .gs
toGSt gs tb = {gs=gs,tb=tb}

appGStTb :: !.(IdFun *OSToolbox) !*(GSt .gs) -> *GSt .gs
appGStTb f gst=:{tb} = {gst & tb=f tb}

accGStTb :: !.(St *OSToolbox .x) !*(GSt .gs) -> (!.x,!*GSt .gs)
accGStTb f gst=:{tb}
    # (x,tb)    = f tb
    = (x,{gst & tb=tb})

appGSt :: !.(IdFun .gs) !*(GSt .gs) -> *GSt .gs
appGSt f gst=:{gs} = {gst & gs=f gs}

accGSt :: !.(St .gs .x) !*(GSt .gs) -> (.x,!*GSt .gs)
accGSt f gst=:{gs}
    # (x,gs) = f gs
    = (x,{gst & gs=gs})
