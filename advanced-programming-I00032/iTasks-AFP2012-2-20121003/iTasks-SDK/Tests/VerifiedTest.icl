implementation module VerifiedTest

import iTasks
import StdMisc
import Text

derive bimap (,),Maybe

// Verified Types

derive gVisualize PositiveNum
derive gUpdate PositiveNum
derive JSONDecode PositiveNum
derive JSONEncode PositiveNum
//derive gVerify PositiveNum

::PositiveNum = Positive Int

gVerify{|PositiveNum|} x vst = verifyConstructor (Just "Enter a positive number") (\(Positive x) -> x > 0) (\_ -> "You must enter a positive number") x vst
	
verifiedTest :: Task (Maybe PositiveNum)
verifiedTest = enterInformation "Enter number" "Enter a postive number" >>= showMessageAbout "Result" "The result is"

Start :: *World -> *World
Start world = startEngine [workflow "Verification Test" verifiedTest] world