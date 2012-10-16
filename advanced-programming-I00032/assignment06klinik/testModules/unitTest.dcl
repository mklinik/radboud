definition module unitTest

import StdEnv, gast

:: TestFun

doTest :: TestFun -> [String]

testEqual   :: String x x -> TestFun | gEq{|*|}, genShow{|*|} x
testUnEqual :: String x x -> TestFun | gEq{|*|}, genShow{|*|} x
testLess    :: String x x -> TestFun | gLess{|*|}, genShow{|*|} x
testLessEQ  :: String x x -> TestFun | gLess{|*|}, genShow{|*|} x
testOp      :: String x (x x->Bool) x -> TestFun | genShow{|*|} x

(`) infixl 0 :: TestFun TestFun -> TestFun

testPred1 :: String (x -> Bool) x -> TestFun | genShow {| * |} x
testPredL :: String (x -> Bool) [x] -> TestFun | genShow {| * |} x
