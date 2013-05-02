{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module BackendSsmSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck
import System.IO
import System.IO.Unsafe
import System.Process
import Control.Monad.Trans.State

import Parser
import Utils
import BackendSsm
import IntermediateRepresentation
import TestUtils
import Compile

main :: IO ()
main = hspec spec

testFilename :: String
testFilename = "test.ssm"

runSsm :: [String] -> IO [String]
runSsm asm = do
  withFile testFilename WriteMode (\f -> mapM_ (hPutStrLn f) asm)
  fmap lines $ readProcess "java" ["-jar", "ssm-nogui.jar", testFilename] []

runE :: String -> String
runE expr = head $ unsafePerformIO (runSsm $ generateE (evalState (exp2ir $ parse pExpr expr) ssmMachine) [])

run :: String -> [String]
run prog = unsafePerformIO $ runSsm $ unRight $ compileSsm "unittest" prog

testBinOp :: (Show a2, Show a1, Show a, Arbitrary a, Arbitrary a1) => ([Char], a -> a1 -> a2) -> ModifyParams
testBinOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testBinOpNonZero :: (Num a1, Ord a1, Show a2, Show a1, Show a, Arbitrary a, Arbitrary a1) => ([Char], a -> a1 -> a2) -> ModifyParams
testBinOpNonZero (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 (NonZero arg2) -> property $ runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testCompareOp :: (Show a1, Show a, Arbitrary a, Arbitrary a1) => ([Char], a -> a1 -> Bool) -> ModifyParams
testCompareOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (if op arg1 arg2 then machineTrue ssmMachine else machineFalse ssmMachine)

spec :: Spec
spec = do
  describe "local variables" $ do
    it "lots of local variables" $ run (unlines
      [ "Void main()"
      , "{"
      , "  var a = 1;"
      , "  var b = 2;"
      , "  var c = 3;"
      , "  var d = 4;"
      , ""
      , "  print(a + b);"
      , "  print(b + c);"
      , "  print(c + d);"
      , "  print(a + b + c + d);"
      , ""
      , "  return;"
      , "}"
      ]) `shouldBe` ["3", "5", "7", "10"]
    it "iterative factorial with local variable" $ run (unlines
      [ "Int facI(Int x) {"
      , "  var acc = 1;"
      , "  while(x > 0) {"
      , "    acc = acc * x;"
      , "    x = x - 1;"
      , "  }"
      , "  return acc;"
      , "}"
      , ""
      , "Void main() {"
      , " print(facI(0));"
      , " print(facI(1));"
      , " print(facI(2));"
      , " print(facI(3));"
      , " print(facI(4));"
      , " print(facI(5));"
      , " print(facI(6));"
      , " print(facI(7));"
      , "}"
      ]) `shouldBe` ["1", "1", "2", "6", "24", "120", "720", "5040"]

  describe "unary negate" $ do
    let unaryNegate x = "Bool unaryNegate(Bool x) { return !x; } Void main() { print(unaryNegate("++show x++")); }"
    it "unary negate True"  $ run (unaryNegate True) `shouldBe` [show $ machineFalse ssmMachine]
    it "unary negate False" $ run (unaryNegate False) `shouldBe` [show $ machineTrue ssmMachine]

  describe "unary minus" $ do
    let unaryMinus x = "Int unaryMinus(Int x) { return -x; } Void main() { print(unaryMinus("++show x++")); }"
    it "unary minus 1" $ run (unaryMinus 1) `shouldBe` ["-1"]
    it "unary minus -1" $ run (unaryMinus (-1)) `shouldBe` ["1"]
    it "unary minus 0" $ run (unaryMinus (0)) `shouldBe` ["0"]

  describe "while loops" $ do
    it "iterative countdown" $ run (unlines
      [ "Void countdown(Int x) {"
      , "  while(x > 0) {"
      , "    print(x);"
      , "    x = x - 1;"
      , "  }"
      , "}"
      , "Void main() { countdown(4); }"
      ]) `shouldBe` ["4", "3", "2", "1"]
    it "iterative factorial" $ run (unlines
      [ "Int facI(Int x, Int acc) {" -- no local variables yet
      , "  acc = 1;"
      , "  while(x > 0) {"
      , "    acc = acc * x;"
      , "    x = x - 1;"
      , "  }"
      , "  return acc;"
      , "}"
      , ""
      , "Void main() {"
      , " print(facI(0, 0));"
      , " print(facI(1, 0));"
      , " print(facI(2, 0));"
      , " print(facI(3, 0));"
      , " print(facI(4, 0));"
      , " print(facI(5, 0));"
      , " print(facI(6, 0));"
      , " print(facI(7, 0));"
      , "}"
      ]) `shouldBe` ["1", "1", "2", "6", "24", "120", "720", "5040"]

  describe "if-then-else" $ do
    it "branches to then branch" $ run "Void main() { if(True ) print(13); else print(23); return; }" `shouldBe` ["13"]
    it "branches to else branch" $ run "Void main() { if(False) print(13); else print(23); return; }" `shouldBe` ["23"]

  describe "recursive functions" $ do
    it "recursive countdown" $ run (unlines
      [ "Void countdown(Int x) { print(x); if(x <= 0) return; else countdown(x - 1); }"
      , "Void main() { countdown(10); }"
      ]) `shouldBe` (map show $ reverse ([0..10]::[Int]))
    it "recursive fac" $ run (unlines
      [ "Int fac(Int x) { if(x<=0) return 1; else return x * fac(x - 1); }"
      , "Void main() { print(fac(3)); }"
      ]) `shouldBe` ["6"]

  describe "main function" $
    it "print 42" $ run "Void main() { print(42); }" `shouldBe` ["42"]

  describe "access to function arguments" $ do
    it "identity function, Int, 3" $ run "a id(a x) { return x; } Void main() { print(id(3)); }" `shouldBe` ["3"]
    it "identity function, Int, -42" $ run "a id(a x) { return x; } Void main() { print(id(-42)); }" `shouldBe` ["-42"]
    it "identity function, Bool, True" $ run "a id(a x) { return x; } Void main() { print(id(True)); }" `shouldBe` ["-1"]
    it "identity function, Bool, False" $ run "a id(a x) { return x; } Void main() { print(id(False)); }" `shouldBe` ["0"]

    it "square function" $ run "Int square(Int x) { return x * x; } Void main() { print(square(3)); }" `shouldBe` ["9"]
    it "constant function" $ run "a const(a x, b y) { return x; } Void main() { print(const(42, True)); print(const(False, 100)); }"
      `shouldBe` ["42", "0"]

    it "assignments to function arguments" $ run (unlines
      [ "Int f(Int x, Bool b) { if(b) x = x + 1; else x = x - 1; return x; }"
      , "Void main() { print(f(5, True)); print(f(5, False)); }"
      ]) `shouldBe` ["6", "4"]

  describe "logical operators" $ do
    it "logical and" $ testCompareOp ("&&", (&&)::(Bool -> Bool -> Bool))
    it "logical or"  $ testCompareOp ("||", (||)::(Bool -> Bool -> Bool))

  describe "comparison" $ do
    it "equality (Int)"     $ testCompareOp ("==", (==)::(Int -> Int -> Bool))
    it "inequality (Int)"   $ testCompareOp ("!=", (/=)::(Int -> Int -> Bool))
    it "equality (Bool)"    $ testCompareOp ("==", (==)::(Bool -> Bool -> Bool))
    it "inequality (Bool)"  $ testCompareOp ("!=", (/=)::(Bool -> Bool -> Bool))
    it "less than"          $ testCompareOp ("<",  (<)::(Int -> Int -> Bool))
    it "less than or equal" $ testCompareOp ("<=", (<=)::(Int -> Int -> Bool))
    it "greater than"       $ testCompareOp (">",  (>)::(Int -> Int -> Bool))
    it "greater or equal"   $ testCompareOp (">=", (>=)::(Int -> Int -> Bool))

  describe "arithmetic" $ do
    it "addition"       $ testBinOp ("+", (+)::(Int -> Int -> Int))
    it "subtraction"    $ testBinOp ("-", (-)::(Int -> Int -> Int))
    it "multiplication" $ testBinOp ("*", (*)::(Int -> Int -> Int))
    it "division"       $ testBinOpNonZero ("/", (quot)::(Int -> Int -> Int))
    it "modulo"         $ testBinOpNonZero ("%", (rem)::(Int -> Int -> Int))
