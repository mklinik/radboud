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

-- testBinOp :: (Show a, Show b) => (String, (a -> a -> b)) -> (a -> a -> Bool)
testBinOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testBinOpNonZero (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 (NonZero arg2) -> property $ runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testCompareOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (if op arg1 arg2 then machineTrue ssmMachine else machineFalse ssmMachine)

spec :: Spec
spec = do
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
