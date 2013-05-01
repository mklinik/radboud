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

runProg :: String -> [String]
runProg prog = unsafePerformIO (runSsm $ generateSs (evalState (program2ir $ parse pProgram prog) ssmMachine))

-- testBinOp :: (Show a, Show b) => (String, (a -> a -> b)) -> (a -> a -> Bool)
testBinOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testBinOpNonZero (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 (NonZero arg2) -> property $ runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testCompareOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (if op arg1 arg2 then machineTrue ssmMachine else machineFalse ssmMachine)

spec :: Spec
spec = do
  describe "main function" $
    it "print 42" $ runProg "Void main() { print(42); }" `shouldBe` ["42"]

  describe "access to function arguments" $ do
    it "identity function, Int, 3" $ runProg "a id(a x) { return x; } Void main() { print(id(3)); }" `shouldBe` ["3"]
    it "identity function, Int, -42" $ runProg "a id(a x) { return x; } Void main() { print(id(-42)); }" `shouldBe` ["-42"]
    it "identity function, Bool, True" $ runProg "a id(a x) { return x; } Void main() { print(id(True)); }" `shouldBe` ["-1"]
    it "identity function, Bool, False" $ runProg "a id(a x) { return x; } Void main() { print(id(False)); }" `shouldBe` ["0"]

    it "square function" $ runProg "Int square(Int x) { return x * x; } Void main() { print(square(3)); }" `shouldBe` ["9"]
    it "constant function" $ runProg "a const(a x, b y) { return x; } Void main() { print(const(42, True)); print(const(False, 100)); }"
      `shouldBe` ["42", "0"]

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
