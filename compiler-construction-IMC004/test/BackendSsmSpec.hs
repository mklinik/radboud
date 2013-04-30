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

-- testBinOp :: (Show a, Show b) => (String, (a -> a -> b)) -> (a -> a -> Bool)
testBinOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testBinOpNonZero (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 (NonZero arg2) -> property $ runE (show arg1 ++ opStr ++ show arg2) == show (op arg1 arg2)

testCompareOp (opStr, op) = modifyQuickCheckMaxSuccess (const 10) $ property $
  \arg1 arg2 -> runE (show arg1 ++ opStr ++ show arg2) == show (if op arg1 arg2 then machineTrue ssmMachine else machineFalse ssmMachine)

spec :: Spec
spec = do
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
