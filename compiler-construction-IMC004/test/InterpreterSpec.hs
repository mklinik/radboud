{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module InterpreterSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck

import Parser
import Utils
import Interpreter

expr :: String -> Value
expr prog = eval emptyState $ runParser_ "" pExpr prog

testBinOp opSyntax opFunction resultConstructor =
  property $ \i1 i2 -> expr (show i1 ++ " " ++ opSyntax ++ " " ++ show i2) == resultConstructor (opFunction i1 i2)

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates integer constants" $ do
      property $ \x -> expr (show x) == I x
    it "evaluates integer constants" $ do
      expr "10" `shouldBe` I 10
      expr "-42" `shouldBe` I (-42)

    it "evaluates boolean constants" $ do
      property $ \x -> expr (show x) == B x

    it "evaluates (+)" $ testBinOp "+" (+) I
    it "evaluates (-)" $ testBinOp "-" (-) I
    it "evaluates (*)" $ testBinOp "*" (*) I
    it "evaluates (/)" $ property $ \i1 i2 -> i2 /= 0 ==> expr (show i1 ++ " / " ++ show i2) == I (div i1 i2)
    it "evaluates (%)" $ property $ \i1 i2 -> i2 /= 0 ==> expr (show i1 ++ " % " ++ show i2) == I (mod i1 i2)
    it "evaluates (<)" $ testBinOp "<" ((<)::(Integer -> Integer -> Bool)) B
    it "evaluates (>)" $ testBinOp ">" ((>)::(Integer -> Integer -> Bool)) B
    it "evaluates (<=)" $ testBinOp "<=" ((<=)::(Integer -> Integer -> Bool)) B
    it "evaluates (>=)" $ testBinOp ">=" ((>=)::(Integer -> Integer -> Bool)) B

    it "evaluates binary boolean OR " $ testBinOp "||" (||) B
    it "evaluates binary boolean AND" $ testBinOp "&&" (&&) B

    it "evaluates unary minus" $ property $ \i -> expr ("- (" ++ show i ++ ")") == I (-i)
    it "evaluates double negation" $ property $ \i -> expr ("--" ++ show i) == I i
    it "evaluates unary negation" $ property $ \i -> expr ("!" ++ show i) == B (not i)


  describe "State" $ do
    it "retrieves the most recently added identifier in a singleton state" $ do
      (("x" |-> I 10) emptyState $ "x") `shouldBe` I 10
    it "retrieves the most recently added identifier in a state with more elements" $ do
      (("x" |-> I 10) $ ("y" |-> B True) emptyState) "y" `shouldBe` B True
    it "retrieves an identifier somewhere inside the state" $ do
      (("x" |-> I 10) $ ("y" |-> B True) emptyState) "x" `shouldBe` I 10

main :: IO ()
main = hspec spec
