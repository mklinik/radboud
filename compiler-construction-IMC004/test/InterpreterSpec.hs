{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module InterpreterSpec (spec, main) where

import Test.Hspec

import Parser
import Utils
import Interpreter

expr :: String -> Value
expr prog = eval emptyState $ runParser_ "" pExpr prog

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates binary operators" $
      expr "10" `shouldBe` I 10

main :: IO ()
main = hspec spec
