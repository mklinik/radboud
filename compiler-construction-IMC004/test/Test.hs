{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module Main (specs, main) where

import Test.Hspec
import Text.ParserCombinators.UU.BasicInstances (Parser)

import Parser
import Ast

parse :: (Parser a) -> String -> a
parse = runParser ""

specs :: Spec
specs = do
  describe "pExpr" $ do
    it "parses an integer" $
      parse pExpr "1" `shouldBe` AstInteger 1

    it "parses the boolean constant True" $
      parse pExpr "True" `shouldBe` AstBoolean True
    it "parses the boolean constant False" $
      parse pExpr "False" `shouldBe` AstBoolean False
    it "parses an identifier starting with True" $
      parse pExpr "Truee" `shouldBe` AstIdentifier "Truee"

    it "parses a tuple of Int and Bool" $
      parse pExpr "(10, True)" `shouldBe` AstTuple (AstInteger 10) (AstBoolean True)
    it "parses the empty list constant" $
      parse pExpr "[]" `shouldBe` AstEmptyList

    it "parses (*) with higher precedence than (:)" $
      parse pExpr "1 * 2 : []" `shouldBe` AstBinOp ":" (AstBinOp "*" (AstInteger 1) (AstInteger 2)) AstEmptyList
    it "parses (*) with higher precedence than (+)" $
      parse pExpr "1 + 2 * 3" `shouldBe` AstBinOp "+" (AstInteger 1) (AstBinOp "*" (AstInteger 2) (AstInteger 3))
    it "parses (/) with higher precedence than (-)" $
      parse pExpr "1 - 4 / 2" `shouldBe` AstBinOp "-" (AstInteger 1) (AstBinOp "/" (AstInteger 4) (AstInteger 2))
    it "parses (%) with higher precedence than (+)" $
      parse pExpr "1 + 2 % 0" `shouldBe` AstBinOp "+" (AstInteger 1) (AstBinOp "%" (AstInteger 2) (AstInteger 0))
    it "parses (+) with higher precedence than (<)" $
      parse pExpr "10 < 2 + 9" `shouldBe` AstBinOp "<" (AstInteger 10) (AstBinOp "+" (AstInteger 2) (AstInteger 9))

    it "parses (&&) with higher precedence than (||)" $
      parse pExpr "True || True && False" `shouldBe` AstBinOp "||" (AstBoolean True) (AstBinOp "&&" (AstBoolean True) (AstBoolean False))

    it "parses (+) left-associative" $
      parse pExpr "1 + 2 + 3 + 4" `shouldBe`
        AstBinOp "+" (AstBinOp "+" (AstBinOp "+" (AstInteger 1)
                                                 (AstInteger 2))
                                   (AstInteger 3))
                     (AstInteger 4)

    it "parses (*) left-associative" $
      parse pExpr "1 * 2 * 3 * 4" `shouldBe`
        AstBinOp "*" (AstBinOp "*" (AstBinOp "*" (AstInteger 1)
                                                 (AstInteger 2))
                                   (AstInteger 3))
                     (AstInteger 4)


main :: IO ()
main = hspec specs
