{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module Main (specs, main) where

import Test.Hspec
import Test.QuickCheck
import Text.ParserCombinators.UU.BasicInstances (Parser)

import Parser
import Ast

parse :: (Parser a) -> String -> a
parse = runParser ""

specs :: Spec
specs = do
  describe "pExpr" $ do
    it "various forms of identifiers" $
      property $ forAll (elements ["a", "a_", "a10", "a_", "TCP_IP_Connection", "Truee"]) $
        \i -> parse pExpr i == AstIdentifier i

    it "an integer" $
      parse pExpr "1" `shouldBe` AstInteger 1

    it "the boolean constant True" $
      parse pExpr "True" `shouldBe` AstBoolean True
    it "the boolean constant False" $
      parse pExpr "False" `shouldBe` AstBoolean False

    it "a tuple of Int and Bool" $
      parse pExpr "(10, True)" `shouldBe` AstTuple (AstInteger 10) (AstBoolean True)

    it "the empty list constant" $
      parse pExpr "[]" `shouldBe` AstEmptyList

    it "(*) with higher precedence than (:)" $
      parse pExpr "1 * 2 : []" `shouldBe` AstBinOp ":" (AstBinOp "*" (AstInteger 1) (AstInteger 2)) AstEmptyList
    it "(*) with higher precedence than (+)" $
      parse pExpr "1 + 2 * 3" `shouldBe` AstBinOp "+" (AstInteger 1) (AstBinOp "*" (AstInteger 2) (AstInteger 3))
    it "(/) with higher precedence than (-)" $
      parse pExpr "1 - 4 / 2" `shouldBe` AstBinOp "-" (AstInteger 1) (AstBinOp "/" (AstInteger 4) (AstInteger 2))
    it "(%) with higher precedence than (+)" $
      parse pExpr "1 + 2 % 0" `shouldBe` AstBinOp "+" (AstInteger 1) (AstBinOp "%" (AstInteger 2) (AstInteger 0))
    it "(+) with higher precedence than (<)" $
      parse pExpr "10 < 2 + 9" `shouldBe` AstBinOp "<" (AstInteger 10) (AstBinOp "+" (AstInteger 2) (AstInteger 9))

    it "(&&) with higher precedence than (||)" $
      parse pExpr "True || True && False" `shouldBe` AstBinOp "||" (AstBoolean True) (AstBinOp "&&" (AstBoolean True) (AstBoolean False))

    it "(+) left-associative" $
      parse pExpr "1 + 2 + 3 + 4" `shouldBe`
        AstBinOp "+" (AstBinOp "+" (AstBinOp "+" (AstInteger 1)
                                                 (AstInteger 2))
                                   (AstInteger 3))
                     (AstInteger 4)

    it "(*) left-associative" $
      parse pExpr "1 * 2 * 3 * 4" `shouldBe`
        AstBinOp "*" (AstBinOp "*" (AstBinOp "*" (AstInteger 1)
                                                 (AstInteger 2))
                                   (AstInteger 3))
                     (AstInteger 4)

    it "(:) right-associative" $
      parse pExpr "1 : 2 : []" `shouldBe` AstBinOp ":" (AstInteger 1) (AstBinOp ":" (AstInteger 2) (AstEmptyList))

    it "(:) with lower precedence than boolean operators" $
      parse pExpr "5 < 3 : 7 || 4 : []" `shouldBe`
        AstBinOp ":" (AstBinOp "<" (AstInteger 5) (AstInteger 3))
                     (AstBinOp ":" (AstBinOp "||" (AstInteger 7) (AstInteger 4))
                                   AstEmptyList)

    it "(:) with lower precedence than arithmetic operators" $
      parse pExpr "5 + 3 : 7 * 4 : []" `shouldBe`
        AstBinOp ":" (AstBinOp "+" (AstInteger 5) (AstInteger 3))
                     (AstBinOp ":" (AstBinOp "*" (AstInteger 7) (AstInteger 4))
                                   AstEmptyList)

    it "'-10' as negative integer constant" $
      parse pExpr "- 10" `shouldBe` AstInteger (-10)
    it "5 - 10 as subtraction" $
      parse pExpr "5 - 10" `shouldBe` AstBinOp "-" (AstInteger 5) (AstInteger 10)
    it "'5 - - 10' as subtraction with negative integer constant" $
      parse pExpr "5 - - 10" `shouldBe` AstBinOp "-" (AstInteger 5) (AstInteger (-10))
    it "'5--10' as subtraction with negative integer constant" $
      parse pExpr "5--10" `shouldBe` AstBinOp "-" (AstInteger 5) (AstInteger (-10))

    it "unary boolean negation" $
      parse pExpr "!True" `shouldBe` AstUnaryOp "!" (AstBoolean True)

    it "unary arithmetic negation" $
      parse pExpr "- a" `shouldBe` AstUnaryOp "-" (AstIdentifier "a")

    it "--a as double unary negation" $
      parse pExpr "--a" `shouldBe` AstUnaryOp "-" (AstUnaryOp "-" (AstIdentifier "a"))

    it "--1 as unary negation of a negative number" $
      parse pExpr "--1" `shouldBe` AstUnaryOp "-" (AstInteger (-1))

    describe "parenthesized expressions" $ do
      it "(1)" $
        parse pExpr "(1)" `shouldBe` AstInteger 1

      it "1 + (2 + 3)" $
        parse pExpr "1 + (2 + 3)" `shouldBe` AstBinOp "+" (AstInteger 1) (AstBinOp "+" (AstInteger 2) (AstInteger 3))

      it "1 * (2 + 3)" $
        parse pExpr "1 * (2 + 3)" `shouldBe` AstBinOp "*" (AstInteger 1) (AstBinOp "+" (AstInteger 2) (AstInteger 3))

    describe "various forms of function calls" $ do
      it "foo()" $
        parse pExpr "foo()" `shouldBe` AstFunctionCall "foo" []
      it "foo(1)" $
        parse pExpr "foo(1)" `shouldBe` AstFunctionCall "foo" [AstInteger 1]
      it "foo(1, True, bar())" $
        parse pExpr "foo(1, True, bar())" `shouldBe` AstFunctionCall "foo" [AstInteger 1, AstBoolean True, AstFunctionCall "bar" []]


  describe "pFunDeclaration" $ do
    it "function without formal parameters" $
      parse pFunDeclaration "Void foo() { return; }" `shouldBe`
        AstFunDeclaration (BaseType "Void") "foo" [] [] [AstReturn Nothing]
    it "function with one formal parameters" $
      parse pFunDeclaration "Void foo(Int x) { return; }" `shouldBe`
        AstFunDeclaration
          (BaseType "Void")
          "foo"
          [AstFunctionArgument (BaseType "Int") "x"]
          []
          [AstReturn Nothing]
    it "function with three formal parameters and a return value" $
      parse pFunDeclaration "Void foo(Int x, a y, Bool z) { return a; }" `shouldBe`
        AstFunDeclaration
          (BaseType "Void")
          "foo"
          [ AstFunctionArgument (BaseType "Int") "x"
          , AstFunctionArgument (PolymorphicType "a") "y"
          , AstFunctionArgument (BaseType "Bool") "z"
          ]
          []
          [AstReturn (Just (AstIdentifier "a"))]

  describe "pType" $ do
    specTypeParser $ parse (pType defaultBaseTypes)

  describe "pReturnType" $ do
    specTypeParser $ parse pReturnType
    it "Void" $ parse pReturnType "Void" `shouldBe` BaseType "Void"

specTypeParser p = do
  it "polymorphic type" $ p "x" `shouldBe` PolymorphicType "x"
  it "base type Int" $ p "Int" `shouldBe` BaseType "Int"
  it "base type Bool" $ p "Bool" `shouldBe` BaseType "Bool"
  it "tuple type" $ p "(Int, Bool)" `shouldBe` TupleType (BaseType "Int") (BaseType "Bool")
  it "list type" $ p "[Int]" `shouldBe` ListType (BaseType "Int")
  it "polymorphic list type" $ p "[a]" `shouldBe` ListType (PolymorphicType "a")

main :: IO ()
main = hspec specs
