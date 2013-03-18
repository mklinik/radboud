{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module Main (specs, main) where

import Test.Hspec
import Test.QuickCheck

import Parser
import Ast
import Utils

parse :: (SplParser a) -> String -> a
parse = runParser_ ""

specs :: Spec
specs = do
  describe "pExpr" $ do
    it "various forms of identifiers" $
      property $ forAll (elements ["a", "a_", "a10", "a_", "TCP_IP_Connection", "Truee"]) $
        \i -> parse pExpr i == AstIdentifier emptyMeta i

    it "an integer" $
      parse pExpr "1" `shouldBe` AstInteger emptyMeta 1

    it "the boolean constant True" $
      parse pExpr "True" `shouldBe` AstBoolean emptyMeta True
    it "the boolean constant False" $
      parse pExpr "False" `shouldBe` AstBoolean emptyMeta False

    it "a tuple of Int and Bool" $
      parse pExpr "(10, True)" `shouldBe` AstTuple emptyMeta (AstInteger emptyMeta 10) (AstBoolean emptyMeta True)

    it "the empty list constant" $
      parse pExpr "[]" `shouldBe` AstEmptyList emptyMeta

    it "(*) with higher precedence than (:)" $
      parse pExpr "1 * 2 : []" `shouldBe` AstBinOp emptyMeta ":" (AstBinOp emptyMeta "*" (AstInteger emptyMeta 1) (AstInteger emptyMeta 2)) (AstEmptyList emptyMeta)
    it "(*) with higher precedence than (+)" $
      parse pExpr "1 + 2 * 3" `shouldBe` AstBinOp emptyMeta "+" (AstInteger emptyMeta 1) (AstBinOp emptyMeta "*" (AstInteger emptyMeta 2) (AstInteger emptyMeta 3))
    it "(/) with higher precedence than (-)" $
      parse pExpr "1 - 4 / 2" `shouldBe` AstBinOp emptyMeta "-" (AstInteger emptyMeta 1) (AstBinOp emptyMeta "/" (AstInteger emptyMeta 4) (AstInteger emptyMeta 2))
    it "(%) with higher precedence than (+)" $
      parse pExpr "1 + 2 % 0" `shouldBe` AstBinOp emptyMeta "+" (AstInteger emptyMeta 1) (AstBinOp emptyMeta "%" (AstInteger emptyMeta 2) (AstInteger emptyMeta 0))
    it "(+) with higher precedence than (<)" $
      parse pExpr "10 < 2 + 9" `shouldBe` AstBinOp emptyMeta "<" (AstInteger emptyMeta 10) (AstBinOp emptyMeta "+" (AstInteger emptyMeta 2) (AstInteger emptyMeta 9))

    it "(&&) with higher precedence than (||)" $
      parse pExpr "True || True && False" `shouldBe` AstBinOp emptyMeta "||" (AstBoolean emptyMeta True) (AstBinOp emptyMeta "&&" (AstBoolean emptyMeta True) (AstBoolean emptyMeta False))

    it "(+) left-associative" $
      parse pExpr "1 + 2 + 3 + 4" `shouldBe`
        AstBinOp emptyMeta "+" (AstBinOp emptyMeta "+" (AstBinOp emptyMeta "+" (AstInteger emptyMeta 1)
                                                 (AstInteger emptyMeta 2))
                                   (AstInteger emptyMeta 3))
                     (AstInteger emptyMeta 4)

    it "(*) left-associative" $
      parse pExpr "1 * 2 * 3 * 4" `shouldBe`
        AstBinOp emptyMeta "*" (AstBinOp emptyMeta "*" (AstBinOp emptyMeta "*" (AstInteger emptyMeta 1)
                                                 (AstInteger emptyMeta 2))
                                   (AstInteger emptyMeta 3))
                     (AstInteger emptyMeta 4)

    it "(:) right-associative" $
      parse pExpr "1 : 2 : []" `shouldBe` AstBinOp emptyMeta ":" (AstInteger emptyMeta 1) (AstBinOp emptyMeta ":" (AstInteger emptyMeta 2) (AstEmptyList emptyMeta))

    it "(:) with lower precedence than boolean operators" $
      parse pExpr "5 < 3 : 7 || 4 : []" `shouldBe`
        AstBinOp emptyMeta ":" (AstBinOp emptyMeta "<" (AstInteger emptyMeta 5) (AstInteger emptyMeta 3))
                     (AstBinOp emptyMeta ":" (AstBinOp emptyMeta "||" (AstInteger emptyMeta 7) (AstInteger emptyMeta 4))
                                   (AstEmptyList emptyMeta))

    it "(:) with lower precedence than arithmetic operators" $
      parse pExpr "5 + 3 : 7 * 4 : []" `shouldBe`
        AstBinOp emptyMeta ":" (AstBinOp emptyMeta "+" (AstInteger emptyMeta 5) (AstInteger emptyMeta 3))
                     (AstBinOp emptyMeta ":" (AstBinOp emptyMeta "*" (AstInteger emptyMeta 7) (AstInteger emptyMeta 4))
                                   (AstEmptyList emptyMeta))

    it "'-10' as negative integer constant" $
      parse pExpr "- 10" `shouldBe` AstInteger emptyMeta (-10)
    it "5 - 10 as subtraction" $
      parse pExpr "5 - 10" `shouldBe` AstBinOp emptyMeta "-" (AstInteger emptyMeta 5) (AstInteger emptyMeta 10)
    it "'5 - - 10' as subtraction with negative integer constant" $
      parse pExpr "5 - - 10" `shouldBe` AstBinOp emptyMeta "-" (AstInteger emptyMeta 5) (AstInteger emptyMeta (-10))
    it "'5--10' as subtraction with negative integer constant" $
      parse pExpr "5--10" `shouldBe` AstBinOp emptyMeta "-" (AstInteger emptyMeta 5) (AstInteger emptyMeta (-10))

    it "unary boolean negation" $
      parse pExpr "!True" `shouldBe` AstUnaryOp emptyMeta "!" (AstBoolean emptyMeta True)

    it "unary arithmetic negation" $
      parse pExpr "- a" `shouldBe` AstUnaryOp emptyMeta "-" (AstIdentifier emptyMeta "a")

    it "--a as double unary negation" $
      parse pExpr "--a" `shouldBe` AstUnaryOp emptyMeta "-" (AstUnaryOp emptyMeta "-" (AstIdentifier emptyMeta "a"))

    it "--1 as unary negation of a negative number" $
      parse pExpr "--1" `shouldBe` AstUnaryOp emptyMeta "-" (AstInteger emptyMeta (-1))

    describe "parenthesized expressions" $ do
      it "(1)" $
        parse pExpr "(1)" `shouldBe` AstInteger emptyMeta 1

      it "1 + (2 + 3)" $
        parse pExpr "1 + (2 + 3)" `shouldBe` AstBinOp emptyMeta "+" (AstInteger emptyMeta 1) (AstBinOp emptyMeta "+" (AstInteger emptyMeta 2) (AstInteger emptyMeta 3))

      it "1 * (2 + 3)" $
        parse pExpr "1 * (2 + 3)" `shouldBe` AstBinOp emptyMeta "*" (AstInteger emptyMeta 1) (AstBinOp emptyMeta "+" (AstInteger emptyMeta 2) (AstInteger emptyMeta 3))

    describe "various forms of function calls" $ do
      it "foo()" $
        parse pExpr "foo()" `shouldBe` (AstFunctionCallExpr $ AstFunctionCall emptyMeta "foo" [])
      it "foo(1)" $
        parse pExpr "foo(1)" `shouldBe` (AstFunctionCallExpr $ AstFunctionCall emptyMeta "foo" [AstInteger emptyMeta 1])
      it "foo(1, True, bar())" $
        parse pExpr "foo(1, True, bar())" `shouldBe` (AstFunctionCallExpr $ AstFunctionCall emptyMeta "foo" [AstInteger emptyMeta 1, AstBoolean emptyMeta True, AstFunctionCallExpr (AstFunctionCall emptyMeta "bar" [])])


  describe "pFunDeclaration" $ do
    it "function without formal parameters" $
      parse pFunDeclaration "Void foo() { return; }" `shouldBe`
        AstFunDeclaration emptyMeta (BaseType "Void") "foo" [] [] [AstReturn Nothing]
    it "function with one formal parameters" $
      parse pFunDeclaration "Void foo(Int x) { return; }" `shouldBe`
        AstFunDeclaration
          emptyMeta
          (BaseType "Void")
          "foo"
          [AstFunctionArgument (BaseType "Int") "x"]
          []
          [AstReturn Nothing]
    it "function with three formal parameters and a return value" $
      parse pFunDeclaration "Void foo(Int x, a y, Bool z) { return a; }" `shouldBe`
        AstFunDeclaration
          emptyMeta
          (BaseType "Void")
          "foo"
          [ AstFunctionArgument (BaseType "Int") "x"
          , AstFunctionArgument (PolymorphicType "a") "y"
          , AstFunctionArgument (BaseType "Bool") "z"
          ]
          []
          [AstReturn (Just (AstIdentifier emptyMeta "a"))]

  describe "pType" $ do
    specTypeParser $ parse (pType defaultBaseTypes)

  describe "pReturnType" $ do
    specTypeParser $ parse pReturnType
    it "Void" $ parse pReturnType "Void" `shouldBe` BaseType "Void"

  describe "pVarDeclaration" $ do
    let p = parse pVarDeclaration
    it "integer variable" $ p "Int x = 5;" `shouldBe` AstVarDeclaration emptyMeta (BaseType "Int") "x" (AstInteger emptyMeta 5)
    -- it "integer variable" $ p "Int x = 5" `shouldThrow` anyException

  describe "pStatement" $ do
    let p = parse pStatement
    it "return;" $ p "return;" `shouldBe` AstReturn Nothing
    it "return 10;" $ p "return 10;" `shouldBe` AstReturn (Just (AstInteger emptyMeta 10))
    it "if-then-else" $ p "if(True) return; else return;" `shouldBe`
      AstIfThenElse (AstBoolean emptyMeta True)
                    (AstReturn Nothing)
                    (AstReturn Nothing)
    it "while" $ p "while(True) return;" `shouldBe` AstWhile (AstBoolean emptyMeta True) (AstReturn Nothing)
    it "assignment" $ p "x = 10;" `shouldBe` AstAssignment "x" (AstInteger emptyMeta 10)
    it "function call" $ p "foo();" `shouldBe` AstFunctionCallStmt (AstFunctionCall emptyMeta "foo" [])
    it "empty statement block" $ p "{}" `shouldBe` AstBlock []
    it "statement block with return " $ p "{return;}" `shouldBe` AstBlock [AstReturn Nothing]
    it "return with parenthesized expression is not a function call" $
      p "return (10);" `shouldBe` AstReturn (Just (AstInteger emptyMeta 10))

specTypeParser :: (String -> AstType) -> Spec
specTypeParser p = do
  it "polymorphic type" $ p "x" `shouldBe` PolymorphicType "x"
  it "base type Int" $ p "Int" `shouldBe` BaseType "Int"
  it "base type Bool" $ p "Bool" `shouldBe` BaseType "Bool"
  it "tuple type" $ p "(Int, Bool)" `shouldBe` TupleType (BaseType "Int") (BaseType "Bool")
  it "list type" $ p "[Int]" `shouldBe` ListType (BaseType "Int")
  it "polymorphic list type" $ p "[a]" `shouldBe` ListType (PolymorphicType "a")

main :: IO ()
main = hspec specs
