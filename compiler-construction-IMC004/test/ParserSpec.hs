{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module ParserSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck

import Parser
import Ast
import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

    it "boolean negation binds more tightly than &&" $
      parse pExpr "!True && False" `shouldBe` AstBinOp emptyMeta "&&"
        (AstUnaryOp emptyMeta "!" (AstBoolean emptyMeta True))
        (AstBoolean emptyMeta False)

    it "boolean negation binds more tightly than ||" $
      parse pExpr "!True || False" `shouldBe` AstBinOp emptyMeta "||"
        (AstUnaryOp emptyMeta "!" (AstBoolean emptyMeta True))
        (AstBoolean emptyMeta False)

    it "integer negation binds more tightly than +" $
      parse pExpr "-x + y" `shouldBe` AstBinOp emptyMeta "+"
        (AstUnaryOp emptyMeta "-" (AstIdentifier emptyMeta "x"))
        (AstIdentifier emptyMeta "y")

    it "integer negation binds more tightly than *" $
      parse pExpr "-x * y" `shouldBe` AstBinOp emptyMeta "*"
        (AstUnaryOp emptyMeta "-" (AstIdentifier emptyMeta "x"))
        (AstIdentifier emptyMeta "y")

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
        AstFunDeclaration emptyMeta (BaseType emptyMeta "Void") "foo" [] [] [AstReturn emptyMeta Nothing]
    it "function with one formal parameters" $
      parse pFunDeclaration "Void foo(Int x) { return; }" `shouldBe`
        AstFunDeclaration
          emptyMeta
          (BaseType emptyMeta "Void")
          "foo"
          [AstFunctionArgument emptyMeta (BaseType emptyMeta "Int") "x"]
          []
          [AstReturn emptyMeta Nothing]
    it "function with three formal parameters and a return value" $
      parse pFunDeclaration "Void foo(Int x, a y, Bool z) { return a; }" `shouldBe`
        AstFunDeclaration
          emptyMeta
          (BaseType emptyMeta "Void")
          "foo"
          [ AstFunctionArgument emptyMeta (BaseType emptyMeta "Int") "x"
          , AstFunctionArgument emptyMeta (PolymorphicType emptyMeta "a") "y"
          , AstFunctionArgument emptyMeta (BaseType emptyMeta "Bool") "z"
          ]
          []
          [AstReturn emptyMeta (Just (AstIdentifier emptyMeta "a"))]
    it "function without formal params, no local variables, and an assignment." $ do
      parse pFunDeclaration "Void bar() { foo = 100; return; }" `shouldBe`
        AstFunDeclaration
          emptyMeta
          (BaseType emptyMeta "Void")
          "bar"
          []
          []
          [AstAssignment emptyMeta "foo" $ AstInteger emptyMeta 100
          ,AstReturn emptyMeta Nothing]

  describe "pType" $ do
    specTypeParser $ parse (pType defaultBaseTypes)

  describe "pReturnType" $ do
    specTypeParser $ parse pReturnType
    it "Void" $ parse pReturnType "Void" `shouldBe` BaseType emptyMeta "Void"

  describe "pVarDeclaration" $ do
    let p = parse pVarDeclaration
    it "integer variable" $ p "Int    x = 5;" `shouldBe` AstVarDeclaration emptyMeta (BaseType emptyMeta "Int") "x" (AstInteger emptyMeta 5)
    -- it "integer variable" $ p "Int x = 5" `shouldThrow` anyException

  describe "pStatement" $ do
    let p = parse pStatement
    it "return;" $ p "return;" `shouldBe` AstReturn emptyMeta Nothing
    it "return 10;" $ p "return 10;" `shouldBe` AstReturn emptyMeta (Just (AstInteger emptyMeta 10))
    it "if-then-else" $ p "if(True) return; else return;" `shouldBe`
      AstIfThenElse emptyMeta
                    (AstBoolean emptyMeta True)
                    (AstReturn emptyMeta Nothing)
                    (AstReturn emptyMeta Nothing)
    it "while" $ p "while(True) return;" `shouldBe` AstWhile emptyMeta (AstBoolean emptyMeta True) (AstReturn emptyMeta Nothing)
    it "assignment" $ p "x = 10;" `shouldBe` AstAssignment emptyMeta "x" (AstInteger emptyMeta 10)
    it "function call" $ p "foo();" `shouldBe` AstFunctionCallStmt (AstFunctionCall emptyMeta "foo" [])
    it "empty statement block" $ p "{}" `shouldBe` AstBlock []
    it "statement block with return " $ p "{return;}" `shouldBe` AstBlock [AstReturn emptyMeta Nothing]
    it "return with parenthesized expression is not a function call" $
      p "return (10);" `shouldBe` AstReturn emptyMeta (Just (AstInteger emptyMeta 10))

specTypeParser :: (String -> AstType) -> Spec
specTypeParser p = do
  it "polymorphic type" $ p "x" `shouldBe` PolymorphicType emptyMeta "x"
  it "base type Int" $ p "Int" `shouldBe` BaseType emptyMeta "Int"
  it "base type Bool" $ p "Bool" `shouldBe` BaseType emptyMeta "Bool"
  it "tuple type" $ p "(Int, Bool)" `shouldBe` TupleType emptyMeta (BaseType emptyMeta "Int") (BaseType emptyMeta "Bool")
  it "list type" $ p "[Int]" `shouldBe` ListType emptyMeta (BaseType emptyMeta "Int")
  it "polymorphic list type" $ p "[a]" `shouldBe` ListType emptyMeta (PolymorphicType emptyMeta "a")
