{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module TypecheckerSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy

import Parser
import Utils
import Typechecker
import SplType
import Ast

main :: IO ()
main = hspec spec

-- evaluate a typecheck action in an empty environment
evalTypecheckBare :: (Typecheck a) -> a
evalTypecheckBare t = unRight $ evalState (runEitherT (t)) emptyEnvironment

parseConvertShow :: String -> String
parseConvertShow s = convertShow $ parse pReturnType s

convertShow :: AstType -> String
convertShow t = show $ evalTypecheckBare $ astType2splType t

typeInt, typeBool, typeVoid :: AstType
typeInt = BaseType emptyMeta "Int"
typeBool = BaseType emptyMeta "Bool"
typeVoid = BaseType emptyMeta "Void"
typeVar :: String -> AstType
typeVar x = PolymorphicType emptyMeta x

-- Given a program and an identifier, infer the type of the identifier in the
-- standard environment.
typeOf :: String -> String -> String
typeOf identifier program =
  let ast = parse pProgram program
      (result, _) = runTypecheck $ typecheck ast >> envLookup identifier emptyMeta
  in
    case result of
      Right (typ, _) -> prettyprintType $ makeNiceAutoTypeVariables typ
      Left err -> show err

spec :: Spec
spec = do
  describe "typeVars" $ do
    it "gives the empty list for a base type" $ null $ typeVars (SplBaseType BaseTypeInt)

  describe "astType2splType" $ do
    it "is the identity function on monomorphic types" $ do
      parseConvertShow "Bool" `shouldBe` "Bool"
      parseConvertShow "Int" `shouldBe` "Int"
      parseConvertShow "Void" `shouldBe` "Void"
    it "replaces single type variables with fresh ones" $ do
      parseConvertShow "a" `shouldBe` "<0>"
      parseConvertShow "[a]" `shouldBe` "[<0>]"
    it "replaces distinct type variables with distinct fresh ones" $ do
      parseConvertShow "(a, b)" `shouldBe` "(<0>, <1>)"
    it "replaces the same type variables with the same fresh ones" $ do
      parseConvertShow "(a, a)" `shouldBe` "(<0>, <0>)"
    it "replaces same with same and distinct with distinct type variables" $ do
      parseConvertShow "(a, (b, (b, a)))" `shouldBe` "(<0>, (<1>, (<1>, <0>)))"
    it "is the identity on monomorphic function types" $ do
      convertShow (FunctionType [typeInt, typeBool] typeVoid) `shouldBe` "(Int Bool -> Void)"
    it "replaces distinct with distinct type variables in function types" $ do
      convertShow (FunctionType [typeVar "x", typeVar "y"] (typeVar "z")) `shouldBe` "(<0> <1> -> <2>)"
    it "replaces distinct with distinct and same with same type vars in function types" $ do
      convertShow (FunctionType [typeVar "x", typeVar "y", typeVar "y"] (typeVar "x")) `shouldBe` "(<0> <1> <1> -> <0>)"

  describe "typecheck" $ do
    it "infers the identity function" $ do
      typeOf "id" "a id(b x){ return x; }" `shouldBe` "(a -> a)"
    it "infers the constant function" $ do
      typeOf "const" "a const(b x, c y){ return x; }" `shouldBe` "(a b -> a)"
    it "infers a monomorphic recursive list definition" $ do
      typeOf "x" "a x = 1:x;" `shouldBe` "[Int]"
    it "infers a polymorphic recursive list definition" $ do
      typeOf "x" "a x = head(x):x;" `shouldBe` "[a]"
    it "infers mutual recursive values" $ do
      typeOf "x" "a x = y; b y = x;" `shouldBe` "a"
      typeOf "y" "a x = y; b y = x;" `shouldBe` "a"
    it "infers mutual recursive lists" $ do
      typeOf "x" "a x = head(y):x; b y = head(x):y;" `shouldBe` "[a]"
      typeOf "y" "a x = head(y):x; b y = head(x):y;" `shouldBe` "[a]"

    let mutualFandG = unlines
            ["a f(b x) { return g(x); }"
            ,"a g(b x) { return f(x); }"
            ]

    it "infers mutual recursive functions" $ do
      typeOf "f" mutualFandG `shouldBe` "(a -> b)"
      typeOf "g" mutualFandG `shouldBe` "(a -> b)"

    it "can use mutual f and g with different types of arguments" $ do
      typeOf "x" (mutualFandG ++ "x x = (f(True), g(10));") `shouldBe` "(a, b)"

    it "infers mutual recursive functions where f is a tad more specific" $ do
      let prog = (unlines
            ["a f(a x) { return g(x); }"
            ,"a g(b x) { return f(x); }"
            ])
      typeOf "f" prog `shouldBe` "(a -> a)"
      typeOf "g" prog `shouldBe` "(a -> a)"

    it "infers that g must be a function" $ do
      typeOf "f" "Int f(a g) { return g(1); }" `shouldBe` "((Int -> Int) -> Int)"

    it "infers that x must be a tuple" $ do
      typeOf "f" "Int f(a x) { return fst(x); }" `shouldBe` "((Int, a) -> Int)"

    it "can use different instantiations of globals in tuples" $ do
      let identity = "a id(a x) { return x; }"
      typeOf "x" (identity ++ "a x = (   10,     True );") `shouldBe` "(Int, Bool)"
      typeOf "x" (identity ++ "a x = (id(10), id(True));") `shouldBe` "(Int, Bool)"

    it "can use different instantiations of globals in statements" $ do
      typeOf "f" "a id(a x) { return x; } a f() { id(10); id(True); return 10; }" `shouldBe` "( -> Int)"

    it "cannot use different instantiatons of locals" $ do
      typeOf "f" "a f(b x) { return (x(10), x(True)); }" `shouldBe`
        "Couldn't match expected type `Int' with actual type `Bool' at position 1:27"

    let double = "a double(f f, x x) { return f(f(x)); }"

    it "can type the double function" $ do
      typeOf "double" double `shouldBe` "((a -> a) a -> a)"

    -- some boring stuff
    it "infers some monomorphic values" $ do
      typeOf "x" "Int x = 10;" `shouldBe` "Int"
      typeOf "x" "Bool x = True;" `shouldBe` "Bool"
      typeOf "x" "a x = (10, True);" `shouldBe` "(Int, Bool)"
      typeOf "x" "a x = 10:[];" `shouldBe` "[Int]"
      typeOf "x" "a x = (10, True):[];" `shouldBe` "[(Int, Bool)]"
      typeOf "x" "a x = (10:[], True:[]):[];" `shouldBe` "[([Int], [Bool])]"

    it "typechecks comparison operators" $ do
      property $ forAll (elements ["<", ">", "<=", ">=", "==", "!="]) $
        \o -> typeOf "x" ("x x = 10" ++ o ++ "10;") == "Bool"

    it "typechecks arithmetical operators" $ do
      property $ forAll (elements ["+", "-", "*", "/", "%"]) $
        \o -> typeOf "x" ("x x = 10" ++ o ++ "10;") == "Int"

    it "typechecks logical operators" $ do
      typeOf "x" "x x = True && True;" `shouldBe` "Bool"
      typeOf "x" "x x = True || True;" `shouldBe` "Bool"

    it "infers that the identity becomes (Int -> Int) when applied to Int in body" $ do
      typeOf "id" "a id(a x) { id(1); return x; }" `shouldBe` "(Int -> Int)"
    it "infers that the identity stays (a -> a) when applied to int outside body" $ do
      typeOf "id" "a id(a x) { return x; } Int y = id(1);" `shouldBe` "(a -> a)"

    it "fails extracting an Int from a tuple of Bools" $ do
      typeOf "x" "Int x = fst((True, True));" `shouldBe`
        "Couldn't match expected type `Int' with actual type `Bool' at position 1:9"

    it "typechecks mutually recursive local variables" $ do
      typeOf "foo" "Int foo() { var x = y; var y = z; var z = x; return y; }" `shouldBe` "( -> Int)"

    describe "conditionals" $ do

      it "infers that the condition of an if-then-else must be Bool" $ do
        typeOf "f" "a f(b x) { if(x) return 10; else return 20; }" `shouldBe` "(Bool -> Int)"

      it "complains when the condition of an if-then-else is not Bool" $ do
        typeOf "f" "Int f() { if( 10 ) return 10; else return 20; }" `shouldBe`
          "Couldn't match expected type `Bool' with actual type `Int' at position 1:11"

    describe "return values" $ do

      it "infers that all returned values must be of the same type" $ do
        typeOf "f" "a f(b x) { return 10; return x; }" `shouldBe` "(Int -> Int)"

      it "complains when a returned value doesn't match the return type of the function" $ do
        typeOf "f" "Int f() { return True; }" `shouldBe`
          "Couldn't match expected type `Int' with actual type `Bool' at position 1:11"

      it "complains when not all returned values are of the same type" $ do
        typeOf "f" "a f() { return True; return 10; }" `shouldBe`
          "Couldn't match expected type `Bool' with actual type `Int' at position 1:22"

      it "complains when a Void function returns a value" $ do
        typeOf "f" "Void f() { return 10; }" `shouldBe`
          "Couldn't match expected type `Void' with actual type `Int' at position 1:12"

      it "complains when a non-Void function returns without a value" $ do
        typeOf "f" "Int f() { return; }" `shouldBe`
          "Couldn't match expected type `Int' with actual type `Void' at position 1:11"

    describe "while loops" $ do

      it "infers that the condition of while must be Bool" $ do
        typeOf "f" "Void f(b x) { while(x) return; }" `shouldBe` "(Bool -> Void)"

      it "complains when the condition of while isn't Bool" $ do
        typeOf "f" "Void f() { while(10) return; }" `shouldBe`
          "Couldn't match expected type `Bool' with actual type `Int' at position 1:12"


    describe "assignments" $ do

      it "typechecks assignment to self" $ do
        typeOf "f" "a f() { var x = 10; x = x; return x; }" `shouldBe` "( -> Int)"
      it "typechecks assignment between locals" $ do
        typeOf "f" "a f() { var x = 10; var y = 20; x = y; y = x; return y; }" `shouldBe` "( -> Int)"
      it "infers the type of a local variable" $ do
        typeOf "f" "a f() { x x = y; y y = z; z z = True; return x; }" `shouldBe` "( -> Bool)"
