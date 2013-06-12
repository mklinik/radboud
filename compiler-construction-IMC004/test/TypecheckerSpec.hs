{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module TypecheckerSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set

import Parser
import Utils
import Typechecker
import SplType
import Ast

main :: IO ()
main = hspec spec

-- evaluate a typecheck action in an empty environment
evalTypecheckBare :: (Typecheck a) -> a
evalTypecheckBare t = unRight $ evalState (runEitherT (t)) emptyTypecheckState

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
  in
    case runTypecheck $ typecheck ast of
      Right (env,_) -> case runTypecheck $ envLookup emptyMeta identifier env of
          Right typ -> prettyprintType $ makeNiceAutoTypeVariables typ
          Left err -> show err
      Left err -> show err

spec :: Spec
spec = do
  describe "typeVars" $ do
    it "gives the empty list for a base type" $ null $ typeVars (SplBaseType BaseTypeInt)
    it "recurses into record types, fixed row" $
      Set.fromList (typeVars $ SplRecordType $ SplFixedRow $ Map.fromList
          [("x", SplTypeVariable "<1>")
          ,("y", SplTypeVariable "<2>")]) `shouldBe` Set.fromList ["<1>", "<2>"]
    it "recurses into record types, variable row" $
      Set.fromList (typeVars $ SplRecordType $ SplVariableRow "<3>" $ Map.fromList
          [("x", SplTypeVariable "<1>")
          ,("y", SplTypeVariable "<2>")]) `shouldBe` Set.fromList ["<1>", "<2>"]

  describe "substitute" $ do
    it "substitutes a base type for a type variable" $
      substitute (mkSubstitution "<1>" splTypeBool) (SplTypeVariable "<1>") `shouldBe` splTypeBool
    it "overwrites a substitution" $
      substitute (mkSubstitution "<1>" splTypeBool `after` mkSubstitution "<1>" splTypeInt) (SplTypeVariable "<1>") `shouldBe` splTypeInt
    it "nested substitution" $ do
      let u = mkSubstitution "<2>" splTypeBool `after` mkSubstitution "<1>" (SplTypeVariable "<2>")
      substitute u (SplTypeVariable "<1>") `shouldBe` splTypeBool
    it "nested substitution, the other direction" $ do
      let u =  mkSubstitution "<1>" (SplTypeVariable "<2>") `after` mkSubstitution "<2>" splTypeBool
      substitute u (SplTypeVariable "<1>") `shouldBe` splTypeBool
    it "rightmost row entries win" $ do
      let fieldsRight = Map.fromList [("x", splTypeInt)]
      let fieldsLeft = Map.fromList [("x", splTypeBool), ("y", splTypeBool)]
      let expected = Map.fromList [("x", splTypeInt), ("y", splTypeBool)]
      substitute (mkRowSubstitution "<1>" (SplFixedRow fieldsLeft)) (SplVariableRow "<1>" fieldsRight) `shouldBe` SplFixedRow expected
    it "nested row substitution" $ do
      let typ = SplFixedRow $ Map.fromList [("x", SplRecordType $ SplVariableRow "<1>" $ Map.fromList [("y", splTypeInt)])]
      let sub = mkRowSubstitution "<1>" $ SplFixedRow $ Map.fromList [("z", splTypeBool)]
      substitute sub typ `shouldBe` (SplFixedRow $ Map.fromList [("x", SplRecordType $ SplFixedRow $ Map.fromList
        [("y", splTypeInt)
        ,("z", splTypeBool)])])
    it "substitutes types in rows, fixed row" $ do
      let typ = SplRecordType $ SplFixedRow $ Map.fromList [("x", SplTypeVariable "<1>")]
      let sub = mkSubstitution "<1>" splTypeInt
      let expected = SplRecordType $ SplFixedRow $ Map.fromList [("x", splTypeInt)]
      substitute sub typ `shouldBe` expected
    it "substitutes types in rows, variable row" $ do
      let typ = SplRecordType $ SplVariableRow "<2>" $ Map.fromList [("x", SplTypeVariable "<1>")]
      let sub = mkSubstitution "<1>" splTypeInt
      let expected = SplRecordType $ SplVariableRow "<2>" $ Map.fromList [("x", splTypeInt)]
      substitute sub typ `shouldBe` expected


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
      typeOf "const" "a const(b x, c y){ return x; }" `shouldBe` "(b a -> b)"
    it "infers a monomorphic recursive list definition" $ do
      typeOf "x" "a x = 1:x;" `shouldBe` "[Int]"
    it "can infer a polymorphic recursive list definition" $ do
      typeOf "x" "a x = head(x):x;" `shouldBe` "[a]"
    it "cannot infer mutual recursive values" $ do
      typeOf "x" "a x = y; b y = x;" `shouldBe` "Unknown identifier `y' at position 1:7"
    it "cannot infer mutual recursive lists" $ do
      typeOf "x" "a x = head(y):x; b y = head(x):y;" `shouldBe` "Unknown identifier `y' at position 1:12"

    let mutualFandG = unlines
            ["{a f(b x, c y) { return g(y, x); }"
            ,"a g(b x, c y) { return f(x, y); }}"
            ]

    it "can infer mutual recursive functions" $ do
      typeOf "f" mutualFandG `shouldBe` "(a a -> b)"
      typeOf "g" mutualFandG `shouldBe` "(a a -> b)"

    it "infers that g must be a function" $ do
      typeOf "f" "Int f(a g) { return g(1); }" `shouldBe` "((Int -> Int) -> Int)"

    it "infers that x must be a tuple" $ do
      typeOf "f" "Int f(a x) { return fst(x); }" `shouldBe` "((Int, a) -> Int)"

    let identity = "a id(a x) { return x; }"

    it "can use different instantiations of globals in tuples" $ do
      typeOf "x" (identity ++ "a x = (   10,     True );") `shouldBe` "(Int, Bool)"
      typeOf "x" (identity ++ "a x = (id(10), id(True));") `shouldBe` "(Int, Bool)"

    it "can use different instantiations of globals in statements" $ do
      typeOf "f" (identity ++ "a f() { id(10); id(True); return 10; }") `shouldBe` "( -> Int)"

    it "canot use different instantiations of a global list when using the value" $ do
      typeOf "f" "[a] x = []; a f() { return (1:x, True:x); }" `shouldBe`
        "Couldn't match expected type `[Bool]' with actual type `[Int]' at position 1:39"

    it "cannot use different instantiations of a global list when assigning the value" $ do
      typeOf "f" "[Int] x = []; Void f() { x = 1:[]; x = True:[]; return; }" `shouldBe`
        "Couldn't match expected type `Int' with actual type `Bool' at position 1:40"

    it "assignment to a global list inside a function must determine it's type" $ do
      typeOf "x" "[a] x = []; a f() { x = 1:[]; return; }" `shouldBe` "[Int]"

    it "propagates types transitively through global variables, forwards" $ do
      let prog = "z z = True; y y = z; x x = y;"
      typeOf "x" prog `shouldBe` "Bool"
      typeOf "y" prog `shouldBe` "Bool"
      typeOf "z" prog `shouldBe` "Bool"

    it "propagate types transitively through global variables, backwards, fails" $ do
      let prog = "x x = y; y y = z; z z = True;"
      typeOf "x" prog `shouldBe` "Unknown identifier `y' at position 1:7"

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
        "Couldn't match expected type `(Int, a)' with actual type `(Bool, Bool)' at position 1:13"

    describe "local variables" $ do

      it "can typecheck mutually recursive local variables" $ do
        typeOf "foo" "Int foo() { var x = y; var y = z; var z = x; return y; }" `shouldBe`
          "( -> Int)"

      it "cannot use different instantiatons of function arguments" $ do
        typeOf "f" "a f(b x) { return (x(10), x(True)); }" `shouldBe`
          "Couldn't match expected type `Int' with actual type `Bool' at position 1:29"

      it "cannot use different instantiations of local variables in function body" $ do
        typeOf "f" "a f() { var x = []; return (1:x, True:x); }" `shouldBe`
          "Couldn't match expected type `[Bool]' with actual type `[Int]' at position 1:39"

      it "cannot use different instantiations of local variables in other initializers" $ do
        typeOf "f" "a f() { var x = []; var y = 1:x; var z = True:x; return (y, z); }" `shouldBe`
          "Couldn't match expected type `[Bool]' with actual type `[Int]' at position 1:47"

      it "cannot use different instantiations of local variables in other initializers, reverse order" $ do
        typeOf "f" "a f() { var y = 1:x; var z = True:x; var x = []; return (y, z); }" `shouldBe`
          "Couldn't match expected type `[Bool]' with actual type `[Int]' at position 1:35"

      it "cannot use different instantiations of local variables in other initializers, reverse order" $ do
        typeOf "f" "a f() { var y = 1:x; var z = True:x; var x = []; x = 1:[]; return (y, z); }" `shouldBe`
          "Couldn't match expected type `[Bool]' with actual type `[Int]' at position 1:35"

      it "cannot assign different instantiations to local variables" $ do
        typeOf "f" "a f() { var x = []; x = 1:[]; x = True:[]; return; }" `shouldBe`
          "Couldn't match expected type `Int' with actual type `Bool' at position 1:35"

    describe "conditionals" $ do

      it "infers that the condition of an if-then-else must be Bool" $ do
        typeOf "f" "a f(b x) { if(x) return 10; else return 20; }" `shouldBe` "(Bool -> Int)"

      it "complains when the condition of an if-then-else is not Bool" $ do
        typeOf "f" "Int f() { if( 10 ) return 10; else return 20; }" `shouldBe`
          "Couldn't match expected type `Bool' with actual type `Int' at position 1:15"

    describe "return values" $ do

      it "infers that all returned values must be of the same type" $ do
        typeOf "f" "a f(b x) { return 10; return x; }" `shouldBe` "(Int -> Int)"

      it "complains when a returned value doesn't match the return type of the function" $ do
        typeOf "f" "Int f() { return True; }" `shouldBe`
          "Couldn't match expected type `Int' with actual type `Bool' at position 1:18"

      it "complains when not all returned values are of the same type" $ do
        typeOf "f" "a f() { return True; return 10; }" `shouldBe`
          "Couldn't match expected type `Bool' with actual type `Int' at position 1:29"

      it "complains when a Void function returns a value" $ do
        typeOf "f" "Void f() { return 10; }" `shouldBe`
          "Couldn't match expected type `Void' with actual type `Int' at position 1:19"

      it "complains when a non-Void function returns without a value" $ do
        typeOf "f" "Int f() { return; }" `shouldBe`
          "Couldn't match expected type `Int' with actual type `Void' at position 1:11"

    describe "while loops" $ do

      it "infers that the condition of while must be Bool" $ do
        typeOf "f" "Void f(b x) { while(x) return; }" `shouldBe` "(Bool -> Void)"

      it "complains when the condition of while isn't Bool" $ do
        typeOf "f" "Void f() { while(10) return; }" `shouldBe`
          "Couldn't match expected type `Bool' with actual type `Int' at position 1:18"


    describe "assignments" $ do

      it "typechecks assignment to self" $ do
        typeOf "f" "a f() { var x = 10; x = x; return x; }" `shouldBe` "( -> Int)"
      it "typechecks assignment between locals" $ do
        typeOf "f" "a f() { var x = 10; var y = 20; x = y; y = x; return y; }" `shouldBe` "( -> Int)"
      it "can infer the type of a local variable transitively through declarations, backwards" $ do
        typeOf "f" "a f() { x x = y; y y = z; z z = True; return x; }" `shouldBe` "( -> Bool)"
      it "infers the type of a local variable transitively through assignments" $ do
        typeOf "f" "a f() { x x = x; y y = y; z z = z; z = True; y = z; x = y; return x; }" `shouldBe` "( -> Bool)"

      it "unifies both a and b from const and const2 as Int" $ do
        typeOf "f" (unlines
          ["a const(a x, b y) { return x; }"
          ,"b const2(a x, b y) { return y; }"
          ,"a f() {"
          ,"  var foo = head([]);"
          ,"  if(True) foo = const; else foo = const2;"
          ,"  return foo(10, 20);"
          ,"}"
          ]) `shouldBe` "( -> Int)"

      it "unifies both a and b from const and const2 as Int" $ do
        typeOf "f" (unlines
          ["a const(a x, b y) { return x; }"
          ,"b const2(a x, b y) { return y; }"
          ,"a f() {"
          ,"  var foo = const;"
          ,"  if(True) foo = const2;"
          ,"  return foo(10, 20);"
          ,"}"
          ]) `shouldBe` "( -> Int)"

      it "infers that the first argument of f must be of the same type as the global x" $ do
        typeOf "f" (unlines
          ["var x = [];"
          ,"fun f(y y, z z) { x = y; return z; }"
          ,"Void main() { x = 1:[]; return; }"
          ]) `shouldBe` "([Int] a -> a)"

    describe "standard examples" $ do
      it "infers foldl" $ do
        typeOf "foldl" (unlines
          ["a foldl(b f, c z, d list)"
          ,"{"
          ,"  if( isEmpty(list) )"
          ,"    return z;"
          ,"  else"
          ,"    return foldl(f, f(z, head(list)), tail(list));"
          ,"}"
          ]) `shouldBe` "((b a -> b) b [a] -> b)"
      it "infers foldr" $ do
        typeOf "foldr" (unlines
          ["a foldr(b f, c z, d list)"
          ,"{"
          ,"  if( isEmpty(list) )"
          ,"    return z;"
          ,"  else"
          ,"    return f(head(list), foldr(f, z, tail(list)));"
          ,"}    "
          ]) `shouldBe` "((a b -> b) b [a] -> b)"
      it "infers map" $ do
        typeOf "map" (unlines
          ["a map(b f, c list)"
          ,"{"
          ,"  if( isEmpty(list) )"
          ,"    return [];"
          ,"  else"
          ,"    return f(head(list)) : map(f, tail(list));"
          ,"}"
          ]) `shouldBe` "((a -> b) [a] -> [b])"

    let getX = "a getX(b b) { return b.x; }"
    let getY = "a getY(b b) { return b.y; }"

    describe "records" $ do
      it "infers that the argument must be a record with one field x" $
        typeOf "getX" "a getX(b b) { return b.x; }" `shouldBe` "({a x} -> a)"
      it "infers that the argument must be a record with two fields x and y" $
        typeOf "getXY" "a getXY(b b) { return (b.x, b.y); }" `shouldBe` "({a x, b y} -> (a, b))"
      it "infers that the argument must be a record with two fields x and y of type Int" $
        typeOf "addXY" "a addXY(b b) { return (b.x + b.y); }" `shouldBe` "({Int x, Int y} -> Int)"
      it "infers that getX only needs x, getY only needs y, but getXY needs both" $ do
        let program = unlines
              [ getX
              , getY
              , "a getXY(b b) { return (getX(b), getY(b)); }"
              ]
        typeOf "getX" program `shouldBe` "({a x} -> a)"
        typeOf "getY" program `shouldBe` "({a y} -> a)"
        typeOf "getXY" program `shouldBe` "({a x, b y} -> (a, b))"
      it "record with a single field x can be passed to getX" $ do
        let program = unlines
              [ getX
              , "var blah = getX({ x = 10 });"
              ]
        typeOf "blah" program `shouldBe` "Int"
      it "record with a single field y cannot be passed to getX" $ do
        let program = unlines
              [ getX
              , "var blah = getX({ y = 10 });"
              ]
        typeOf "blah" program `shouldBe` "Couldn't match expected type `{a x}' with actual type `{Int y}' at position 2:17"
      it "record with two fields x,y can be passed to getX" $ do
        let program = unlines
              [ getX
              , "var blah = getX({ x = 10, y = 10 });"
              ]
        typeOf "blah" program `shouldBe` "Int"
      it "global variables are not row-polymorphic, i.e. the type collects all fields used" $ do
        typeOf "x" (unlines
          [ "var x = [];"
          , "Int foo = head(x).x;"
          , "Int bar = head(x).y;"
          ]) `shouldBe` "[{Int x, Int y}]"
