{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module InterpreterSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck
import qualified Control.Monad.Trans.State.Lazy as MT
import qualified Control.Monad.Trans.Either as MT
import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)

import Parser
import Utils
import Interpreter

expr :: String -> Value
expr prog = unsafePerformIO $ (liftM unRight) $ MT.evalStateT (MT.runEitherT (eval $ parse pExpr prog)) emptyEnvironment

run :: String -> IO Value
run prog = (liftM unRight) $ MT.evalStateT (MT.runEitherT $ interpretProgram $ parse pProgram prog) emptyEnvironment

run_ :: [String] -> IO Value
run_ prog = (liftM unRight) $ MT.evalStateT (MT.runEitherT $ interpretProgram $ parse pProgram $ unlines prog) emptyEnvironment

testBinOp opSyntax opFunction resultConstructor =
  property $ \i1 i2 -> expr (show i1 ++ " " ++ opSyntax ++ " " ++ show i2) == (resultConstructor (opFunction i1 i2))

spec :: Spec
spec = do

  describe "Interpreter" $ do
    it "integer constant" $ run "Int main() { return 10; }" `shouldReturn` (I 10)
    it "global variable" $
      run_ ["Int foo = 42;"
           ,"Int main()"
           ,"{"
           ,"  return foo;"
           ,"}"
           ] `shouldReturn` (I 42)
    it "assignments" $
      run_ ["Int main()"
           ,"{"
           ,"  Int i = 5;"
           ,"  i = 10;"
           ,"  return i;"
           ,"}"
           ] `shouldReturn` (I 10)
    it "side effects" $
      run_ ["Int foo = 42;"
           ,"Void bar()"
           ,"{"
           ,"  foo = 100; return;"
           ,"}"
           ,"Int main()"
           ,"{ bar();"
           ,"  return foo;"
           ,"}"
           ] `shouldReturn` (I 100)
    it "environments shrink again" $ do
      run_ ["Int x = 1;"
           ,"Void bar() { Int x = 10; return; }"
           ,"Int main() { bar(); return x; }"
           ] `shouldReturn` (I 1)

    it "two return statements return the first value" $ do
      run "Int main() { return 10; return 20; }" `shouldReturn` (I 10)

  describe "while loop" $ do

    it "doesn't run the statement when the condition is false" $ do
      run "Int main() { Int counter = 0; while(False) { counter = counter + 1; } return counter; }"
        `shouldReturn` (I 0)
    it "runs the statement once when the condition becomes false in the first iteration" $ do
      run_ ["Int main() {"
           ,"  Int counter = 0;"
           ,"  while( counter <= 0 ) counter = counter + 1;"
           ,"  return counter;"
           ,"}"
           ] `shouldReturn` (I 1)
    it "runs the statement several times" $ do
      run_ ["Int main() {"
           ,"  Int counter = 0;"
           ,"  while( counter <= 10 ) counter = counter + 1;"
           ,"  return counter;"
           ,"}"
           ] `shouldReturn` (I 11)

  describe "if-then-else" $ do
    it "interprets the then-branch when the condition is true" $ do
      run "Int main() { if(True) return 1000; else return 42; }"
        `shouldReturn` (I 1000)
    it "interprets the else-branch when the condition is false" $ do
      run "Int main() { if(False) return 1000; else return 42; }"
        `shouldReturn` (I 42)

  describe "eval" $ do
    it "evaluates integer constants" $ do
      property $ \x -> expr (show x) == (I x)
    it "evaluates integer constants" $ do
      expr "10" `shouldBe` (I 10)
      expr "-42" `shouldBe` (I (-42))

    it "evaluates boolean constants" $ do
      property $ \x -> expr (show x) == (B x)

    it "evaluates (+)" $ testBinOp "+" (+) I
    it "evaluates (-)" $ testBinOp "-" (-) I
    it "evaluates (*)" $ testBinOp "*" (*) I
    it "evaluates (/)" $ property $ \i1 i2 -> i2 /= 0 ==> expr (show i1 ++ " / " ++ show i2) == (I (div i1 i2))
    it "evaluates (%)" $ property $ \i1 i2 -> i2 /= 0 ==> expr (show i1 ++ " % " ++ show i2) == (I (mod i1 i2))
    it "evaluates (<)" $ testBinOp "<" ((<)::(Integer -> Integer -> Bool)) B
    it "evaluates (>)" $ testBinOp ">" ((>)::(Integer -> Integer -> Bool)) B
    it "evaluates (<=)" $ testBinOp "<=" ((<=)::(Integer -> Integer -> Bool)) B
    it "evaluates (>=)" $ testBinOp ">=" ((>=)::(Integer -> Integer -> Bool)) B

    it "evaluates binary boolean OR " $ testBinOp "||" (||) B
    it "evaluates binary boolean AND" $ testBinOp "&&" (&&) B

    it "evaluates unary minus" $ property $ \i -> expr ("- (" ++ show i ++ ")") == (I (-i))
    it "evaluates double negation" $ property $ \i -> expr ("--" ++ show i) == (I i)
    it "evaluates unary negation" $ property $ \i -> expr ("!" ++ show i) == (B (not i))

    it "evaluates tuples" $ do
      expr "(10, 20)" `shouldBe` T (I 10, I 20)
      expr "(10, True)" `shouldBe` T (I 10, B True)
      expr "(10, (True, False))" `shouldBe` T (I 10, T (B True, B False))

    it "evaluates lists" $ do
      expr "[]" `shouldBe` L []
      expr "10:[]" `shouldBe` L [I 10]
      expr "True:[]" `shouldBe` L [B True]
      expr "[]:[]" `shouldBe` L [L []]

  describe "builtin functions" $ do
    it "fst is the first projection" $ do
      run "Int main() { return fst( (10, 20) ); }" `shouldReturn` I 10
    it "snd is the second projection" $ do
      run "Int main() { return snd( (10, 20) ); }" `shouldReturn` I 20

    it "head is the head function" $ do
      run "Int main() { return head(10:[]); }" `shouldReturn` I 10
    it "tail is the tail function" $ do
      run "Int main() { return tail(10:[]); }" `shouldReturn` L []
    it "head and tail together" $ do
      run "Int main() { return head(tail(tail(10:20:30:[]))); }" `shouldReturn` I 30

    it "isEmpty gives True on the empty list" $ do
      run "Int main() { return isEmpty([]); }" `shouldReturn` B True
    it "isEmpty gives False on a non-empty list" $ do
      run "Int main() { return isEmpty(10:[]); }" `shouldReturn` B False


  describe "State" $ do
    it "retrieves the most recently added identifier in a singleton state" $ do
      envLookup "x" (("x" `envAdd` I 10) emptyEnvironment) `shouldBe` I 10
    it "retrieves the most recently added identifier in a state with more elements" $ do
      envLookup "y" ((("x" `envAdd` I 10) . ("y" `envAdd` B True)) emptyEnvironment) `shouldBe` B True
    it "retrieves an identifier somewhere inside the state" $ do
      envLookup "x" ((("x" `envAdd` I 10) . ("y" `envAdd` B True)) emptyEnvironment) `shouldBe` I 10

main :: IO ()
main = hspec spec
