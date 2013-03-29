{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module TypecheckerSpec (spec, main) where

import Test.Hspec
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy

import Parser
import Utils
import Typechecker

main :: IO ()
main = hspec spec

parse :: (SplParser a) -> String -> a
parse = runParser_ ""

unRight :: Either a b -> b
unRight (Right x) = x
unRight _ = undefined

-- inferExpr :: String -> SplType
-- inferExpr expr = unRight $ inferType emptyEnvironment (parse pExpr expr) (SplTypeVariable "a")

-- evaluate a typecheck action in an empty environment
evalTypecheckBare :: (Typecheck a) -> a
evalTypecheckBare t = unRight $ evalState (runEitherT (t)) (0, emptyEnvironment)

parseConvertShow s = show (evalTypecheckBare $ astType2splType (parse pReturnType s))

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
      parseConvertShow "a" `shouldBe` "{0}"
      parseConvertShow "[a]" `shouldBe` "[{0}]"
    it "replaces distinct type variables with distinct fresh ones" $ do
      parseConvertShow "(a, b)" `shouldBe` "({0}, {1})"
    it "replaces the same type variables with the same fresh ones" $ do
      parseConvertShow "(a, a)" `shouldBe` "({0}, {0})"
    it "replaces same with same and distinct with distinct type variables" $ do
      parseConvertShow "(a, (b, (b, a)))" `shouldBe` "({0}, ({1}, ({1}, {0})))"
