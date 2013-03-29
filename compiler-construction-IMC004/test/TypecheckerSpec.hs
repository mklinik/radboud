
{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module TypecheckerSpec (spec, main) where

import Test.Hspec

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

spec :: Spec
spec = do
  describe "typeVars" $ do
    it "gives the empty list for a base type" $ null $ typeVars (SplBaseType BaseTypeInt)

  -- describe "inferType" $ do
    -- it "infers that 5 is of type Int" $ inferExpr "5" `shouldBe` SplBaseType BaseTypeInt
    -- it "infers that True is of type Bool" $ inferExpr "True" `shouldBe` SplBaseType BaseTypeBool
