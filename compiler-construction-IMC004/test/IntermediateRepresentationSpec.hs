{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module IntermediateRepresentationSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck
import System.IO
import System.IO.Unsafe
import System.Process
import Control.Monad.Trans.State

import Parser
import Utils
import IntermediateRepresentation
import BackendSsm

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "record label management" $ do
    it "the first label starts at 1" $ evalState (recordLabel "x") ssmMachine `shouldBe` 1
    it "new labels get new numbers" $ evalState (recordLabel "x" >> recordLabel "y") ssmMachine `shouldBe` 2
    it "looking up a label returns the originally assigned number" $
      evalState (recordLabel "x" >> recordLabel "y" >> recordLabel "x") ssmMachine `shouldBe` 1
