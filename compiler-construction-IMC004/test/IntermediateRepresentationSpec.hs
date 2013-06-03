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

irOf s = evalState (exp2ir (parse pExpr s)) ssmMachine

spec :: Spec
spec = do
  describe "record label management" $ do
    it "the first label starts at 1" $ evalState (recordLabel "x") ssmMachine `shouldBe` 1
    it "new labels get new numbers" $ evalState (recordLabel "x" >> recordLabel "y") ssmMachine `shouldBe` 2
    it "looking up a label returns the originally assigned number" $
      evalState (recordLabel "x" >> recordLabel "y" >> recordLabel "x") ssmMachine `shouldBe` 1

  describe "exp2ir for records" $ do
    it "translates a simple record" $ irOf "{x = 10, y = True}" `shouldBe`
      IrRecord [ (1, IrConst 10)
               , (2, IrConst (machineTrue ssmMachine))
               ]
    it "translates the empty record" $ irOf "{}" `shouldBe` IrRecord []
    it "translates a nested record" $ irOf "{z = { x = 10, y = True }, y = 30, x = False}" `shouldBe`
      IrRecord [ (1, (IrRecord -- z
                        [ (2, IrConst 10) -- x
                        , (3, IrConst (machineTrue ssmMachine))])) -- y
               , (3, IrConst 30) -- y
               , (2, IrConst (machineFalse ssmMachine))-- x
               ]
