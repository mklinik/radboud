{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}

module BackendSsmSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import Data.List (intersperse)
import System.IO
import System.IO.Unsafe
import System.Process

import Parser
import Utils
import Ast
import BackendSsm
import IntermediateRepresentation

main :: IO ()
main = hspec spec

testFilename = "test.ssm"

runSsm :: [String] -> IO [String]
runSsm asm = do
  f <- openFile testFilename WriteMode
  mapM_ (hPutStrLn f) asm
  hClose f
  output <- readProcess "java" ["-jar", "ssm-nogui.jar", testFilename] []
  return $ lines output

runE :: String -> String
runE expr = head $ unsafePerformIO (runSsm $ generateE [] $ ast2ir $ parse pExpr expr)


spec :: Spec
spec = do
  describe "addition" $ do
    it "5 + 3" $ runE "5 + 3" `shouldBe` "8"
    it "5 + -3" $ runE "5 + -3" `shouldBe` "2"
    it "-5 + -3" $ runE "-5 + -3" `shouldBe` "-8"
  describe "subtraction" $ do
    it "5 - 3" $ runE "5 - 3" `shouldBe` "2"
    it "-5 - 3" $ runE "-5 - 3" `shouldBe` "-8"
    it "-5 - -3" $ runE "-5 - -3" `shouldBe` "-2"
