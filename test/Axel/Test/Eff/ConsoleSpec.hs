module Axel.Test.Eff.ConsoleSpec where

import qualified Axel.Eff.Console as Console
import qualified Axel.Test.Eff.ConsoleMock as Mock

import Control.Monad.Freer as Eff

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Console :: SpecWith ()
spec_Console =
  describe "putStrLn" $ do
    it "prints to the console with a trailing newline" $ do
      let action = Console.putStrLn "line1\nline2"
      let origState = Mock.mkConsoleState
      let expected = Mock.ConsoleState {Mock._consoleOutput = "line1\nline2\n"}
      let result = Eff.run . Mock.runConsole origState $ action
      result `shouldBe` ((), expected)
