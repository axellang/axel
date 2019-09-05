{-# LANGUAGE TypeApplications #-}

module Axel.Test.Eff.ConsoleSpec where

import qualified Axel.Eff.Console as Console
import qualified Axel.Test.Eff.ConsoleMock as Mock

import Control.Monad.Freer as Eff
import Control.Monad.Freer.Error as Effs

import Test.Tasty.Hspec

import TestUtils

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Console :: SpecWith ()
spec_Console =
  describe "putStrLn" $ do
    it "prints to the console with a trailing newline" $ do
      let action = Console.putStrLn "line1\nline2"
      let origState = Mock.mkConsoleState
      let expected = Mock.ConsoleState {Mock._consoleOutput = "line1\nline2\n"}
      let result =
            unwrapRight $
            Eff.run . Effs.runError @String . Mock.runConsole origState $ action
      result `shouldBe` ((), expected)
