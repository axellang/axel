{- HLINT ignore "Redundant do" -}
module Axel.Test.Eff.ConsoleSpec where

import Axel.Prelude

import qualified Axel.Eff.Console as Console
import qualified Axel.Test.Eff.ConsoleMock as Mock

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

import Test.Hspec

import TestUtils

spec_Console :: Spec
spec_Console =
  describe "putStrLn" $ do
    it "prints to the console with a trailing newline" $ do
      let action = Console.putStrLn "line1\nline2"
      let origState = Mock.mkConsoleState
      let expected = Mock.ConsoleState {Mock._consoleOutput = "line1\nline2\n"}
      let result =
            unwrapRight id $
            Eff.runPureEff .
            Eff.runErrorNoCallStack @Text . Mock.runConsole origState $
            action
      result `shouldBe` ((), expected)
