module Monad.ConsoleSpec where

import qualified Axel.Monad.Console as Console

import qualified Monad.ConsoleMock as Mock

import Test.Tasty.Hspec

spec_Console :: SpecWith ()
spec_Console =
  describe "Console" $ do
    describe "putStrLn" $ do
      it "prints to the console with a trailing newline" $ do
        let action = Console.putStrLn "line1\nline2"
            origState = Mock.mkConsoleState
            expected =
              Mock.ConsoleState {Mock._consoleOutput = "line1\nline2\n"}
        case Mock.runConsoleT origState action of
          Left err -> error err
          Right result -> result `shouldBe` ((), expected)
