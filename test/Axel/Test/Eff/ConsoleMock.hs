{-# LANGUAGE TemplateHaskell #-}

module Axel.Test.Eff.ConsoleMock where

import Axel.Prelude

import Axel.Eff.Console as Effs

import Control.Lens

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.State.Static.Local as Eff

import TestUtils

newtype ConsoleState =
  ConsoleState
    { _consoleOutput :: Text
    }
  deriving (Eq, Show)

makeFieldsNoPrefix ''ConsoleState

mkConsoleState :: ConsoleState
mkConsoleState = ConsoleState {_consoleOutput = ""}

runConsole ::
     forall effs a. (Eff.Error Text :> effs)
  => ConsoleState
  -> Eff.Eff (Effs.Console ': effs) a
  -> Eff.Eff effs (a, ConsoleState)
runConsole origState = Eff.reinterpret (Eff.runState origState) (const go)
  where
    go :: Console m b -> Eff.Eff (Eff.State ConsoleState ': effs) b
    go GetTerminalSize =
      throwInterpretError "GetTerminalSize" "Not implemented!"
    go (PutStr str) = Eff.modify $ consoleOutput %~ (<> str)
