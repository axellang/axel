{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Eff.ConsoleMock where

import Axel.Eff.Console as Effs

import Control.Lens

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.State as Sem

import TestUtils

newtype ConsoleState =
  ConsoleState
    { _consoleOutput :: String
    }
  deriving (Eq, Show)

makeFieldsNoPrefix ''ConsoleState

mkConsoleState :: ConsoleState
mkConsoleState = ConsoleState {_consoleOutput = ""}

runConsole ::
     forall effs a. (Sem.Member (Sem.Error String) effs)
  => ConsoleState
  -> Sem.Sem (Effs.Console ': effs) a
  -> Sem.Sem effs (ConsoleState, a)
runConsole origState action = Sem.runState origState $ Sem.reinterpret go action
  where
    go :: Console m b -> Sem.Sem (Sem.State ConsoleState ': effs) b
    go GetTerminalSize =
      throwInterpretError "GetTerminalSize" "Not implemented!"
    go (PutStr str) = Sem.modify $ consoleOutput %~ (<> str)
