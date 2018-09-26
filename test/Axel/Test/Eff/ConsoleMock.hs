{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Monad.ConsoleMock where

import Axel.Eff.Console as Effs

import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.State as Effs

newtype ConsoleState = ConsoleState
  { _consoleOutput :: String
  } deriving (Eq, Show)

makeFieldsNoPrefix ''ConsoleState

mkConsoleState :: ConsoleState
mkConsoleState = ConsoleState {_consoleOutput = ""}

runConsole ::
     forall effs a.
     ConsoleState
  -> Eff (Effs.Console ': effs) a
  -> Eff effs (a, ConsoleState)
runConsole origState action = runState origState $ reinterpret go action
  where
    go :: Console b -> Eff (Effs.State ConsoleState ': effs) b
    go (PutStr str) = modify @ConsoleState $ consoleOutput %~ (<> str)
