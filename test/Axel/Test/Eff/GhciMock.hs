{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Eff.GhciMock where

import Axel.Eff.Ghci as Effs

import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.Error as Effs
import Control.Monad.Freer.State as Effs

import TestUtils

data GhciState =
  GhciState
    { _ghciExecutionLog :: [String]
    , _ghciMockResults :: [[String]]
    }
  deriving (Eq, Show)

makeFieldsNoPrefix ''GhciState

mkGhciState :: [[String]] -> GhciState
mkGhciState = GhciState []

runGhci ::
     forall effs a. (Member (Effs.Error String) effs)
  => GhciState
  -> Eff (Effs.Ghci ': effs) a
  -> Eff effs (a, GhciState)
runGhci origState action = runState origState $ reinterpret go action
  where
    go :: Ghci b -> Eff (Effs.State GhciState ': effs) b
    go (Exec _ command) = do
      modify @GhciState $ ghciExecutionLog %~ (|> command)
      gets @GhciState (uncons . (^. ghciMockResults)) >>= \case
        Just (mockResult, newMockResults) -> do
          modify @GhciState $ ghciMockResults .~ newMockResults
          pure mockResult
        Nothing ->
          throwInterpretError @GhciState "Exec" "No mock result available"
    go Start = throwInterpretError @GhciState "Start" "Not implemented!"
    go (Stop _) = throwInterpretError @GhciState "Stop" "Not implemented!"
