{-# LANGUAGE TemplateHaskell #-}

module Axel.Test.Eff.GhciMock where

import Axel.Prelude

import Axel.Eff.Ghci as Effs

import Control.Lens

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local

import TestUtils

data GhciState =
  GhciState
    { _ghciExecutionLog :: [Text]
    , _ghciMockResults :: [[Text]]
    }
  deriving (Eq, Show)

makeFieldsNoPrefix ''GhciState

mkGhciState :: [[Text]] -> GhciState
mkGhciState = GhciState []

runGhci ::
     forall effs a. (Error Text :> effs)
  => GhciState
  -> Eff (Effs.Ghci ': effs) a
  -> Eff effs (a, GhciState)
runGhci origState = reinterpret (runState origState) (const go)
  where
    go :: Ghci m b -> Eff (State GhciState ': effs) b
    go (Exec _ command) = do
      modify @GhciState $ ghciExecutionLog %~ (|> command)
      gets @GhciState (uncons . (^. ghciMockResults)) >>= \case
        Just (mockResult, newMockResults) -> do
          modify @GhciState $ ghciMockResults .~ newMockResults
          pure mockResult
        Nothing ->
          throwInterpretError @GhciState "Exec" "Mock result not available"
    go Start = throwInterpretError @GhciState "Start" "Not implemented!"
    go (Stop _) = throwInterpretError @GhciState "Stop" "Not implemented!"
