{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Eff.GhciMock where

import Axel.Eff.Ghci as Effs

import Control.Lens
import Polysemy
import Polysemy.Error as Effs
import Polysemy.State as Effs

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
  -> Sem (Effs.Ghci ': effs) a
  -> Sem effs (GhciState, a)
runGhci origState action = runState origState $ reinterpret go action
  where
    go :: Ghci m b -> Sem (Effs.State GhciState ': effs) b
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
