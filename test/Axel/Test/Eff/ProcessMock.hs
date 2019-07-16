{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Test.Eff.ProcessMock where

import Axel.Eff.FileSystem as Effs
import Axel.Eff.Process as Effs
import Axel.Test.MockUtils

import Control.Lens
import Control.Monad.Freer as Eff
import Control.Monad.Freer.Error as Effs
import Control.Monad.Freer.State as Effs

import Data.Functor

import System.Exit

newtype ProcessResult effs =
  ProcessResult ((ExitCode, Maybe (String, String)), Eff effs ())

-- | We are pretending that all `ProcessResult`s are unique no matter what, for simplicity's sake.
instance Eq (ProcessResult effs) where
  (==) :: ProcessResult effs -> ProcessResult effs -> Bool
  _ == _ = False

-- | We are pretending that all `ProcessResult`s are unique no matter what, for simplicity's sake.
instance Show (ProcessResult effs) where
  show :: ProcessResult effs -> String
  show _ = "<ProcessResult>"

data ProcessState effs =
  ProcessState
    { _procMockArgs :: [String]
    , _procExecutionLog :: [(String, Maybe String)]
    , _procMockResults :: [ProcessResult effs]
    }
  deriving (Eq, Show)

makeFieldsNoPrefix ''ProcessState

mkProcessState :: [String] -> [ProcessResult effs] -> ProcessState effs
mkProcessState mockArgs mockResults =
  ProcessState
    { _procMockArgs = mockArgs
    , _procExecutionLog = []
    , _procMockResults = mockResults
    }

runProcess ::
     forall effs a. (Members '[ Effs.Error String, Effs.FileSystem] effs)
  => ProcessState effs
  -> Eff (Effs.Process ': effs) a
  -> Eff effs (a, ProcessState effs)
runProcess origProcessState = runState origProcessState . reinterpret go
  where
    go :: Process ~> Eff (Effs.State (ProcessState effs) ': effs)
    go GetArgs = gets @(ProcessState effs) (^. procMockArgs)
    go (RunProcessCreatingStreams cmd stdin) = do
      modify @(ProcessState effs) $ procExecutionLog %~ (|> (cmd, Just stdin))
      gets @(ProcessState effs) (uncons . (^. procMockResults)) >>= \case
        Just (ProcessResult (mockResult, fsAction), newMockResults) -> do
          modify @(ProcessState effs) $ procMockResults .~ newMockResults
          case mockResult of
            (exitCode, Just (stdout, stderr)) ->
              raise fsAction $> (exitCode, stdout, stderr)
            _ ->
              throwInterpretError
                @(ProcessState effs)
                "RunProcess"
                ("Wrong type for mock result: " <> show mockResult)
        Nothing ->
          throwInterpretError
            @(ProcessState effs)
            "RunProcess"
            "No mock result available"
    go (RunProcessInheritingStreams cmd) = do
      modify @(ProcessState effs) $ procExecutionLog %~ (|> (cmd, Nothing))
      gets @(ProcessState effs) (uncons . (^. procMockResults)) >>= \case
        Just (ProcessResult (mockResult, fsAction), newMockResults) -> do
          modify @(ProcessState effs) $ procMockResults .~ newMockResults
          case mockResult of
            (exitCode, Nothing) -> raise fsAction $> exitCode
            _ ->
              throwInterpretError
                @(ProcessState effs)
                "RunProcessInheritingStreams"
                ("Wrong type for mock result: " <> show mockResult)
        Nothing ->
          throwInterpretError
            @(ProcessState effs)
            "RunProcessInheritingStreams"
            "No mock result available"
