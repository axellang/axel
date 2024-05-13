{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Test.Eff.ProcessMock where

import Axel.Prelude

import Axel.Eff ((:>>))
import Axel.Eff.FileSystem as Effs
import Axel.Eff.Process as Effs

import Control.Lens

import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.State.Static.Local as Eff

import System.Exit

import TestUtils

newtype ProcessResult effs =
  ProcessResult ((ExitCode, Maybe (Text, Text)), Eff.Eff effs ())

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
    { _procMockArgs :: [Text]
    , _procExecutionLog :: [(Text, Maybe Text)]
    , _procMockResults :: [ProcessResult effs]
    }
  deriving (Eq, Show)

makeFieldsNoPrefix ''ProcessState

mkProcessState :: [Text] -> [ProcessResult effs] -> ProcessState effs
mkProcessState mockArgs mockResults =
  ProcessState
    { _procMockArgs = mockArgs
    , _procExecutionLog = []
    , _procMockResults = mockResults
    }

runProcess ::
     forall effs a. ('[ Eff.Error Text, Effs.FileSystem] :>> effs)
  => ProcessState effs
  -> Eff.Eff (Effs.Process ': effs) a
  -> Eff.Eff effs (a, ProcessState effs)
runProcess origProcessState =
  Eff.reinterpret (Eff.runState origProcessState) (const go)
  where
    go :: Process m a' -> Eff.Eff (Eff.State (ProcessState effs) ': effs) a'
    go (CreateIndependentProcess _) =
      throwInterpretError
        @(ProcessState effs)
        "CreateIndependentProcess"
        "Not implemented!"
    go (CreatePassthroughProcess _) =
      throwInterpretError
        @(ProcessState effs)
        "CreatePassthroughProcess"
        "Not implemented!"
    go GetArgs = Eff.gets (^. procMockArgs)
    go (HandleGetContents _) =
      throwInterpretError
        @(ProcessState effs)
        "HandleGetContents"
        "Not implemented!"
    go (HandleGetLine _) =
      throwInterpretError
        @(ProcessState effs)
        "HandleGetLine"
        "Not implemented!"
    go (HandleIsAtEnd _) =
      throwInterpretError
        @(ProcessState effs)
        "HandleIsAtEnd"
        "Not implemented!"
    go (WaitOnProcess _) =
      throwInterpretError
        @(ProcessState effs)
        "WaitOnProcess"
        "Not implemented!"
