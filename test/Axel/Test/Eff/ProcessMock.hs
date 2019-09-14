{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Test.Eff.ProcessMock where

import Axel.Prelude

import Axel.Eff.FileSystem as Effs
import Axel.Eff.Process as Effs

import Control.Lens

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.State as Sem

import Data.Functor

import System.Exit

import TestUtils

newtype ProcessResult effs =
  ProcessResult ((ExitCode, Maybe (Text, Text)), Sem.Sem effs ())

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
     forall effs a. (Sem.Members '[ Sem.Error Text, Effs.FileSystem] effs)
  => ProcessState effs
  -> Sem.Sem (Effs.Process ': effs) a
  -> Sem.Sem effs (ProcessState effs, a)
runProcess origProcessState = Sem.runState origProcessState . Sem.reinterpret go
  where
    go :: Process m a' -> Sem.Sem (Sem.State (ProcessState effs) ': effs) a'
    go GetArgs = Sem.gets (^. procMockArgs)
    go (RunProcessCreatingStreams cmd stdin) = do
      Sem.modify $ procExecutionLog %~ (|> (cmd, Just stdin))
      Sem.gets (uncons . (^. procMockResults)) >>= \case
        Just (ProcessResult (mockResult, fsAction), newMockResults) -> do
          Sem.modify $ procMockResults .~ newMockResults
          case mockResult of
            (exitCode, Just (stdout, stderr)) ->
              Sem.raise fsAction $> (exitCode, stdout, stderr)
            _ ->
              throwInterpretError
                "RunProcess"
                ("Wrong type for mock result: " <> showText mockResult)
        Nothing -> throwInterpretError "RunProcess" "No mock result available"
    go (RunProcessInheritingStreams cmd) = do
      Sem.modify $ procExecutionLog %~ (|> (cmd, Nothing))
      Sem.gets (uncons . (^. procMockResults)) >>= \case
        Just (ProcessResult (mockResult, fsAction), newMockResults) -> do
          Sem.modify $ procMockResults .~ newMockResults
          case mockResult of
            (exitCode, Nothing) -> Sem.raise fsAction $> exitCode
            _ ->
              throwInterpretError
                "RunProcessInheritingStreams"
                ("Wrong type for mock result: " <> showText mockResult)
        Nothing ->
          throwInterpretError
            "RunProcessInheritingStreams"
            "No mock result available"
