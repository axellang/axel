{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Test.Monad.ProcessMock where

import Axel.Monad.Console as Console
import Axel.Monad.FileSystem as FS
import Axel.Monad.Process as Proc
import Axel.Monad.Resource as Res
import Axel.Test.MockUtils
import Axel.Test.Monad.FileSystemMock as Mock

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Lazy

import System.Exit

newtype ProcessResultT m =
  ProcessResultT ((ExitCode, Maybe (String, String)), Mock.FileSystemT m ())
  deriving (Eq, Show)

data ProcessStateT m = ProcessStateT
  { _procMockArgs :: [String]
  , _procExecutionLog :: [(String, [String], Maybe String)]
  , _procMockResults :: [ProcessResultT m]
  } deriving (Eq, Show)

makeFieldsNoPrefix ''ProcessStateT

type ProcessState = ProcessStateT Identity

mkProcessState :: [String] -> [ProcessResultT m] -> ProcessStateT m
mkProcessState mockArgs mockResults =
  ProcessStateT
    { _procMockArgs = mockArgs
    , _procExecutionLog = []
    , _procMockResults = mockResults
    }

newtype ProcessT m a =
  ProcessT (StateT (ProcessStateT m) (ExceptT String (Mock.FileSystemT m)) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadConsole
           , MonadFileSystem
           , MonadResource
           )

type Process = ProcessT Identity

deriving instance
         (Eq
            (StateT (ProcessStateT m) (ExceptT String (Mock.FileSystemT m))
               a)) =>
         Eq (ProcessT m a)

deriving instance
         (Show
            (StateT (ProcessStateT m) (ExceptT String (Mock.FileSystemT m))
               a)) =>
         Show (ProcessT m a)

instance MonadTrans ProcessT where
  lift = ProcessT . lift . lift . lift

instance (Monad m) => MonadProcess (ProcessT m) where
  getArgs = ProcessT $ gets (^. procMockArgs)
  runProcess cmd args stdin =
    ProcessT $ do
      procExecutionLog %= (|> (cmd, args, Just stdin))
      gets (uncons . (^. procMockResults)) >>= \case
        Just (ProcessResultT (mockResult, fsAction), newMockResults) -> do
          procMockResults .= newMockResults
          case mockResult of
            (exitCode, Just (stdout, stderr)) ->
              lift (lift fsAction) >> pure (exitCode, stdout, stderr)
            _ ->
              throwInterpretError
                "RunProcess"
                ("Wrong type for mock result: " <> show mockResult)
        Nothing -> throwInterpretError "runProcess" "No mock result available"
  runProcessInheritingStreams cmd args =
    ProcessT $ do
      procExecutionLog %= (|> (cmd, args, Nothing))
      gets (uncons . (^. procMockResults)) >>= \case
        Just (ProcessResultT (mockResult, fsAction), newMockResults) -> do
          procMockResults .= newMockResults
          case mockResult of
            (exitCode, Nothing) -> pure fsAction >> pure exitCode
            _ ->
              throwInterpretError
                "runProcessInheritingStreams"
                ("Wrong type for mock result: " <> show mockResult)
        Nothing ->
          throwInterpretError
            "RunProcessInheritingStreams"
            "No mock result available"

runProcessT ::
     forall a m. (Monad m)
  => (ProcessStateT m, Mock.FSState)
  -> ProcessT m a
  -> ExceptT String m (a, (ProcessStateT m, Mock.FSState))
runProcessT (origState, origFSState) (ProcessT x) =
  ExceptT $
  runExceptT (runFileSystemT origFSState $ runExceptT $ runStateT x origState) <&> \case
    Left err -> Left err
    Right (x'', fsState) ->
      case x'' of
        Left err -> Left err
        Right (x''', procState) -> Right (x''', (procState, fsState))

runProcess ::
     (ProcessState, Mock.FSState)
  -> Process a
  -> Either String (a, (ProcessState, Mock.FSState))
runProcess origState x = runExcept $ runProcessT origState x
