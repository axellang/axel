{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Process where

import Axel.Prelude

import Control.Monad ((>=>))

import qualified Data.Text as T

import qualified Polysemy as Sem

import qualified System.Environment (getArgs)
import System.Exit (ExitCode)
import System.IO (Handle, hGetContents, hGetLine, hIsEOF)
import qualified System.Process as P

data Process m a where
  CreateIndependentProcess
    :: Text -> Process m (Handle, Handle, Handle, P.ProcessHandle)
  CreatePassthroughProcess :: Text -> Process m P.ProcessHandle
  GetArgs :: Process m [Text]
  HandleGetContents :: Handle -> Process m Text
  HandleGetLine :: Handle -> Process m Text
  HandleIsAtEnd :: Handle -> Process m Bool
  WaitOnProcess :: P.ProcessHandle -> Process m ExitCode

Sem.makeSem ''Process

runProcess ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Process ': effs) a
  -> Sem.Sem effs a
runProcess =
  Sem.interpret $ \case
    CreateIndependentProcess cmd ->
      Sem.embed $ do
        let config =
              (P.shell (T.unpack cmd))
                { P.std_in = P.CreatePipe
                , P.std_out = P.CreatePipe
                , P.std_err = P.CreatePipe
                }
        -- The handles will always be created because of `CreatePipe`, so we can safely unwrap them.
        (Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) <-
          P.createProcess config
        pure (stdinHandle, stdoutHandle, stderrHandle, processHandle)
    CreatePassthroughProcess cmd ->
      Sem.embed $ do
        let config = P.shell (T.unpack cmd)
        (_, _, _, processHandle) <- P.createProcess config
        pure processHandle
    GetArgs -> Sem.embed $ map T.pack <$> System.Environment.getArgs
    HandleGetContents handle -> Sem.embed $ T.pack <$> hGetContents handle
    HandleGetLine handle -> Sem.embed $ T.pack <$> hGetLine handle
    HandleIsAtEnd handle -> Sem.embed $ hIsEOF handle
    WaitOnProcess processHandle -> Sem.embed $ P.waitForProcess processHandle

readProcess ::
     (Sem.Member Process effs) => Text -> Sem.Sem effs (ExitCode, Text, Text)
readProcess cmd = do
  (_, stdoutHandle, stderrHandle, processHandle) <- createIndependentProcess cmd
  exitCode <- waitOnProcess processHandle
  stdout <- handleGetContents stdoutHandle
  stderr <- handleGetContents stderrHandle
  pure (exitCode, stdout, stderr)

passthroughProcess :: (Sem.Member Process effs) => Text -> Sem.Sem effs ExitCode
passthroughProcess = createPassthroughProcess >=> waitOnProcess
