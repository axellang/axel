{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Process where

import Axel.Prelude

import Control.Monad ((>=>))

import qualified Data.Text as T

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.TH as Eff

import qualified System.Environment (getArgs)
import System.Exit (ExitCode)
import System.IO (Handle, hGetContents, hGetLine, hIsEOF)
import qualified System.Process as P

data Process :: Eff.Effect where
  CreateIndependentProcess
    :: Text -> Process m (Handle, Handle, Handle, P.ProcessHandle)
  CreatePassthroughProcess :: Text -> Process m P.ProcessHandle
  GetArgs :: Process m [Text]
  HandleGetContents :: Handle -> Process m Text
  HandleGetLine :: Handle -> Process m Text
  HandleIsAtEnd :: Handle -> Process m Bool
  WaitOnProcess :: P.ProcessHandle -> Process m ExitCode

Eff.makeEffect ''Process

runProcess :: (Eff.IOE :> effs) => Eff.Eff (Process ': effs) a -> Eff.Eff effs a
runProcess =
  Eff.interpret $ \_ ->
    \case
      CreateIndependentProcess cmd ->
        Eff.liftIO $ do
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
        Eff.liftIO $ do
          let config = P.shell (T.unpack cmd)
          (_, _, _, processHandle) <- P.createProcess config
          pure processHandle
      GetArgs -> Eff.liftIO $ map T.pack <$> System.Environment.getArgs
      HandleGetContents handle -> Eff.liftIO $ T.pack <$> hGetContents handle
      HandleGetLine handle -> Eff.liftIO $ T.pack <$> hGetLine handle
      HandleIsAtEnd handle -> Eff.liftIO $ hIsEOF handle
      WaitOnProcess processHandle -> Eff.liftIO $ P.waitForProcess processHandle

readProcess :: (Process :> effs) => Text -> Eff.Eff effs (ExitCode, Text, Text)
readProcess cmd = do
  (_, stdoutHandle, stderrHandle, processHandle) <- createIndependentProcess cmd
  exitCode <- waitOnProcess processHandle
  stdout <- handleGetContents stdoutHandle
  stderr <- handleGetContents stderrHandle
  pure (exitCode, stdout, stderr)

passthroughProcess :: (Process :> effs) => Text -> Eff.Eff effs ExitCode
passthroughProcess = createPassthroughProcess >=> waitOnProcess
