{-# LANGUAGE FlexibleContexts #-}

module Lihsp.Eval where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Lihsp.Error (Error(MacroError))

import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.IO (Handle, hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)

withTempFile :: (MonadIO m) => FilePath -> (FilePath -> Handle -> m a) -> m a
withTempFile nameTemplate f = do
  temporaryDirectory <- liftIO getTemporaryDirectory
  (fileName, handle) <- liftIO $ openTempFile temporaryDirectory nameTemplate
  result <- f fileName handle
  liftIO $ hClose handle
  liftIO $ removeFile fileName
  return result

execInterpreter :: (MonadError Error m, MonadIO m) => FilePath -> m String
execInterpreter fileName = do
  (code, stdout, stderr) <-
    liftIO $ readProcessWithExitCode "runhaskell" [fileName] ""
  case code of
    ExitSuccess -> return stdout
    ExitFailure _ -> throwError $ MacroError stderr

evalSource :: (MonadError Error m, MonadIO m) => String -> m String
evalSource source =
  withTempFile "TempEval.hs" $ \fileName handle -> do
    liftIO $ hPutStr handle source
    execInterpreter fileName
