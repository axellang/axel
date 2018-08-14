{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module Axel.GHC where

import Axel.Error (Error(MacroError))

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

-- TODO Rename to `ghcCompile`
buildWithGHC :: (MonadIO m) => FilePath -> m (Either String String)
buildWithGHC filePath = do
  (exitCode, stdout, stderr) <-
    liftIO $
    readProcessWithExitCode
      "stack"
      ["--resolver", "lts-12.0", "ghc", "--", "-v0", "-ddump-json", filePath]
      ""
  pure $
    case exitCode of
      ExitSuccess -> Right stdout
      ExitFailure _ -> Left stderr

-- TODO Rename to `ghcInterpret`
runWithGHC :: (MonadError Error m, MonadIO m) => FilePath -> m String
runWithGHC filePath = do
  (exitCode, stdout, stderr) <-
    liftIO $ readProcessWithExitCode "stack" ["runghc", filePath] ""
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure _ -> throwError $ MacroError stderr
