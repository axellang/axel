{-# LANGUAGE FlexibleContexts #-}

module Axel.Haskell.GHC where

import Axel.Error (Error(MacroError))

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Version (showVersion)

import Paths_axel (version)

import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

stackResolverWithAxel :: String
stackResolverWithAxel = "nightly-2018-08-20"

hackageId :: String
hackageId = "axel-" <> showVersion version

-- TODO Rename to `ghcCompile`
buildWithGHC :: (MonadIO m) => FilePath -> m (Either String String)
buildWithGHC filePath = do
  (exitCode, stdout, stderr) <-
    liftIO $
    readProcessWithExitCode
      "stack"
      [ "--resolver"
      , stackResolverWithAxel
      , "ghc"
      , "--"
      , "-v0"
      , "-ddump-json"
      , filePath
      ]
      ""
  pure $
    case exitCode of
      ExitSuccess -> Right stdout
      ExitFailure _ -> Left stderr

-- TODO Rename to `ghcInterpret`
runWithGHC :: (MonadError Error m, MonadIO m) => FilePath -> m String
runWithGHC filePath = do
  (exitCode, stdout, stderr) <-
    liftIO $
    readProcessWithExitCode
      "stack"
      [ "--resolver"
      , stackResolverWithAxel
      , "runghc"
      , "--package"
      , hackageId
      , "--"
      , filePath
      ]
      ""
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure _ -> throwError $ MacroError stderr
