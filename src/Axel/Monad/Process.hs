{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Process where

import Control.Monad.IO.Class (MonadIO, liftIO)

import System.Exit (ExitCode)
import qualified System.Process (readProcess, readProcessWithExitCode)
import System.Process.Typed (ProcessConfig)
import qualified System.Process.Typed (runProcess)

class MonadProcess m where
  readProcess :: FilePath -> [String] -> String -> m String
  readProcessWithExitCode ::
       FilePath -> [String] -> String -> m (ExitCode, String, String)
  runProcessInheritingStreams :: ProcessConfig stdin stdout stderr -> m ExitCode

-- NOTE This is undecidable, but `mtl` uses undecidable instances in this scenario(?)....
--      Plus, I can't actually come up with a better solution.
instance (MonadIO m) => MonadProcess m where
  readProcess :: FilePath -> [String] -> String -> m String
  readProcess cmd args stdin =
    liftIO $ System.Process.readProcess cmd args stdin
  readProcessWithExitCode ::
       FilePath -> [String] -> String -> m (ExitCode, String, String)
  readProcessWithExitCode cmd args stdin =
    liftIO $ System.Process.readProcessWithExitCode cmd args stdin
  runProcessInheritingStreams = liftIO . System.Process.Typed.runProcess
