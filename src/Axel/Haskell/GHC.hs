{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Haskell.GHC where

import Axel.Monad.Haskell.Stack (axelStackageId, stackageResolverWithAxel)
import Axel.Monad.Process (MonadProcess(runProcess))

import Control.Monad.Except (MonadError, throwError)

import System.Exit (ExitCode(ExitFailure, ExitSuccess))

ghcCompile :: (MonadError String m, MonadProcess m) => FilePath -> m String
ghcCompile filePath = do
  (exitCode, stdout, stderr) <-
    runProcess
      "stack"
      [ "--resolver"
      , stackageResolverWithAxel
      , "ghc"
      , "--"
      , "-v0"
      , "-ddump-json"
      , filePath
      ]
      ""
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure _ -> throwError stderr

ghcInterpret :: (MonadError String m, MonadProcess m) => FilePath -> m String
ghcInterpret filePath = do
  (exitCode, stdout, stderr) <-
    runProcess
      "stack"
      [ "--resolver"
      , stackageResolverWithAxel
      , "runghc"
      , "--package"
      , axelStackageId
      , "--"
      , filePath
      ]
      ""
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure _ -> throwError stderr
