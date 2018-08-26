{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Haskell.GHC where

import Axel.Error (Error(MacroError))
import Axel.Monad.Haskell.Stack (axelStackageId, stackageResolverWithAxel)
import Axel.Monad.Process (MonadProcess(runProcess))

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)

import System.Exit (ExitCode(ExitFailure, ExitSuccess))

class (Monad m) =>
      MonadGHC m
  where
  ghcCompile :: FilePath -> m String
  default ghcCompile :: (MonadTrans t, MonadGHC m', m ~ t m') =>
    FilePath -> m String
  ghcCompile = lift . ghcCompile
  ghcInterpret :: FilePath -> m String
  default ghcInterpret :: (MonadTrans t, MonadGHC m', m ~ t m') =>
    FilePath -> m String
  ghcInterpret = lift . ghcInterpret

instance (MonadGHC m) => MonadGHC (ContT r m)

instance (MonadGHC m) => MonadGHC (ExceptT e m)

instance (MonadGHC m) => MonadGHC (IdentityT m)

instance (MonadGHC m) => MonadGHC (MaybeT m)

instance (MonadGHC m) => MonadGHC (ReaderT r m)

instance (Monoid w, MonadGHC m) => MonadGHC (LazyRWS.RWST r w s m)

instance (Monoid w, MonadGHC m) => MonadGHC (StrictRWS.RWST r w s m)

instance (MonadGHC m) => MonadGHC (LazyState.StateT s m)

instance (MonadGHC m) => MonadGHC (StrictState.StateT s m)

instance (Monoid w, MonadGHC m) => MonadGHC (LazyWriter.WriterT w m)

instance (Monoid w, MonadGHC m) => MonadGHC (StrictWriter.WriterT w m)

instance {-# OVERLAPPABLE #-} (Monad m, MonadError Error m, MonadProcess m) =>
                              MonadGHC m where
  ghcCompile :: FilePath -> m String
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
      ExitFailure _ -> throwError $ MacroError stderr
  ghcInterpret :: FilePath -> m String
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
      ExitFailure _ -> throwError $ MacroError stderr
