{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Process where

import Control.Monad.IO.Class (MonadIO, liftIO)
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

import qualified System.Environment (getArgs)
import System.Exit (ExitCode)
import qualified System.Process (readProcessWithExitCode)
import qualified System.Process.Typed (proc, runProcess)

class (Monad m) =>
      MonadProcess m
  where
  getArgs :: m [String]
  default getArgs :: (MonadTrans t, MonadProcess m', m ~ t m') =>
    m [String]
  getArgs = lift getArgs
  runProcess :: FilePath -> [String] -> String -> m (ExitCode, String, String)
  default runProcess :: (MonadTrans t, MonadProcess m', m ~ t m') =>
    FilePath -> [String] -> String -> m (ExitCode, String, String)
  runProcess cmd args stdin = lift $ runProcess cmd args stdin
  runProcessInheritingStreams :: FilePath -> [String] -> m ExitCode
  default runProcessInheritingStreams :: ( MonadTrans t
                                         , MonadProcess m'
                                         , m ~ t m'
                                         ) =>
    FilePath -> [String] -> m ExitCode
  runProcessInheritingStreams cmd args =
    lift $ runProcessInheritingStreams cmd args

instance (MonadProcess m) => MonadProcess (ContT r m)

instance (MonadProcess m) => MonadProcess (ExceptT e m)

instance (MonadProcess m) => MonadProcess (IdentityT m)

instance (MonadProcess m) => MonadProcess (MaybeT m)

instance (MonadProcess m) => MonadProcess (ReaderT r m)

instance (Monoid w, MonadProcess m) => MonadProcess (LazyRWS.RWST r w s m)

instance (Monoid w, MonadProcess m) =>
         MonadProcess (StrictRWS.RWST r w s m)

instance (MonadProcess m) => MonadProcess (LazyState.StateT s m)

instance (MonadProcess m) => MonadProcess (StrictState.StateT s m)

instance (Monoid w, MonadProcess m) =>
         MonadProcess (LazyWriter.WriterT w m)

instance (Monoid w, MonadProcess m) =>
         MonadProcess (StrictWriter.WriterT w m)

instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => MonadProcess m where
  getArgs :: m [String]
  getArgs = liftIO System.Environment.getArgs
  runProcess :: FilePath -> [String] -> String -> m (ExitCode, String, String)
  runProcess cmd args stdin =
    liftIO $ System.Process.readProcessWithExitCode cmd args stdin
  runProcessInheritingStreams cmd args =
    liftIO $
    System.Process.Typed.runProcess (System.Process.Typed.proc cmd args)
