{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Console where

import Prelude hiding (putStr)
import qualified Prelude

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

class (Monad m) =>
      MonadConsole m
  where
  putStr :: String -> m ()
  default putStr :: (MonadTrans t, MonadConsole m', m ~ t m') =>
    String -> m ()
  putStr = lift . putStr

instance (MonadConsole m) => MonadConsole (ContT r m)

instance (MonadConsole m) => MonadConsole (ExceptT e m)

instance (MonadConsole m) => MonadConsole (IdentityT m)

instance (MonadConsole m) => MonadConsole (MaybeT m)

instance (MonadConsole m) => MonadConsole (ReaderT r m)

instance (Monoid w, MonadConsole m) => MonadConsole (LazyRWS.RWST r w s m)

instance (Monoid w, MonadConsole m) =>
         MonadConsole (StrictRWS.RWST r w s m)

instance (MonadConsole m) => MonadConsole (LazyState.StateT s m)

instance (MonadConsole m) => MonadConsole (StrictState.StateT s m)

instance (Monoid w, MonadConsole m) =>
         MonadConsole (LazyWriter.WriterT w m)

instance (Monoid w, MonadConsole m) =>
         MonadConsole (StrictWriter.WriterT w m)

instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => MonadConsole m where
  putStr :: String -> m ()
  putStr = liftIO . Prelude.putStr

putStrLn :: (MonadConsole m) => String -> m ()
putStrLn str = putStr (str <> "\n")
