{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad.ResourceMock where

import Axel.Monad.Console as Console
import Axel.Monad.FileSystem as FS
import Axel.Monad.Process as Proc
import Axel.Monad.Resource as Res

import Control.Monad.Identity

import System.FilePath

newtype ResourceT m a =
  ResourceT (m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadConsole
           , MonadFileSystem
           , MonadProcess
           )

type Resource = ResourceT Identity

instance (Monad m) => MonadResource (ResourceT m) where
  getResourcePath (Res.ResourceId resourceId) =
    ResourceT $ pure ("resources" </> resourceId)

runResourceT :: ResourceT m a -> m a
runResourceT (ResourceT x) = x

runResource :: Resource a -> a
runResource = runIdentity . runResourceT
