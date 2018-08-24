{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Output where

import Control.Monad.IO.Class (MonadIO, liftIO)

class MonadOutput m where
  outputStr :: String -> m ()
  outputStrLn :: String -> m ()

instance (MonadIO m) => MonadOutput m where
  outputStr :: String -> m ()
  outputStr = liftIO . putStr
  outputStrLn :: String -> m ()
  outputStrLn = liftIO . putStrLn
