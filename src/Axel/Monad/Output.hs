{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Output where

import Control.Monad.IO.Class (MonadIO, liftIO)

class MonadOutput m where
  outputStr :: String -> m ()
  outputStrLn :: String -> m ()

-- NOTE This is undecidable, but `mtl` uses undecidable instances in this scenario(?)....
--      Plus, I can't actually come up with a better solution.
instance (MonadIO m) => MonadOutput m where
  outputStr :: String -> m ()
  outputStr = liftIO . putStr
  outputStrLn :: String -> m ()
  outputStrLn = liftIO . putStrLn
