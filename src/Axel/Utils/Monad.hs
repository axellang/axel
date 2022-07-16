module Axel.Utils.Monad where

import Axel.Prelude

import Control.Monad (when)

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM cond action = do
  cond' <- cond
  when cond' $ action >> whileM cond action
