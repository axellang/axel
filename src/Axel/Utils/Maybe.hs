module Axel.Utils.Maybe where

import Axel.Prelude

import Data.Functor.Identity (runIdentity)

foldMUntilNothing :: (Monad m) => (a -> Maybe a) -> (a -> m a) -> a -> m a
foldMUntilNothing move modify x = do
  modified <- modify x
  case move modified of
    Nothing -> pure modified
    Just moved -> foldMUntilNothing move modify moved

foldUntilNothing :: (a -> Maybe a) -> (a -> a) -> a -> a
foldUntilNothing move modify =
  runIdentity . foldMUntilNothing move (pure . modify)
