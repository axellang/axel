module Lihsp.Utils.Monad where

exhaustM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
exhaustM f x = do
  result <- f x
  if x == result
    then return result
    else exhaustM f result
