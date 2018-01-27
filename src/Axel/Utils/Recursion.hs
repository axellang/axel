module Axel.Utils.Recursion where

exhaustM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
exhaustM f x = do
  result <- f x
  if x == result
    then return result
    else exhaustM f result

-- TODO Use `Fix`-based recursion schemes, implement `Traversable`, and implement `Foldable` instead.
class Recursive a where
  bottomUpFmap :: (a -> a) -> a -> a
  -- TODO Remove dependency on `Monad` in favor of `Applicative`
  --      (which is all the standard `traverse` requires).
  bottomUpTraverse :: (Monad m) => (a -> m a) -> a -> m a
