module Lihsp.Utils.Recursion where

-- TODO Use `Fix`-based recursion schemes instead.
class Recursive a where
  bottomUp :: (a -> a) -> a -> a
  -- TODO Remove dependency on `Monad` in favor of `Applicative`
  --      (which is all the standard `traverse` requires).
  bottomUpTraverse :: (Monad m) => (a -> m a) -> a -> m a
