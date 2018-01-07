module Lihsp.Utils.Recursion where

-- TODO Use `Fix`-based recursion schemes instead.
class Recursive a where
  bottomUp :: (a -> a) -> a -> a
