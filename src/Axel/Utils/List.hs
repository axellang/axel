module Axel.Utils.List where

import Data.List (isPrefixOf)

-- https://stackoverflow.com/a/26530188/2391244
takeUntil :: (Eq a) => [a] -> [a] -> [a]
takeUntil _ [] = []
takeUntil [] _ = []
takeUntil xs (y:ys) =
  if xs `isPrefixOf` (y : ys)
    then []
    else y : takeUntil xs ys
