module Axel.Utils.List where

import Axel.Utils.Tuple (annotate, annotateWith, annotation, unannotate)

import Control.Lens ((^.))

import Data.Function (on)
import Data.List (findIndex, isPrefixOf, sortBy)
import qualified Data.List.NonEmpty as NE (groupBy, head, toList)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)

head' :: [a] -> Maybe a
head' = listToMaybe

groupAllWith :: (Ord b) => (a -> b) -> [a] -> [([a], b)]
groupAllWith f =
  map extractAnnotation .
  NE.groupBy ((==) `on` (^. annotation)) . map (annotateWith f)
  where
    extractAnnotation xs =
      annotate (NE.head xs ^. annotation) (map unannotate $ NE.toList xs)

-- This is not stable in the technical sense of the term, just roughly stable-ish for limited use cases.
stablyGroupAllWith :: (Eq a, Ord b) => (a -> b) -> [a] -> [([a], b)]
stablyGroupAllWith f xs =
  map unannotate $
  sortBy (comparing (^. annotation)) $
  map
    (annotateWith $ \(groupRepresentative:_, _) ->
       findIndex (== groupRepresentative) xs) $
  groupAllWith f xs

remove :: (Eq a) => (a -> Bool) -> [a] -> ([a], [a])
remove f xs =
  let removed = filter f xs
      xs' = filter (not . (`elem` removed)) xs
   in (xs', removed)

-- https://stackoverflow.com/a/26530188/2391244
takeUntil :: (Eq a) => [a] -> [a] -> [a]
takeUntil _ [] = []
takeUntil [] _ = []
takeUntil xs (y:ys) =
  if xs `isPrefixOf` (y : ys)
    then []
    else y : takeUntil xs ys
