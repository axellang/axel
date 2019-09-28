module Axel.Utils.List where

import Axel.Prelude

import Axel.Utils.Tuple
  ( Annotated
  , annotate
  , annotateWith
  , annotation
  , unannotate
  )

import Control.Lens ((%~), (&), (^.), _1, _2)

import Data.Function (on)
import Data.List (elemIndex, isPrefixOf, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE (groupBy, head, map)
import Data.Maybe (fromJust, listToMaybe)

head' :: [a] -> Maybe a
head' = listToMaybe

-- | 'unsafeHead' == 'head'.
unsafeHead :: [a] -> a
unsafeHead = fromJust . listToMaybe

groupAllWith :: (Ord b) => (a -> b) -> [a] -> [Annotated b (NonEmpty a)]
groupAllWith f =
  map extractAnnotation .
  NE.groupBy ((==) `on` (^. annotation)) . map (annotateWith f)
  where
    extractAnnotation xs =
      annotate (NE.head xs ^. annotation) (NE.map unannotate xs)

-- This is not stable in the technical sense of the term, just roughly stable-ish for limited use cases.
stablyGroupAllWith ::
     (Eq a, Ord b) => (a -> b) -> [a] -> [Annotated b (NonEmpty a)]
stablyGroupAllWith f xs =
  map unannotate $
  sortOn (^. annotation) $
  map
    (annotateWith $ \(groupRepresentative :| _, _) ->
       elemIndex groupRepresentative xs) $
  groupAllWith f xs

removeOut :: (Eq a) => (a -> Bool) -> [a] -> ([a], [a])
removeOut f xs =
  let removed = filter f xs
      xs' = filter (not . (`elem` removed)) xs
   in (xs', removed)

remove :: (Eq a) => (a -> Bool) -> [a] -> [a]
remove f xs = fst $ removeOut f xs

-- https://stackoverflow.com/a/26530188/2391244
takeUntil :: (Eq a) => [a] -> [a] -> [a]
takeUntil _ [] = []
takeUntil [] _ = []
takeUntil xs (y:ys) =
  if xs `isPrefixOf` (y : ys)
    then []
    else y : takeUntil xs ys

filterMapOut :: (a -> Maybe b) -> [a] -> ([a], [b])
filterMapOut f =
  foldr
    (\x acc ->
       case f x of
         Just x' -> acc & _2 %~ (x' :)
         Nothing -> acc & _1 %~ (x :))
    ([], [])

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f xs = snd $ filterMapOut f xs
