{-# LANGUAGE LambdaCase #-}

module Axel.Utils.List where

import Axel.Utils.Tuple
  ( Annotated
  , annotate
  , annotateWith
  , annotation
  , unannotate
  )

import Control.Category ((>>>))
import Control.Lens ((^.))

import Data.Function (on)
import Data.List (elemIndex, foldl', isPrefixOf, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE (groupBy, head, map)
import Data.Maybe (listToMaybe)

head' :: [a] -> Maybe a
head' = listToMaybe

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

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f =
  foldl'
    (\acc ->
       f >>> \case
         Just x -> x : acc
         Nothing -> acc)
    []
