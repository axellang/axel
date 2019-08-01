{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Utils.Recursion where

import Control.Monad.Freer (Eff)
import qualified Control.Monad.Freer as Effs (run)

import Data.Generics.Uniplate.Zipper (Zipper, hole)

exhaustM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
exhaustM f x = do
  result <- f x
  if x == result
    then pure result
    else exhaustM f result

-- TODO Remove dependencies on `Monad` in favor of `Applicative`
--      (which is all that `traverse` requires).
type Traverse m a
   = (Monad m) =>
       (a -> m a) -> a -> m a

type Fmap a = (a -> a) -> a -> a

mkFmapFromTraverse :: Traverse (Eff '[]) a -> Fmap a
mkFmapFromTraverse traverseFn f = Effs.run . traverseFn (pure . f)

class Recursive a where
  bottomUpTraverse :: Traverse m a
  topDownTraverse :: Traverse m a

bottomUpFmap :: (Recursive a) => Fmap a
bottomUpFmap = mkFmapFromTraverse bottomUpTraverse

topDownFmap :: (Recursive a) => Fmap a
topDownFmap f = Effs.run . topDownTraverse (pure . f)

-- TODO Remove dependencies on `Monad` in favor of `Applicative`
--      (which is all that `traverse` requires).
type ZipperTraverse m a
   = (Monad m) =>
       (Zipper a a -> m a) -> a -> m a

type ZipperFmap a = (Zipper a a -> a) -> a -> a

class ZipperRecursive a where
  zipperBottomUpTraverse :: ZipperTraverse m a
  zipperTopDownTraverse :: ZipperTraverse m a

instance (ZipperRecursive a) => Recursive a where
  bottomUpTraverse f = zipperBottomUpTraverse (f . hole)
  bottomUpTraverse :: (Monad m) => (a -> m a) -> a -> m a
  topDownTraverse :: (Monad m) => (a -> m a) -> a -> m a
  topDownTraverse f = zipperTopDownTraverse (f . hole)

zipperBottomUpFmap :: (ZipperRecursive a) => (Zipper a a -> a) -> a -> a
zipperBottomUpFmap f = Effs.run . zipperBottomUpTraverse (pure . f)

zipperTopDownFmap :: (ZipperRecursive a) => (Zipper a a -> a) -> a -> a
zipperTopDownFmap f = Effs.run . zipperTopDownTraverse (pure . f)
