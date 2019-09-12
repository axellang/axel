{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Utils.Recursion where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Generics.Uniplate.Zipper (Zipper, hole)

exhaustM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
exhaustM f x = do
  result <- f x
  if x == result
    then pure result
    else exhaustM f result

-- TODO Remove dependencies on `Monad` in favor of `Applicative`
--      (which is all that `traverse` requires).
type Traverse m focus a
   = (Monad m) =>
       (focus -> m a) -> a -> m a

type Fmap focus a = (focus -> a) -> a -> a

mkFmapFromTraverse :: Traverse Identity focus a -> Fmap focus a
mkFmapFromTraverse traverseFn f = runIdentity . traverseFn (pure . f)

class Recursive a where
  bottomUpTraverse :: Traverse m a a
  topDownTraverse :: Traverse m a a

bottomUpFmap :: (Recursive a) => Fmap a a
bottomUpFmap = mkFmapFromTraverse bottomUpTraverse

topDownFmap :: (Recursive a) => Fmap a a
topDownFmap = mkFmapFromTraverse topDownTraverse

class ZipperRecursive a where
  zipperBottomUpTraverse :: Traverse m (Zipper a a) a
  zipperTopDownTraverse :: Traverse m (Zipper a a) a

instance (ZipperRecursive a) => Recursive a where
  bottomUpTraverse :: Traverse m a a
  bottomUpTraverse f = zipperBottomUpTraverse (f . hole)
  topDownTraverse :: Traverse m a a
  topDownTraverse f = zipperTopDownTraverse (f . hole)

zipperBottomUpFmap :: (ZipperRecursive a) => Fmap (Zipper a a) a
zipperBottomUpFmap = mkFmapFromTraverse zipperBottomUpTraverse

zipperTopDownFmap :: (ZipperRecursive a) => Fmap (Zipper a a) a
zipperTopDownFmap = mkFmapFromTraverse zipperTopDownTraverse
