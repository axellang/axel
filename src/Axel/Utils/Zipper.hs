module Axel.Utils.Zipper where

import Axel.Prelude

import Data.Generics.Uniplate.Operations (Uniplate)
import Data.Generics.Uniplate.Zipper (Zipper, down, left, right, up)

import Data.Maybe (fromJust)

unsafeDown :: (Uniplate a) => Zipper a a -> Zipper a a
unsafeDown = fromJust . down

unsafeLeft :: Zipper a a -> Zipper a a
unsafeLeft = fromJust . left

unsafeRight :: Zipper a a -> Zipper a a
unsafeRight = fromJust . right

unsafeUp :: Zipper a a -> Zipper a a
unsafeUp = fromJust . up
