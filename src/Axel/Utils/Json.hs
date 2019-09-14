module Axel.Utils.Json where

import Axel.Prelude

import Control.Lens.Iso (iso)
import Control.Lens.Prism (Prism')

import Data.Aeson.Lens (AsNumber, _Number)

-- Adapted from https://hackage.haskell.org/package/lens-aeson-1.0.2/docs/src/Data-Aeson-Lens.html#_Integer.
_Int :: (AsNumber t) => Prism' t Int
_Int = _Number . iso floor fromIntegral
