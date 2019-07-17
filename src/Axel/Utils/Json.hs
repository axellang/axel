module Axel.Utils.Json where

import Control.Lens.Iso (iso)
import Control.Lens.Prism (Prism')

import Data.Aeson.Lens (AsNumber, _Number)

-- Adapted from file:///~/.stack/snapshots/x86_64-osx/c344f5612cdbdbeb3e3fadc9ca1373990ecaeb3e8b2bd6c69fe97455fc662979/8.6.1/doc/lens-aeson-1.0.2/src/Data.Aeson.Lens.html#_Integer.
-- TODO Use Hackage link instead of this (not tremendously useful) local one.
_Int :: (AsNumber t) => Prism' t Int
_Int = _Number . iso floor fromIntegral
