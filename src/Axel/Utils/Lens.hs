module Axel.Utils.Lens where

import Control.Lens.Prism (APrism, isn't)

is :: APrism s t a b -> s -> Bool
is k = not . isn't k
