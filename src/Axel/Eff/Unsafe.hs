{- HLINT ignore "Avoid restricted function" -}
module Axel.Eff.Unsafe where

import Effectful (Eff, IOE)

import Unsafe.Coerce (unsafeCoerce)

unsafeEmbedIO :: Eff (IOE ': effs) a -> Eff effs a
unsafeEmbedIO = unsafeCoerce
