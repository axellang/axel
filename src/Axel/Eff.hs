module Axel.Eff where

import Data.Kind

import Effectful ((:>), Eff)

-- This is being removed from effectful proper for performance reasons
-- (see https://github.com/haskell-effectful/effectful/issues/52#issuecomment-1269155485),
-- but it's not an issue for us (at least not for the moment).
-- Adapted from https://github.com/haskell-effectful/effectful/blob/0e312c77f49edb5f3d74cb70bf3fc0d9cd1816ff/effectful-core/src/Effectful/Internal/Effect.hs.
-- | Convenience operator for expressing that a function uses multiple effects
--   in a more concise way than enumerating them all with '(:>)'.
--
--   @[E1, E2, ..., En] ':>>' es ≡ (E1 ':>' es, E2 ':>' es, ..., En :> es)@
type family xs :>> es :: Constraint where
  '[]       :>> _es = ()
  (x ': xs) :>>  es = (x :> es, xs :>> es)

type Callback effs fn a
   = forall openEffs. (effs :>> openEffs) =>
                        fn (Eff openEffs a)

