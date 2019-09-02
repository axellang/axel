{-# LANGUAGE RankNTypes #-}

module Axel.Eff where

import Control.Monad.Freer (Eff, Members)

type Callback effs fn a
   = forall openEffs. (Members effs openEffs) =>
                        fn (Eff openEffs a)
