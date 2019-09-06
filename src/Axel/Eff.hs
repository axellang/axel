{-# LANGUAGE RankNTypes #-}

module Axel.Eff where

import Polysemy (Members, Sem)

type Callback effs fn a
   = forall openEffs. (Members effs openEffs) =>
                        fn (Sem openEffs a)
