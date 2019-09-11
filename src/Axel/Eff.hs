{-# LANGUAGE RankNTypes #-}

module Axel.Eff where

import qualified Polysemy as Sem

type Callback effs fn a
   = forall openEffs. (Sem.Members effs openEffs) =>
                        fn (Sem.Sem openEffs a)
