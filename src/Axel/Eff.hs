module Axel.Eff where

import Effectful ((:>>), Eff)

type Callback effs fn a
   = forall openEffs. (effs :>> openEffs) =>
                        fn (Eff openEffs a)
