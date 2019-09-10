{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff where

import qualified Polysemy as Sem

import System.IO.Unsafe (unsafePerformIO)

type Callback effs fn a
   = forall openEffs. (Sem.Members effs openEffs) =>
                        fn (Sem.Sem openEffs a)

unsafeEmbedIO :: Sem.Sem (Sem.Embed IO ': effs) a -> Sem.Sem effs a
unsafeEmbedIO = Sem.interpret $ \case
  Sem.Embed x -> pure $ unsafePerformIO x
