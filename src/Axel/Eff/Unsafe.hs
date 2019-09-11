{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Unsafe where

import qualified Polysemy as Sem

import System.IO.Unsafe (unsafePerformIO)

{-# ANN module "HLint: ignore Avoid restricted function" #-}

unsafeEmbedIO :: Sem.Sem (Sem.Embed IO ': effs) a -> Sem.Sem effs a
unsafeEmbedIO =
  Sem.interpret $ \case
    Sem.Embed x -> do
      let !result = unsafePerformIO x
      pure result
