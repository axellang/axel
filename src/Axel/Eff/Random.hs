{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Random where

import Axel.Prelude

import qualified Polysemy as Sem

import qualified System.Random as R

data Random m a where
  Random :: (R.Random a) => Random m a

Sem.makeSem ''Random

runRandom ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Random ': effs) a
  -> Sem.Sem effs a
runRandom =
  Sem.interpret $ \case
    (Random :: Random m a) -> Sem.embed $ R.randomIO @a
