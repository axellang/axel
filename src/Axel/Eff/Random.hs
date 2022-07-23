{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Random where

import Axel.Prelude

import Effectful ((:>), Eff, Effect, IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)

import qualified System.Random as R

data Random :: Effect where
  Random :: (R.Random a) => Random m a

makeEffect ''Random

runRandom :: (IOE :> effs) => Eff (Random ': effs) a -> Eff effs a
runRandom =
  interpret $ \_ ->
    \case
      Random -> liftIO R.randomIO
