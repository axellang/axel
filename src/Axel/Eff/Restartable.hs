{-# LANGUAGE GADTs #-}

module Axel.Eff.Restartable where

import Axel.Prelude

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

type Restartable = Sem.Error

restart :: (Sem.Member (Restartable a) effs) => a -> Sem.Sem effs ()
restart = Sem.throw

runRestartable ::
     a -> (a -> Sem.Sem (Restartable a ': effs) a) -> Sem.Sem effs a
runRestartable x f =
  Sem.runError (f x) >>= \case
    Left x' -> runRestartable x' f
    Right x' -> pure x'
