module Axel.Eff.Restartable where

import Axel.Prelude

import Effectful ((:>), Eff)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)

type Restartable = Error

restart :: (Restartable a :> effs) => a -> Eff effs ()
restart = throwError

runRestartable :: a -> (a -> Eff (Restartable a ': effs) a) -> Eff effs a
runRestartable x f =
  runErrorNoCallStack (f x) >>= \case
    Left x' -> runRestartable x' f
    Right x' -> pure x'
