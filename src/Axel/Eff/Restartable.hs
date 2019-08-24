{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Restartable where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (runError, throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)

type Restartable a = Effs.Error a

restart :: (Member (Restartable a) effs) => a -> Eff effs ()
restart = throwError

runRestartable :: a -> (a -> Eff (Restartable a ': effs) a) -> Eff effs a
runRestartable x f =
  runError (f x) >>= \case
    Left x' -> runRestartable x' f
    Right x' -> pure x'
