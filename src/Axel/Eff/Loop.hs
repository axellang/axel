{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- Inspired by http://www.haskellforall.com/2012/07/breaking-from-loop.html.
module Axel.Eff.Loop where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error, runError)

breakLoop :: (Member (Effs.Error a) effs) => a -> Eff effs ()
breakLoop = void . throwError

runLoop :: Eff (Effs.Error a ': effs) a -> Eff effs a
runLoop x = either id id <$> Effs.runError x
