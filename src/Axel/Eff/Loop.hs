{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- Inspired by http://www.haskellforall.com/2012/07/breaking-from-loop.html.
module Axel.Eff.Loop where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error, runError)

type Loop a = Effs.Error a

breakLoop ::
     forall a effs. (Member (Loop a) effs)
  => a
  -> Eff effs ()
breakLoop = void . throwError

runLoop :: forall a effs. Eff (Loop a ': effs) a -> Eff effs a
runLoop x = either id id <$> Effs.runError x
