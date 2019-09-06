{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- Inspired by http://www.haskellforall.com/2012/07/breaking-from-loop.html.
module Axel.Eff.Loop where

import Control.Monad (void)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

type Loop a = Sem.Error a

breakLoop ::
     forall a effs. (Sem.Member (Loop a) effs)
  => a
  -> Sem.Sem effs ()
breakLoop = void . Sem.throw

runLoop :: forall a effs. Sem.Sem (Loop a ': effs) a -> Sem.Sem effs a
runLoop x = either id id <$> Sem.runError x
