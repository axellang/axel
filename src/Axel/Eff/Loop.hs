-- Inspired by http://www.haskellforall.com/2012/07/breaking-from-loop.html.
module Axel.Eff.Loop where

import Axel.Prelude

import Control.Monad (void)

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

type Loop a = Eff.Error a

breakLoop ::
     forall a effs. (Loop a :> effs)
  => a
  -> Eff.Eff effs ()
breakLoop = void . Eff.throwError

runLoop :: forall a effs. Eff.Eff (Loop a ': effs) a -> Eff.Eff effs a
runLoop x = either id id <$> Eff.runErrorNoCallStack x
