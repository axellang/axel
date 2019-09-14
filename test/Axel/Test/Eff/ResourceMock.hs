{-# LANGUAGE GADTs #-}

module Axel.Test.Eff.ResourceMock where

import Axel.Prelude

import Axel.Eff.Resource as Effs
import Axel.Utils.FilePath

import qualified Polysemy as Sem

runResource :: Sem.Sem (Effs.Resource ': effs) a -> Sem.Sem effs a
runResource =
  Sem.interpret $ \case
    GetResourcePath (ResourceId resourceId) ->
      pure (FilePath "dataFiles/resources" </> FilePath resourceId)
