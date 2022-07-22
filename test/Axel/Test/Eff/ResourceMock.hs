module Axel.Test.Eff.ResourceMock where

import Axel.Prelude

import Axel.Eff.Resource as Effs
import Axel.Utils.FilePath

import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff

runResource :: Eff.Eff (Effs.Resource ': effs) a -> Eff.Eff effs a
runResource =
  Eff.interpret $ \_ ->
    \case
      GetResourcePath (ResourceId resourceId) ->
        pure (FilePath "dataFiles/resources" </> FilePath resourceId)
