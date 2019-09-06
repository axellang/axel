{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Eff.ResourceMock where

import Axel.Eff.Resource as Effs

import qualified Polysemy as Sem

import System.FilePath

runResource ::
     forall effs a. Sem.Sem (Effs.Resource ': effs) a -> Sem.Sem effs a
runResource =
  Sem.interpret $ \case
    GetResourcePath (ResourceId resourceId) -> pure ("resources" </> resourceId)
