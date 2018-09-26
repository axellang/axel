{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Monad.ResourceMock where

import Axel.Eff.Resource as Effs

import Control.Monad.Freer

import System.FilePath

runResource :: forall effs a. Eff (Effs.Resource ': effs) a -> Eff effs a
runResource =
  interpret $ \case
    GetResourcePath (ResourceId resourceId) -> pure ("resources" </> resourceId)
