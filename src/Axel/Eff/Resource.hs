{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Resource where

import Axel.Eff.FileSystem as FS (FileSystem, readFile)

import Control.Monad ((>=>))

import qualified Polysemy as Sem

import Paths_axel (getDataFileName)

import System.FilePath ((</>))

newtype ResourceId =
  ResourceId String

data Resource m a where
  GetResourcePath :: ResourceId -> Resource m FilePath

Sem.makeSem ''Resource

runResource ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Resource ': effs) a
  -> Sem.Sem effs a
runResource =
  Sem.interpret $ \case
    GetResourcePath (ResourceId resource) ->
      Sem.embed $ getDataFileName ("resources" </> resource)

readResource ::
     (Sem.Members '[ FileSystem, Resource] effs)
  => ResourceId
  -> Sem.Sem effs String
readResource = getResourcePath >=> FS.readFile

newProjectTemplate :: ResourceId
newProjectTemplate = ResourceId "new-project-template"
