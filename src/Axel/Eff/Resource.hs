{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Resource where

import Axel.Prelude

import Axel.Eff.FileSystem as FS (FileSystem, readFile)
import Axel.Utils.FilePath ((</>))

import Control.Lens
import Control.Monad ((>=>))

import Data.Text.Lens (unpacked)

import qualified Polysemy as Sem

import qualified Paths_axel as Paths (getDataFileName)

newtype ResourceId =
  ResourceId Text

data Resource m a where
  GetResourcePath :: ResourceId -> Resource m FilePath

Sem.makeSem ''Resource

getDataFileName :: FilePath -> IO FilePath
getDataFileName = _Wrapping' FilePath (unpacked Paths.getDataFileName)

runResource ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Resource ': effs) a
  -> Sem.Sem effs a
runResource =
  Sem.interpret $ \case
    GetResourcePath (ResourceId resource) ->
      Sem.embed $ getDataFileName (FilePath "resources" </> FilePath resource)

readResource ::
     (Sem.Members '[ FileSystem, Resource] effs)
  => ResourceId
  -> Sem.Sem effs Text
readResource = getResourcePath >=> FS.readFile

newProjectTemplate :: ResourceId
newProjectTemplate = ResourceId "new-project-template"
