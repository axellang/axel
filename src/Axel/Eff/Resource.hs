{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Resource where

import Axel.Prelude

import Axel.Eff.FileSystem as FS (FileSystem, readFile)
import Axel.Utils.FilePath ((</>))

import Control.Lens
import Control.Monad ((>=>))

import Data.Text.Lens (unpacked)

import Effectful ((:>), (:>>))
import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.TH as Eff

import qualified Paths_axel as Paths (getDataFileName)

newtype ResourceId =
  ResourceId Text

data Resource :: Eff.Effect where
  GetResourcePath :: ResourceId -> Resource m FilePath

Eff.makeEffect ''Resource

getDataFileName :: FilePath -> IO FilePath
getDataFileName = _Wrapping' FilePath (unpacked Paths.getDataFileName)

runResource ::
     (Eff.IOE :> effs) => Eff.Eff (Resource ': effs) a -> Eff.Eff effs a
runResource =
  Eff.interpret $ \_ ->
    \case
      GetResourcePath (ResourceId resource) ->
        Eff.liftIO $
        getDataFileName (FilePath "resources" </> FilePath resource)

readResource ::
     ('[ FileSystem, Resource] :>> effs) => ResourceId -> Eff.Eff effs Text
readResource = getResourcePath >=> FS.readFile

newProjectTemplate :: ResourceId
newProjectTemplate = ResourceId "new-project-template"
