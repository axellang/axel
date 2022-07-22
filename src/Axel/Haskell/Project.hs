{-# LANGUAGE OverloadedStrings #-}

module Axel.Haskell.Project where

import Axel.Prelude

import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.Error (Error)
import Axel.Eff.FileSystem
  ( copyFile
  , getCurrentDirectory
  , getDirectoryContentsRec
  , removeFile
  )
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.Ghci as Effs (Ghci)
import qualified Axel.Eff.Ghci as Ghci
import qualified Axel.Eff.Log as Effs (Log)
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Process (passthroughProcess)
import Axel.Eff.Resource (getResourcePath, newProjectTemplate)
import qualified Axel.Eff.Resource as Effs (Resource)
import qualified Axel.Haskell.Cabal as Cabal
  ( addDependency
  , axelPackageId
  , buildProject
  , createProject
  , runProject
  )
import Axel.Haskell.File
  ( convertFileInPlace
  , formatFileInPlace
  , readModuleInfo
  , transpileFileInPlace
  )
import Axel.Sourcemap (ModuleInfo)
import Axel.Utils.FilePath ((<.>), (</>))

import Control.Lens (op)
import Control.Monad (void)
import Control.Monad.Extra (concatMapM)

import qualified Data.Text as T

import Effectful ((:>), (:>>))
import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.State.Static.Local as Eff

type ProjectPath = FilePath

newProject ::
     '[ Effs.FileSystem, Effs.Process, Effs.Resource] :>> effs
  => Text
  -> Eff.Eff effs ()
newProject projectName = do
  Cabal.createProject projectName
  let projectPath = FilePath projectName
  Cabal.addDependency Cabal.axelPackageId projectPath
  templatePath <- getResourcePath newProjectTemplate
  let copyAxel filePath = do
        copyFile
          (templatePath </> filePath <.> "axel")
          (projectPath </> filePath <.> "axel")
        removeFile (projectPath </> filePath <.> "hs")
  mapM_
    copyAxel
    [ FilePath "Setup"
    , FilePath "app" </> FilePath "Main"
    , FilePath "src" </> FilePath "Lib"
    , FilePath "test" </> FilePath "Spec"
    ]

data ProjectFileType
  = Axel
  | Backend

getProjectFiles ::
     (Effs.FileSystem :> effs)
  => ProjectFileType
  -> Eff.Eff effs [FilePath]
getProjectFiles fileType = do
  files <-
    concatMapM
      getDirectoryContentsRec
      [FilePath "app", FilePath "src", FilePath "test"]
  let ext =
        case fileType of
          Axel -> ".axel"
          Backend -> ".hs"
  pure $ filter (\filePath -> ext `T.isSuffixOf` op FilePath filePath) files

transpileProject ::
     ('[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource] :>> effs)
  => Eff.Eff effs ModuleInfo
transpileProject =
  Ghci.withGhci $ do
    axelFiles <- getProjectFiles Axel
    initialModuleInfo <- readModuleInfo axelFiles
    moduleInfo <-
      Eff.execState initialModuleInfo $ mapM transpileFileInPlace axelFiles
    pure moduleInfo

buildProject ::
     ('[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource] :>> effs)
  => Eff.Eff effs ()
buildProject = do
  void $ passthroughProcess "hpack"
  projectPath <- getCurrentDirectory
  transpiledFiles <- transpileProject
  Cabal.buildProject transpiledFiles projectPath

convertProject ::
     ('[ Effs.Console, Effs.FileSystem, Eff.Error Error, Effs.FileSystem, Effs.Process] :>> effs)
  => Eff.Eff effs ()
convertProject = getProjectFiles Backend >>= void . traverse convertFileInPlace

runProject ::
     ('[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Process] :>> effs)
  => Eff.Eff effs ()
runProject = getCurrentDirectory >>= Cabal.runProject

formatProject ::
     ('[ Effs.Console, Effs.FileSystem, Eff.Error Error] :>> effs)
  => Eff.Eff effs ()
formatProject = getProjectFiles Axel >>= void . traverse formatFileInPlace
