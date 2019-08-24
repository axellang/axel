{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Axel.Haskell.Project where

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
import qualified Axel.Eff.Log as Effs (Log)
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Resource (getResourcePath, newProjectTemplate)
import qualified Axel.Eff.Resource as Effs (Resource)
import Axel.Haskell.File (readModuleInfo, transpileFileInPlace)
import Axel.Haskell.Stack
  ( addStackDependency
  , axelStackageId
  , buildStackProject
  , createStackProject
  , runStackProject
  )
import Axel.Macros (ModuleInfo)

import Control.Monad.Freer (Eff, Members)
import qualified Control.Monad.Freer.Error as Effs (Error)
import qualified Control.Monad.Freer.State as Effs (execState)

import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import System.FilePath ((</>))

type ProjectPath = FilePath

newProject ::
     Members '[ Effs.FileSystem, Effs.Process, Effs.Resource] effs
  => String
  -> Eff effs ()
newProject projectName = do
  createStackProject projectName
  addStackDependency axelStackageId projectName
  templatePath <- getResourcePath newProjectTemplate
  let copyAxel filePath = do
        copyFile
          (templatePath </> filePath <> ".axel")
          (projectName </> filePath <> ".axel")
        removeFile (projectName </> filePath <> ".hs")
  mapM_ copyAxel ["Setup", "app" </> "Main", "src" </> "Lib", "test" </> "Spec"]

transpileProject ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource] effs)
  => Eff effs ModuleInfo
transpileProject = do
  files <- concat <$> mapM getDirectoryContentsRec ["app", "src", "test"]
  let axelFiles =
        filter (\filePath -> ".axel" `T.isSuffixOf` T.pack filePath) files
  moduleInfo <- readModuleInfo axelFiles
  Effs.execState moduleInfo $ mapM transpileFileInPlace axelFiles

buildProject ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource] effs)
  => Eff effs ()
buildProject = do
  projectPath <- getCurrentDirectory
  transpiledFiles <- transpileProject
  buildStackProject transpiledFiles projectPath

runProject ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process] effs)
  => Eff effs ()
runProject = getCurrentDirectory >>= runStackProject
